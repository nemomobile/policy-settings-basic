:- module(resource,
	  [update_resource_entries/1, update_resource_owner_entries/1,
	   resource_owner/2, resource_owner/3, resource_group/2,
 	   granted_resource/2, granted_resource/3, active_resource/3,
 	   force_resource_release/3]).

rules([update_resource_entries/1, update_resource_owner_entries/1,
       force_resource_release/3]).


/*
 * These are the mode bits (should they be here or in resource_classes.pl or ?)
 */
resource_mode_bit(auto_release, 1).

/*
 * override_bits
 */
override_bits(Bits, Override, OverrideMask, Result) :-
    KeepMask is 4294967295 xor OverrideMask,
    UnchangedBits is Bits /\ KeepMask,
    OverrideBits  is Override /\ OverrideMask,
    Result is UnchangedBits \/ OverrideBits.

/*****************************************************************************
 *
 * reset_flags
 * set_flags(GrantedBit, AdviceBit)
 * get_flags(GrantedFlag, AdviceFlag)
 *
 *****************************************************************************/
reset_flags :-
    nb_setval('_resource_flags', [0,0]).

set_flags(GrantedBit, AdviceBit) :-
    nb_getval('_resource_flags', [OldGrantedFlag, OldAdviceFlag]),
    NewGrantedFlag is OldGrantedFlag \/ GrantedBit,
    NewAdviceFlag  is OldAdviceFlag  \/ AdviceBit,
    nb_setval('_resource_flags', [NewGrantedFlag, NewAdviceFlag]).

get_flags(GrantedFlag, AdviceFlag) :-
    nb_getval('_resource_flags', [GrantedFlag, AdviceFlag]). 


/*****************************************************************************
 *
 * resource(ResourceFlags, SharedFlags, Resource, ResourceBit, Mode)
 *
 *****************************************************************************/
resource(ResourceFlags, SharedFlags, Resource, ResourceBit, Mode) :-
    integer(ResourceFlags),
    integer(SharedFlags),
    resource_bit(Resource, ResourceBit),
    RBit is ResourceFlags /\ ResourceBit,
    SBit is SharedFlags /\ ResourceBit,
    RBit = ResourceBit,
    (SBit = ResourceBit *->  Mode=shared ; Mode=exclusive).

resource(ResourceFlags, Resource) :-
    integer(ResourceFlags),
    resource_bit(Resource, ResourceBit),
    not(ResourceBit = 0),
    RBit is ResourceFlags /\ ResourceBit,
    RBit = ResourceBit.

/*****************************************************************************
 *
 * reset_resource_owners
 * set_resource_owner(Resource, Class, Mode, Group)
 * get_resource_owner(Resource, Class, Mode, Group)
 * rollback_resource_owner(Class)
 *
 *
 *****************************************************************************/
varname_for_resource_owner(Resource, Varname) :-
    resource(Resource),
    atom_concat('_resource_owner_for_', Resource, Varname).

reset_resource_owners :-
    forall(resource(Resource),
	   set_resource_owner(Resource, nobody, shared, idle)
    ).

set_resource_owner(Resource, Class, Mode, Group) :-
    varname_for_resource_owner(Resource, Varname),
    nb_setval(Varname, [Class, Mode, Group]).

get_resource_owner(Resource, Class, Mode, Group) :-
    varname_for_resource_owner(Resource, Varname),
    nb_getval(Varname, [Class, Mode, Group]).

unset_resource_owner(Resource, Class) :-
    varname_for_resource_owner(Resource, Varname),
    nb_getval(Varname, [Class, _, _]),
    not(get_previous_owner(Resource, Class)),
    nb_setval(Varname, [nobody, shared, idle]), !
    ;
    true.

rollback_resource_owner(Class) :-
    forall(resource(Resource), unset_resource_owner(Resource, Class)).


/*****************************************************************************
 *
 * copy_resource_owners_to_previous_owners
 * set_previous_owner_set(ResourceFlags, Class)
 * get_previous_owner(Resource, Class)
 *
 *
 *****************************************************************************/
varname_for_previous_owner(Resource, Varname) :-
    resource(Resource),
    atom_concat('_previous_owner_for_', Resource, Varname).

set_previous_owner(Resource, Class) :-
    varname_for_previous_owner(Resource, Varname),
    nb_setval(Varname, Class).

copy_resource_owners_to_previous_owners :-
    forall(get_resource_owner(Resource, Class, _, _),
	   set_previous_owner(Resource, Class)
    ).

set_previous_owner_set(ResourceFlags, Class) :-
    integer(ResourceFlags),
    forall(resource(ResourceFlags, Resource),
	   set_previous_owner(Resource, Class)
    ), !
      ;
    true.

get_previous_owner(Resource, Class) :-
    varname_for_previous_owner(Resource, Varname),
    nb_getval(Varname, Class).


/*****************************************************************************
 *
 * These are to produce the bit fields for the sorting key of
 * resource_set.
 *
 * The top priority is key value 0. Increasing key values represent
 * gradually decreasing priorities.
 *
 *****************************************************************************/
class_key_bits(CK, Bits) :-            Bits = CK * 67108864.        % Bit 26-31
audio_key_bits(AK, Bits) :-            Bits is (AK /\ 7) * 8388608. % Bit 25-23

%
% exlusive users have priority over shared ones
%
shared_key_bits(0, Bits) :-            Bits = 0.
shared_key_bits(S, Bits) :- not(S=0),  Bits = 4194304.              % Bit 22

%
% release has priority over acquire
%
request_key_bits(release, Bits) :-     Bits = 0.
request_key_bits(acquire, Bits) :-     Bits = 2097152.              % Bit 21

                                                                   % Bit 0-20
reqno_key_bits(ReqNo, Bits) :-         Bits is 2097151 - (ReqNo /\ 2097151).


/*****************************************************************************
 *
 *
 *
 *****************************************************************************/
audio_group_priority(ResourceClass, AudioGroup, Priority) :-
    audio_group_relative_class_priority(ResourceClass, AudioGroup, Priority), !
      ;
    Priority = 0.

resource_set_key(ClassPriority, AudioPriority, Shared, Request, ReqNo, Key) :-
    class_key_bits(ClassPriority, ClassBits),
    audio_key_bits(AudioPriority, AudioBits),
    shared_key_bits(Shared, SharedBits),
    request_key_bits(Request, RequestBits),
    reqno_key_bits(ReqNo, ReqnoBits),
    Key is ClassBits \/ AudioBits \/ SharedBits \/ RequestBits \/ ReqnoBits.

resource_set_keyval(KeyVal) :-
    resource_set(ManagerId, Class, Mode,
		 Mandatory, Optional, ShReq,Mask,
		 Request, ReqNo, AudioGroup),
    resource_class_priority(Class, ClassPriority),
    audio_group_priority(Class, AudioGroup, AudioPriority),
    resource_class_sharing(Class, ShClass),
    override_bits(ShClass, ShReq, Mask, Shared),
    resource_set_key(ClassPriority,AudioPriority, Shared, Request, ReqNo, Key),
    KeyVal = Key-[ManagerId, Class, Mode, Mandatory,Optional,Shared, Request,
		  AudioGroup].


sorted_resource_set(ManagerId, Class, Mode, Mandatory,Optional,Shared,
		    Request, AudioGroup) :-
    findall(KeyVal, resource_set_keyval(KeyVal), Unsorted),
    keysort(Unsorted, Sorted),
    member(_-[ManagerId, Class, Mode, Mandatory,Optional,Shared, Request, 
	      AudioGroup], Sorted).


/*
 *
 */
class_to_reject(implicit).
class_to_reject(nobody).

/*****************************************************************************
 *
 * advice(ResourceBit, Mode, Class, OwnerClass, OwnerMode, AudioGroup, Advice)
 *
 *****************************************************************************/
%
% if 'nobody' owns its free; consequently advice it to use
%
advice_rule(ResourceBit, _, _, nobody, _, _, ResourceBit).

%
% if it is currently shared advice to use it
%
advice_rule(ResourceBit, _, _, _, shared, _, ResourceBit).

%
% if requestor and the owner is in the same class advice to use it
% in case the relative audio group priority is high enough
%
%
advice_rule(ResourceBit, _, Class, Class, _, Group, ResourceBit) :-
    resource_bit(Resource, ResourceBit),
    get_resource_owner(Resource, Class, _, OwnerGroup),
    audio_group_priority(Class, Group, Priority),
    audio_group_priority(Class, OwnerGroup, OwnerPriority),
    Priority =< OwnerPriority.

%
% fallback case; advice not to use it
%
advice_rule(_, _, _, _, _, _, 0).


advice(ResourceBit, Mode, Class, OwnerClass, OwnerMode, AuGr, Advice) :-
    advice_rule(ResourceBit, Mode,Class,OwnerClass,OwnerMode,AuGr, Advice), !.


/*****************************************************************************
 *
 * acquire(Resource, OwnerClass, Class, Mode, AudioGroup)
 *
 *****************************************************************************/
change_owner(audio_playback, nobody, Class, Mode, AudioGroup) :-
    set_resource_owner(audio_playback, Class, Mode, AudioGroup).
change_owner(audio_recording, nobody, Class, Mode, AudioGroup) :-
    set_resource_owner(audio_recording, Class, Mode, AudioGroup).
change_owner(Resource, nobody, Class, Mode, _) :-
    set_resource_owner(Resource, Class, Mode, Class).
change_owner(_, _, _, _, _).

acquire(Resource, OwnerClass, Class, Mode, AudioGroup) :-
    change_owner(Resource, OwnerClass, Class, Mode, AudioGroup), !.

/*****************************************************************************
 *
 * The final_flags checks wheter the proposed granted and adviced flags would
 * contain all the mandatory flags. If so the final flags are the proposed.
 * Otherwise the final flags are 0.
 * 
 *   final_flags(Request, Mandatory, GrantProposed,AdviceProposed,
 *		 Granted,Advice)
 *
 *****************************************************************************/
copy_if_mandatory_is_set(Proposed, Mandatory, Copied) :-
    ProposedMandatory is Proposed /\ Mandatory,
    (ProposedMandatory = Mandatory *-> Copied=Proposed ; Copied=0).

final_flags(acquire, Mandatory, GrantProposed,AdviceProposed, Granted,Advice):-
    copy_if_mandatory_is_set(GrantProposed, Mandatory, Granted),
    copy_if_mandatory_is_set(AdviceProposed, Mandatory, Advice).

%
% for released resource sets the final grant is always 0.
%
final_flags(release, Mandatory, _,AdviceProposed, 0,Advice) :-
    copy_if_mandatory_is_set(AdviceProposed, Mandatory, Advice).

/*
 * auto_release(CurrentRequest, Mode, Granted, NewRequest)
 */
auto_release(CurrentRequest, _, Grant, CurrentRequest) :-
    not(Grant = 0).

auto_release(CurrentRequest, Mode, Grant, NewRequest) :-
    Grant = 0,
    resource_mode_bit(auto_release, AutoReleaseBit),
    AutoReleaseMode is Mode /\ AutoReleaseBit,
    (AutoReleaseMode = 0 *-> NewRequest=CurrentRequest ; NewRequest=release).

/*
 *
 */
grant(acquire, All,Shared, Class, AuGr, Granted, Advice) :-
    resource(All, Shared, Resource, ResourceBit, Mode),
    (fake_grant(Class, ResourceBit, Granted) *->
          Advice=Granted
      ;
          (get_resource_owner(Resource, CurrentOwner, CurrentMode, _),
           (forbid_resource_owner_change(Resource, CurrentOwner,Class,Mode,
					 AuGr) *->
                 (Granted=0, Advice=0)
            ;
                 (advice(ResourceBit,Mode,Class,CurrentOwner,CurrentMode,AuGr,
			 Advice),
	          acquire(Resource, CurrentOwner, Class, Mode, AuGr),
	          (CurrentMode = shared *-> Granted=ResourceBit ; Granted=0)
                 )
	   )
          )
    ).

grant(release, All,Shared, Class, AuGr, 0, Advice) :-
    resource(All, Shared, Resource, ResourceBit, Mode),
    get_resource_owner(Resource, CurrentOwner, CurrentMode, _),
    advice(ResourceBit, Mode, Class, CurrentOwner, CurrentMode, AuGr, Advice).

grant_resource_set(Request, Class, Mandatory,Optional,Shared, AudioGroup,
		   Granted, Advice):-
    All is Mandatory \/ Optional,
    reset_flags,
    forall(grant(Request, All,Shared, Class, AudioGroup, GrantBit,AdviceBit),
	   set_flags(GrantBit, AdviceBit)
    ),
    get_flags(GrantPropose, AdvicePropose),
    final_flags(Request,Mandatory, GrantPropose,AdvicePropose, Granted,Advice),
    (Granted=0, not(GrantPropose=0), rollback_resource_owner(Class), !
       ;
    set_previous_owner_set(Granted, Class)).

resource_entry(Entry) :-
    reset_resource_owners,
    copy_resource_owners_to_previous_owners,
    sorted_resource_set(ManagerId, Class, Mode, Mandatory,Optional,Shared,
			CurReq, AudioGroup),
    grant_resource_set(CurReq, Class, Mandatory,Optional,Shared, AudioGroup,
		       Granted, Advice),
    auto_release(CurReq, Mode, Granted, Request),
    Entry = [resource_set, [manager_id, ManagerId],
	                   [granted   , Granted  ],
	                   [advice    , Advice   ],
	                   [request   , Request  ]
    ].

update_resource_entries(List) :-
    findall(E, resource_entry(E), List), !
    ;
    List = [].

/*
 *
 */
block_entry(Class, AudioGroup, Entry) :-
    resource_set_with_active_audio(ManagerId, Class, AudioGroup),
    Entry = [resource_set, [manager_id, ManagerId], [block, 1]].

force_resource_release(Class, AudioGroup, List) :-
    findall(E, block_entry(Class, AudioGroup, E), List), !
    ;
    List = [].

/*
 *
 */
resource_owner_entry(Entry) :-
    resource(Resource),
    get_resource_owner(Resource, Owner, Mode, Group),
    Entry = [resource_owner, [resource, Resource], 
	                     [owner   , Owner   ],
	                     [mode    , Mode    ],
	                     [group   , Group   ]
    ].

update_resource_owner_entries(List) :-
    findall(E, resource_owner_entry(E), List), !
    ;
    List = [].


/*
 * factsore interface
 */
resource_set(ManagerId, Class, Mode,
	     Mandatory, Optional, Shared,Mask,
	     Request, ReqNo, AudioGroup) :-
    fact_exists('com.nokia.policy.resource_set',
     [manager_id, mode, class, mandatory,optional,shared,mask, request, reqno,
      audiogr   ],
     [ManagerId , Mode, Class, Mandatory,Optional,Shared,Mask, Request, ReqNo,
      AudioGroup]
    ),
    resource_class(Class).

resource_set_with_active_audio(ManagerId, Class, AudioGroup) :-
    fact_exists('com.nokia.policy.resource_set',
		[manager_id, class, request, block, granted, audiogr   ],
		[ManagerId , Class, acquire, 0    , Granted, AudioGroup]),
    resource_class(Class),
    resource_bit(audio_playback, ResourceBit),
    GrantedBit is Granted /\ ResourceBit,
    GrantedBit = ResourceBit.

resource_owner(Resource, Class) :-
    get_resource_owner(Resource, Class, _, _), !
    ;
    Class = nobody.

resource_owner(Resource, Class, Group) :-
    get_resource_owner(Resource, Class, _, Group), !
    ;
    Class = nobody,
    Group = idle.

resource_group(Resource, Group) :-
    get_resource_owner(Resource, _, _, Group), !
    ;
    Group = idle.

granted_resource(Class, Resource) :-
    fact_exists('com.nokia.policy.resource_set',
		[class, granted], [Class, Granted]),
    resource_bit(Resource, ResourceBit),
    GrantedBit is Granted /\ ResourceBit,
    GrantedBit = ResourceBit.

granted_resource(Class, Group, Resource) :-
    fact_exists('com.nokia.policy.resource_set',
		[class, audiogr, granted], [Class, Group, Granted]),
    resource_bit(Resource, ResourceBit),
    GrantedBit is Granted /\ ResourceBit,
    GrantedBit = ResourceBit.

active_resource(Class, Group, Resource) :-
    fact_exists('com.nokia.policy.resource_set',
		[class, audiogr, granted, block],
		[Class, Group  , Granted, 0    ]),
    resource_bit(Resource, ResourceBit),
    GrantedBit is Granted /\ ResourceBit,
    GrantedBit = ResourceBit,
    !.
