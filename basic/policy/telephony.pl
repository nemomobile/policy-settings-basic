:- module(telephony, [call_request/3, call_exists/2, call_domain/2]).


% Our predicates for the rule engine.
rules([call_request/3]).


% *
% * We export a predicates for telephony: call_request(Id, State, Actions)
% *
% * call_request is used to determine the policy decisions when call <Id>
% *   wants to change to <State>. It unifies Actions with a list of the form
% *   [[call_action, [A]]], where A is a list of pairs. Each pair is of the
% *   form [Id, IdAct], and describes what policy action should be taken for
% *   call <Id>. The possible actions are created, active, onhold, 
% *   disconnected.
% *
% * Notes: All the call action predicates except hold_call are unecessary and
% *        used only for improved readability.
% *


% *
% * exported predicates
% *

call_request(Id, State, [Actions]) :-
    call_actions(Id, State, CallActions),
    append([call_action], CallActions, Actions).



max_allowed_calls(2).      % maximum number of allowed calls


% *****************************************************************************
% *                         *** call request handling ***                     *
% *****************************************************************************

% *
% * Calls are always allowed to be put on hold.
% *

call_actions(Id, onhold, [Action]) :- hold_call(Id, Action), !.

% *
% * When disconnecting a call, check which end initiated the disconnection.
% * If the call was remotely terminated, take no extra actions. If the call
% * was terminated locally and it was not a conference call, reactivate the
% * last autoheld call if any.
% * 
% * tp-ring does not emit any MembersChanged[Detailed] signals if the call
% * was terminated locally. Hence, we consider the call locally terminated if
% * it is either in localhungup or active state. Remotely terminated calls are
% * always in <peerhungup> state.
% *

% * Administer which end hung up the call.
call_actions(Id, peerhungup, [Action]) :- peerhungup_call(Id, Action), !.
call_actions(Id, localhungup, [Action]) :- localhungup_call(Id, Action), !.

call_actions(Id, disconnected, Actions) :-
        (call_exists([id, state, connected], [Id, localhungup, yes])
         ;
         call_exists([id, state, connected], [Id, active, yes])),
	not(is_conference_parent(Id)),
        not((call_exists([id, state], [ActiveId, active]),
	     not(ActiveId == Id))),
	last_autoheld_call(LastAutoHeld),
	activate_call(LastAutoHeld, LastAutoHeldActions),
	disconnect_call(Id, DisconnectActions),
	append([DisconnectActions], [LastAutoHeldActions], Actions), !.

call_actions(Id, disconnected, [Action]) :- disconnect_call(Id, Action), !.


% *
% * When calling out, a new call can be created if none is alerting, we have
% * no active video call and we have few enough calls. In this case, also
% * hold any active calls.
% *

% * always allow a conference to be created
call_actions(Id, callout, [Actions]) :-
    is_conference_parent(Id),
    create_call(Id, Actions), !.

call_actions(Id, callout, [Actions]) :-
    has_active_video_call,
    disconnect_call(Id, Actions), !.

call_actions(Id, callout, Actions) :-
    not(has_alerting_call),
    number_of_calls(_, _, NumCalls),
    number_of_calls(_, unknown, NumUnknown),
    number_of_calls(_, conference, NumConf),
    max_allowed_calls(Max),
    Current is NumCalls - NumUnknown - NumConf,
    (Current >= Max *->
         (disconnect_call(Id, DisconnectActions),
	  Actions = [DisconnectActions])
       ;
         (autohold_active_calls(Id, AutoHoldActions),
          create_call(Id, CreateActions),
          append(AutoHoldActions, [CreateActions], Actions))),
    !.

call_actions(Id, callout, Actions) :-
    not(has_alerting_call),
    number_of_calls(Current), max_allowed_calls(Max), Current < Max,
    autohold_active_calls(Id, AutoHoldActions),
    create_call(Id, CreateActions),
    append(AutoHoldActions, [CreateActions], Actions), !.


% *
% * An (incoming) call can be created if none is alerting, and we have
% * few enough calls. Otherwise reject the call as busy.
% * 

call_actions(Id, created, [Action]) :-
    ((has_alerting_call ; 
      (number_of_calls(Current), max_allowed_calls(Max), Current > Max)) ->
         reject_call(Id, busy, Action)
     ;
         create_call(Id, Action)), !.


% *
% * A call can be activated without further actions if we have no active calls.
% *

call_actions(Id, active, [Action]) :- 
    not(has_active_call),
    activate_call(Id, Action), !.

% *
% * Otherwise the active call(s) must be either put on hold first or
% * disconnected depending on whether the new call is an emergency call.
% *

call_actions(Id, active, Actions) :-
    autohold_active_calls(Id, AutoHoldActions),
    activate_call(Id, ActivateActions),
    append(AutoHoldActions, [ActivateActions], Actions), !.


% *
% * All other call requests are rejected. Currently this results in
% * disconnecting the call.
% *

call_actions(Id, _, [Action]) :- reject_call(Id, Action), !.


% *
% * Generate a list of autohold actions for the currently active call(s).
% * When autoholding calls for a new cellular call, let the CMT autohold
% * existing active cellular calls (action cmtautohold).
% *

autohold_active_calls(Id, Actions) :-
    (is_cellular_call(Id) ->
     autohold_by_cellular(Actions)
     ;
     autohold_by_noncellular(Actions)).

autohold_by_noncellular(Actions) :-
    find_calls(active, ActiveCalls),
    maplist(autohold_call, ActiveCalls, Actions).

autohold_by_cellular(Actions) :-
    find_calls(active, ActiveCalls),
    autohold_by_cellular_(ActiveCalls, [], Actions).

autohold_by_cellular_([], Acc, Acc).
autohold_by_cellular_([H|T], Acc, Out) :-
    is_cellular_call(H),
    cmt_autohold_call(H, Action),
    autohold_by_cellular_(T, [Action|Acc], Out), !.
autohold_by_cellular_([H|T], Acc, Out) :-
    autohold_call(H, Action),
    autohold_by_cellular_(T, [Action|Acc], Out), !.



% *****************************************************************************
% *                *** supporting predicates for call handling ***            *
% *****************************************************************************

% find calls in a given state
find_calls(State, CallList) :-
    findall(Call,
            call_exists([id, state], [Call, State]),
            CallList).

% find calls of a given domain in a given state
find_calls(Domain, State, CallList) :-
    findall(Call,
	    call_in_state(Domain, Call, State),
	    CallList).

% number of calls in a given state
num_calls_in_state(State, N) :- find_calls(State, List), length(List, N).

% number of existing calls
number_of_calls(N) :-
    num_calls_in_state(created, Created),
    num_calls_in_state(callout, CallOut),
    num_calls_in_state(active, Active),
    num_calls_in_state(onhold, OnHold),
    num_calls_in_state(autohold, AutoHold),
    N is Created + CallOut + Active + OnHold + AutoHold.

number_of_calls(Domain, State, N) :-
    find_calls(Domain, State, List), length(List, N).

/*
number_of_calls(Domain, N) :-
    find_calls(Domain, created , CreateList),   length(List, NumCreated),
    find_calls(Domain, callout , CalloutList),  length(List, NumCallout),
    find_calls(Domain, active  , ActiveList),   length(List, NumActive),
    find_calls(Domain, onhold  , OnholdList),   length(List, NumOnhold),
    find_calls(Domain, autohold, AutoholdList), length(List, NumAutohold),
    N is NumCreate, NumCallout, NumActive, NumOnhold, NumAutohold.
*/

call_in_state(Domain, Call, State) :-
    call_exists([id, state], [Call, State]),
    call_domain(Call, Domain).



% *****************************************************************************
% *                    *** call classification predicates ***                 *
% *****************************************************************************

has_active_call :- call_exists([state], [active]);
                   call_exists([state], [conference]).

has_active_video_call :- call_exists([state, video], [active, yes]).

has_held_call :- (call_exists([state], [onhold])
                    ;
                  call_exists([state], [autohold])).

has_outgoing_call :- call_exists([state, direction], [created, outgoing]).

has_peerhungup_call :- 
    call_exists([state, direction, connected], [peerhungup, incoming, yes]);
    call_exists([state, direction], [peerhungup, outgoing]).

has_localhungup_call :- call_exists([state], [localhungup]).

has_alerting_call :- call_exists([state, direction], [created, incoming]).

has_conference_call :- call_exists([state], [conference]).

has_early_emergency_call :-
    fact_exists('com.nokia.policy.emergency_call', [state], [active]).

% *****************************************************************************
% *               *** call domain classification predicates ***               *
% *****************************************************************************

call_prefix(cellular, '/org/freedesktop/Telepathy/Connection/ring/tel/ring/').
call_prefix(skype   , '/org/freedesktop/Telepathy/Connection/spirit/skype/').
call_prefix(gtalk   , '/org/freedesktop/Telepathy/Connection/gabble/jabber/').

call_domain(Id, Domain) :-
    call_exists([id, path], [Id, Path]),
    call_prefix(Domain, Prefix),
    sub_atom(Path, 0, _, _, Prefix).

is_conference_parent(Id) :- call_exists([id, parent], [Id, Id]).

/*
is_emergency_call(Id) :-
    call_exists([id, emergency], [Id, yes]).
*/

is_cellular_call(Id) :-
    call_exists([id, path], [Id, Path]),
    is_cellular_path(Path).

/*
is_ip_call(Id) :-
    call_exists([id, path], [Id, Path]),
    not(is_cellular_path(Path)).
*/

is_cellular_path(P) :-
    sub_string(P,
               0, 52, _,
               '/org/freedesktop/Telepathy/Connection/ring/tel/ring/').

/*
has_active_cellular_call :-
    call_exists([state, path], [active, Path]),
    is_cellular_path(Path).

has_active_ip_call :-
    call_exists([state, path], [active, Path]),
    not(is_cellular_path(Path)).
*/

nonholdable_call(Id) :-
    call_exists([id, holdable], [Id, no]).


% *****************************************************************************
% *                          *** basic call actions ***                       *
% *****************************************************************************

% create a call
create_call(Id, [Id, created]).

% hold a call
hold_call(Id, [Id, onhold]).

% *
% * Autoholding is the action of automatically holding a call to make
% * room for another call to be activated (ie. accepted). Some calls
% * cannot be held at all. Instead of trying to hold such calls we
% * disconnect them when autoholding would be necessary.
% autohold_call(Id, [Id, autohold]).

autohold_call(Id, Action) :- autohold_or_disconnect(Id, Action).

autohold_or_disconnect(Id, Action) :-
    ((nonholdable_call(Id), disconnect_call(Id, Action))
      ;
    autohold_action(Id, Action)), !.
    
autohold_action(Id, [Id, autohold]).

% cmtautohold is autohold done by the CMT (for CS/CS conflicts).
cmt_autohold_call(Id, [Id, cmtautohold]).

% adminster remotely initiated disconnection of a call
peerhungup_call(Id, [Id, peerhungup]).

% administer locally initiated disconnection of a call
localhungup_call(Id, [Id, localhungup]).

% disconnect a call
disconnect_call(Id, [Id, disconnected]).

% reject a call
reject_call(Id, [Id, disconnected]).

% reject a call with a given reason
reject_call(Id, Reason, [Id, Reason]).

% activate a call
activate_call(Id, [Id, active]).


% *****************************************************************************
% *                         *** supporting predicates ***                     *
% *****************************************************************************

% predicate to look up calls
call_exists(Keys, Values) :-
    fact_exists('com.nokia.policy.call', Keys, Values).

% find last call that was held to activate another call
last_autoheld_call(Id) :-
    autohold_orders(List),
    max_elem(List, N),
    call_exists([id, state, order], [Id, autohold, N]).


% find (orders) of all autoheld calls
autohold_orders(List) :- findall(N, call_exists([order], [N]), List).

% find the largest element of a list
max_elem([H|T], Max) :- max_elem_(T, H, Max).
max_elem_([], Max, Max).
max_elem_([H|T], Acc, Max) :- compare(>, H, Acc), max_elem_(T, H, Max), !.
max_elem_([_|T], Acc, Max) :- max_elem_(T, Acc, Max).

