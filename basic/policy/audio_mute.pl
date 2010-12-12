:- module(audio_mute, [set_mute/1]).

rules([set_mute/1]).

/*
 * Mute
 */
mute_entry(Mute, Entry) :-
    audio_device_type(source, Device),
    Entry = [audio_mute, [device, Device], [mute, Mute]].

set_mute(MuteList) :-
    mute(Mute), 
    findall(E, mute_entry(Mute, E), MuteList), !
      ;
    MuteList = [].

/*
 * FactStore
 */
mute(M) :-
    fact_exists('com.nokia.policy.mute',
		[value], [X]),
    (not(X=0) *-> M=muted ; M=unmuted), !
    ;
    M=unmuted.


