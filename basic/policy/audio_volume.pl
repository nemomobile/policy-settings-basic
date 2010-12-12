:- module(audio_volume, [set_volume_limits/1]).


rules([set_volume_limits/1]).


/*
 *
 */
volume_limit_entry(OwnerClass, OwnerGroup, Entry) :-
    audio_type(sink, OwnerClass),
    volume_limit(OwnerGroup, Group, Limit),
    Entry = [volume_limit, [group, Group], [limit, Limit]].


set_volume_limits(LimitList) :-
    resource:resource_owner(audio_playback, Class, Group),
    findall(E, volume_limit_entry(Class, Group, E), LimitList), !
     ;
    LimitList = [].
