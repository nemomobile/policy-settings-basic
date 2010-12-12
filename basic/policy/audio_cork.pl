:- module(audio_cork, [set_corks/1]).


rules([set_corks/1]).

/*
 *
 */
cork_entry(OwnerClass, OwnerGroup, Entry) :-
    audio_type(sink, OwnerClass),
    cork(OwnerGroup, Group, Cork),
    Entry = [cork_stream, [group, Group], [cork, Cork]].


set_corks(CorkList) :-
    resource:resource_owner(audio_playback, Class, Group),
    findall(E, cork_entry(Class, Group, E), CorkList), !
      ;
    CorkList = [].
