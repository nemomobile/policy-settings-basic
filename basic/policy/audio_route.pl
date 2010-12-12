:- module(audio_route, [set_audio_routes/1, set_audio_privacy/2, get_route/2]).

rules([set_audio_routes/1, set_audio_privacy/2]).

/*
 * Global Values
 */
set_route(sink  , Device) :- nb_setval('sink_route'  , Device).
set_route(source, Device) :- nb_setval('source_route', Device).
get_route(sink  , Device) :- nb_getval('sink_route'  , Device).
get_route(source, Device) :- nb_getval('source_route', Device).


/*
 *
 */
privacy_map(private, private).
privacy_map(public , private).
privacy_map(public , public ).

privacy(Class, Privacy) :-
    (implicated_privacy(Privacy), !)
    ;
    (resource_class_privacy(Class, P), privacy_map(P, Privacy)).

privacy_match(Privacy, Device) :-
    privacy_map(Privacy, ConsideredPrivacy),
    audio_device_privacy(ConsideredPrivacy, Device).

current_audio_class(Class) :-
    resource:resource_owner(audio_playback, Class), !
    ;
    Class = nobody.


device_candidate(Class, Type, Device) :-
    privacy(Class, Privacy),
    audio_device_type(Type, Device),
    audio_device_privacy(Privacy, Device),
    accessory:selectable_audio(Device).

route_to_device(Type, Device) :-
    current_audio_class(Class),
    device_candidate(Class, Type, Device),
    not(invalid_audio_device_choice(Class, Type, Device)), !
    ;
    Type = sink,
    Device = null.

route_entry(Type, Device, Mode, HwID, E) :-
    E = [audio_route, [type,Type], [device,Device], [mode,Mode], [hwid,HwID]].

route(Route) :-
    audio_device_type(DeviceType),
    route_to_device(DeviceType, Device),
    set_route(DeviceType, Device),
    audio_configuration(DeviceType, Device, Mode, HwID),
    route_entry(DeviceType, Device, Mode, HwID, Route).

set_audio_routes(RouteList) :-
    set_route(_, undefined),
    findall(R, route(R), RouteList).

/*
 *
 */
set_audio_privacy(Stage, PrivacyList) :-
    current_route(sink, Device),
    audio_device_privacy(Privacy, Device),
    PrivacyList = [[privacy, [stage, Stage], [value, Privacy]]], !
      ;
    PrivacyList = [].

/*
 * FactStore interface
 */
audio_configuration(source, _, na, na).
audio_configuration(sink, Device, Mode, HwID) :-
    fact_exists('com.nokia.policy.audio_output_configuration',
		[device, mode, hwid], [Device, Mode, HwID]).

current_route(DeviceType, Where) :-
    fact_exists('com.nokia.policy.audio_route',
		[type, device], [DeviceType, Where]).

