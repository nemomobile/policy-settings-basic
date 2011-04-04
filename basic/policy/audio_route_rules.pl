/*
 *
 */
resource_class_privacy(proclaimer , public).
resource_class_privacy(navigator  , public).
resource_class_privacy(call       , public).
resource_class_privacy(videoeditor, public).
resource_class_privacy(camera     , public).
resource_class_privacy(ringtone   , public).
resource_class_privacy(alarm      , public).
resource_class_privacy(game       , public).
resource_class_privacy(player     , public).
resource_class_privacy(implicit   , public).
resource_class_privacy(event      , public).
resource_class_privacy(background , public).
resource_class_privacy(nobody     , public).

/*
 * implicated privacies
 */
%
% active video calls implicate 'public' privacy
%
implicated_privacy(public) :-
    resource:resource_owner(audio_playback, call),
    resource:resource_owner(video_playback, call).


/*
 * Here is a bunch of exception for audio routing
 */
%
% never route voice call output to public devices
% explicitly demands it via privacy override
%
invalid_audio_device_choice(call, sink, Device) :-
    (implicated_privacy(public) *-> Privacy=private ; Privacy=public),
    audio_device_privacy(Privacy, Device).


%
% if a call were active during navigation the call should determine the route
%
invalid_audio_device_choice(navigator, sink, Device) :-
    resource:granted_resource(call, audio_playback),
    invalid_audio_device_choice(call, sink, Device).

%
% do not route video call to earpiece or hac
%
invalid_audio_device_choice(call, sink, earpiece) :-
    resource:resource_owner(video_playback, call).

invalid_audio_device_choice(call, sink, earpieceandtvout) :-
    resource:resource_owner(video_playback, call).

invalid_audio_device_choice(call, sink, hac) :-
    resource:resource_owner(video_playback, call).

invalid_audio_device_choice(call, sink, hacandtvout) :-
    resource:resource_owner(video_playback, call).


%
% Do not route ringtones to private accessories
%
invalid_audio_device_choice(ringtone, sink, Device) :-
    audio_accessory(Device),
    audio_device_privacy(private, Device),
    audio_device_type(sink, Device).


%
% do not route anything to earpiece or hac if we had no active call 
%
invalid_audio_device_choice(_, sink, earpiece) :-
    not(resource:granted_resource(call, audio_playback)).

invalid_audio_device_choice(_, sink, earpieceandtvout) :-
    not(resource:granted_resource(call, audio_playback)).

invalid_audio_device_choice(_, sink, hac) :-
    not(resource:granted_resource(call, audio_playback)).

invalid_audio_device_choice(_, sink, hacandtvout) :-
    not(resource:granted_resource(call, audio_playback)).


%
% route only ringtone to twin device like ihfandheadset
%
invalid_audio_device_choice(Class, sink, TwinDevice) :-
    twin_audio_device(TwinDevice),
    not(notification_class(Class)).

%
% do not route cscall or ipcall to bta2dp
%
invalid_audio_device_choice(call, _, bta2dp).
invalid_audio_device_choice(call, _, tvoutandbta2dp).

%
% never route anyting to headmike
%
invalid_audio_device_choice(_, source, headmike).

%
% route the source to accessories that have mike as well if the
% sink is also routed there
% 
invalid_audio_device_choice(_, source, bthsp) :-
    not(audio_route:get_route(sink, bthsp)).
invalid_audio_device_choice(_, source, tvoutandbthsp) :-
    not(audio_route:get_route(sink, tvoutandbthsp)).

invalid_audio_device_choice(_, source, headset) :-
    not(audio_route:get_route(sink, headset)).
