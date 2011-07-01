/***************************************************************************
 *
 * fake_grant(Class, ResourceBit, Granted)
 *
 ***************************************************************************/
%
% always grant everyting for proclaimer's
% they supposed to use the alwayson audio group
%
fake_grant(proclaimer, ResourceBit, ResourceBit).



/***************************************************************************
 *
 * forbid_resource_owner_change(Resource, OwnerClass, AcquiringClass,
 *                              Mode, AudioGroup)
 *
 ***************************************************************************/
%
% only calls are mixed to navigator
%
forbid_resource_owner_change(audio_playback, navigator, Class, _, _) :-
    not(Class = call).

%
% suspend flash playback during calls
%
forbid_resource_owner_change(video_playback, _, _, _, flash) :-
  resource:resource_owner(audio_playback, call).

%
% suspend camera during calls
%
/*
forbid_resource_owner_change(video_recording, _, _, _, camera) :-
  resource:resource_owner(audio_playback, call).
*/

%
% disable all audio notifications if the camera is on
%
/*
forbid_resource_owner_change(audio_playback, _, Class, _, _) :-
    notification_resource_class(Class),
    resource:resource_owner(video_recording, camera),
    resource:resource_owner(audio_playback, camera).
*/

%
% disable all vibra notifications if the camera is on
%
forbid_resource_owner_change(vibra, _, Class, _, _) :-
    notification_resource_class(Class),
    resource:resource_owner(video_recording, camera).
