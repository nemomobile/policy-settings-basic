/****************************************************************************
 *
 * resources
 *
 ****************************************************************************/
resource(audio_playback).
resource(video_playback).
resource(audio_recording).
resource(video_recording).
resource(vibra).
resource(leds).
resource(backlight).
resource(headset_buttons).
resource(system_button).
resource(lock_button).
resource(scale_button).
resource(snap_button).
resource(lens_cover).

%               resource        decimal mask      bit  hexa mask
%            -----------------------------------------------------
resource_bit( '<no-resource>' ,           0 ).  %   -  0x00000000
resource_bit( audio_playback  ,           1 ).  %   0  0x00000001
resource_bit( video_playback  ,           2 ).  %   1  0x00000002
resource_bit( audio_recording ,           4 ).  %   2  0x00000004
resource_bit( video_recording ,           8 ).  %   3  0x00000008
resource_bit( vibra           ,          16 ).  %   4  0x00000010
resource_bit( leds            ,          32 ).  %   5  0x00000020
resource_bit( backlight       ,          64 ).  %   6  0x00000040
resource_bit( system_button   ,         256 ).  %   8  0x00000100
resource_bit( lock_button     ,         512 ).  %   9  0x00000200
resource_bit( scale_button    ,        1024 ).  %  10  0x00000400
resource_bit( snap_button     ,        2048 ).  %  11  0x00000800
resource_bit( lens_cover      ,        4096 ).  %  12  0x00001000
resource_bit( headset_buttons ,        8192 ).  %  13  0x00002000

/****************************************************************************
 *
 * resource classes
 *
 *****************************************************************************/
resource_class(proclaimer).  % always audible announcements
resource_class(navigator).   % navigator application
resource_class(call).        % Telephony voice
resource_class(videoeditor). % Foreground video editor/encoder (used by MMS)
resource_class(camera).      % camera applications
resource_class(ringtone).    % Telephony ringtone
resource_class(alarm).       % wakeup, calendar and other alarms
resource_class(game).        %
resource_class(player).      % Madia players, browser embedded flash, fmradio
resource_class(implicit).    % Everything else (i.e. default class)
resource_class(event).       % Messages (SMS, Chat etc), network events etc
resource_class(background).  % UI and sound-less rendering
resource_class(nobody).      % Lowest priority class


/****************************************************************************
 *
 * resource class priorities
 *
 *****************************************************************************/
resource_class_priority( proclaimer ,  0 ).
resource_class_priority( navigator  ,  1 ).
resource_class_priority( call       ,  2 ).
resource_class_priority( videoeditor,  3 ).
resource_class_priority( camera     ,  4 ).
resource_class_priority( ringtone   ,  5 ).
resource_class_priority( alarm      ,  6 ).
resource_class_priority( game       ,  7 ).
resource_class_priority( player     ,  8 ).
resource_class_priority( implicit   ,  9 ).
resource_class_priority( event      , 10 ).
resource_class_priority( background , 11 ).
resource_class_priority( nobody     , 12 ).

/****************************************************************************
 *
 * default sharing rules for resource classes
 *
 *****************************************************************************/
%                                    Bitwise \/ of the
%                       Class      shared resource bits
%                     --------------------------------------
resource_class_sharing( proclaimer  ,          1   ).   % AudioPlayback
resource_class_sharing( navigator   ,          1   ).   % AudioPlayback
resource_class_sharing( call        ,          0   ).   % none
resource_class_sharing( videoeditor ,          0   ).   % none
resource_class_sharing( camera      ,          0   ).   % none
resource_class_sharing( ringtone    ,          0   ).   % none
resource_class_sharing( alarm       ,          0   ).   % none
resource_class_sharing( game        ,          0   ).   % none
resource_class_sharing( player      ,          0   ).   % none
resource_class_sharing( implicit    ,          0   ).   % none
resource_class_sharing( event       ,          1   ).   % AudioPlayback
resource_class_sharing( background  ,          0   ).   % none
resource_class_sharing( nobody      ,          0   ).   % none


/****************************************************************************
 *
 * valid combinaton of resources and classes
 *
 *****************************************************************************/
%                        resource      resource_class
%                    --------------------------------            
valid_resource_class( audio_playback ,    proclaimer  ).
valid_resource_class( audio_playback ,    navigator   ).
valid_resource_class( audio_playback ,    call        ).
valid_resource_class( audio_playback ,    videoeditor ).
valid_resource_class( audio_playback ,    camera      ).
valid_resource_class( audio_playback ,    ringtone    ).
valid_resource_class( audio_playback ,    alarm       ).
valid_resource_class( audio_playback ,    game        ).
valid_resource_class( audio_playback ,    player      ).
valid_resource_class( audio_playback ,    implicit    ).
valid_resource_class( audio_playback ,    event       ).
valid_resource_class( audio_playback ,    background  ).

valid_resource_class( video_playback ,    navigator   ).
valid_resource_class( video_playback ,    call        ).
valid_resource_class( video_playback ,    videoeditor ).
valid_resource_class( video_playback ,    camera      ).
valid_resource_class( video_playback ,    game        ).
valid_resource_class( video_playback ,    player      ).
valid_resource_class( video_playback ,    implicit    ).
valid_resource_class( video_playback ,    background  ).

valid_resource_class( audio_recording,    call        ).
valid_resource_class( audio_recording,    videoeditor ).
valid_resource_class( audio_recording,    camera      ).
valid_resource_class( audio_recording,    player      ).
valid_resource_class( audio_recording,    implicit    ).
valid_resource_class( audio_recording,    background  ).

valid_resource_class( video_recording,    videoeditor ).
valid_resource_class( video_recording,    camera      ).
valid_resource_class( video_recording,    implicit    ).
valid_resource_class( video_recording,    background  ).

valid_resource_class( vibra          ,    proclaimer  ).
valid_resource_class( vibra          ,    navigator   ).
valid_resource_class( vibra          ,    call        ).
valid_resource_class( vibra          ,    camera      ).
valid_resource_class( vibra          ,    ringtone    ).
valid_resource_class( vibra          ,    alarm       ).
valid_resource_class( vibra          ,    game        ).
valid_resource_class( vibra          ,    player      ).
valid_resource_class( vibra          ,    implicit    ).
valid_resource_class( vibra          ,    event       ).
valid_resource_class( vibra          ,    background  ).
valid_resource_class( vibra          ,    nobody      ).

valid_resource_class( leds           ,    proclaimer  ).
valid_resource_class( leds           ,    navigator   ).
valid_resource_class( leds           ,    call        ).
valid_resource_class( leds           ,    camera      ).
valid_resource_class( leds           ,    ringtone    ).
valid_resource_class( leds           ,    alarm       ).
valid_resource_class( leds           ,    game        ).
valid_resource_class( leds           ,    player      ).
valid_resource_class( leds           ,    implicit    ).
valid_resource_class( leds           ,    event       ).
valid_resource_class( leds           ,    background  ).
valid_resource_class( leds           ,    nobody      ).

valid_resource_class( backlight      ,    proclaimer  ).
valid_resource_class( backlight      ,    navigator   ).
valid_resource_class( backlight      ,    call        ).
valid_resource_class( backlight      ,    camera      ).
valid_resource_class( backlight      ,    ringtone    ).
valid_resource_class( backlight      ,    alarm       ).
valid_resource_class( backlight      ,    game        ).
valid_resource_class( backlight      ,    player      ).
valid_resource_class( backlight      ,    implicit    ).
valid_resource_class( backlight      ,    event       ).
valid_resource_class( backlight      ,    background  ).
valid_resource_class( backlight      ,    nobody      ).

valid_resource_class( system_button  ,    navigator   ).
valid_resource_class( system_button  ,    call        ).
valid_resource_class( system_button  ,    camera      ).
valid_resource_class( system_button  ,    ringtone    ).
valid_resource_class( system_button  ,    alarm       ).
valid_resource_class( system_button  ,    game        ).
valid_resource_class( system_button  ,    player      ).
valid_resource_class( system_button  ,    implicit    ).
valid_resource_class( system_button  ,    event       ).
valid_resource_class( system_button  ,    background  ).
valid_resource_class( system_button  ,    nobody      ).

valid_resource_class( lock_button    ,    navigator   ).
valid_resource_class( lock_button    ,    call        ).
valid_resource_class( lock_button    ,    camera      ).
valid_resource_class( lock_button    ,    ringtone    ).
valid_resource_class( lock_button    ,    alarm       ).
valid_resource_class( lock_button    ,    game        ).
valid_resource_class( lock_button    ,    player      ).
valid_resource_class( lock_button    ,    implicit    ).
valid_resource_class( lock_button    ,    event       ).
valid_resource_class( lock_button    ,    background  ).
valid_resource_class( lock_button    ,    nobody      ).

valid_resource_class( scale_button   ,    navigator   ).
valid_resource_class( scale_button   ,    call        ).
valid_resource_class( scale_button   ,    camera      ).
valid_resource_class( scale_button   ,    ringtone    ).
valid_resource_class( scale_button   ,    alarm       ).
valid_resource_class( scale_button   ,    game        ).
valid_resource_class( scale_button   ,    player      ).
valid_resource_class( scale_button   ,    implicit    ).
valid_resource_class( scale_button   ,    event       ).
valid_resource_class( scale_button   ,    background  ).
valid_resource_class( scale_button   ,    nobody      ).

valid_resource_class( lens_cover     ,    navigator   ).
valid_resource_class( lens_cover     ,    call        ).
valid_resource_class( lens_cover     ,    camera      ).
valid_resource_class( lens_cover     ,    ringtone    ).
valid_resource_class( lens_cover     ,    alarm       ).
valid_resource_class( lens_cover     ,    game        ).
valid_resource_class( lens_cover     ,    player      ).
valid_resource_class( lens_cover     ,    implicit    ).
valid_resource_class( lens_cover     ,    event       ).
valid_resource_class( lens_cover     ,    background  ).
valid_resource_class( lens_cover     ,    nobody      ).

valid_resource_class( headset_buttons ,   navigator   ).
valid_resource_class( headset_buttons ,   call        ).
valid_resource_class( headset_buttons ,   camera      ).
valid_resource_class( headset_buttons ,   ringtone    ).
valid_resource_class( headset_buttons ,   alarm       ).
valid_resource_class( headset_buttons ,   game        ).
valid_resource_class( headset_buttons ,   player      ).
valid_resource_class( headset_buttons ,   implicit    ).
valid_resource_class( headset_buttons ,   event       ).
valid_resource_class( headset_buttons ,   background  ).
valid_resource_class( headset_buttons ,   nobody      ).
