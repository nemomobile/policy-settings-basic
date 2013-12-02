/**********************************
 *
 *
 *
 **********************************/
%           Type    ResourceClass
%          ----------------------
audio_type( sink  ,     proclaimer ).
audio_type( sink  ,     navigator  ).
audio_type( sink  ,     call       ).
audio_type( source,     call       ).
audio_type( sink  ,     videoeditor).
audio_type( sink  ,     camera     ).
audio_type( source,     camera     ).
audio_type( sink  ,     ringtone   ).
audio_type( sink  ,     alarm      ).
audio_type( sink  ,     game       ).
audio_type( sink  ,     player     ).
audio_type( source,     player     ).
audio_type( sink  ,     implicit   ).
audio_type( source,     implicit   ).
audio_type( sink  ,     event      ).
audio_type( sink  ,     background ).
audio_type( sink  ,     nobody     ).
audio_type( source,     nobody     ).

/************************************
 * 
 * audio_group definitions
 *
 ************************************/
audio_group(alwayson).    % battery low notifications, etc.
audio_group(nonsilent).   % 
audio_group(cstone).      % call progress indication & DTMF for GSM and 3G
audio_group(navigator).   % Voice guided navigation
audio_group(call).        % Telephony GSM, 3G, Skype and other VoIP
audio_group(videoeditor). % Video editor
audio_group(camera).      % camera applications
audio_group(ringtone).    % phone ringing
audio_group(alarm).       % wakeup, calendar and other alarms
audio_group(game).        %
audio_group(player).      % Media player, Rapshody etc.
audio_group(flash).       % Browser embeded flash
audio_group(othermedia).  % Everything else (i.e. default group)
audio_group(event).       % Messages (SMS, Chat etc), network events etc
audio_group(systemsound). % Desktop sounds etc
audio_group(feedbacksound).  % UI sounds, pulldown sounds
audio_group(inputsound).  % Key presses, touchscreen sounds
audio_group(background).  % UI and sound-less rendering
audio_group(idle).        % If nothing runs this is active


/*******************************************
 * 
 * audio_group_type
 *
 *******************************************/
audio_group_type(event).     % subscription for playback availability
audio_group_type(resource).  % resource manipulated via the libresource library
audio_group_type(internal).  % resource but libresource dows not know about it

%                  Type      AudioGroup
%                ----------------------------
audio_group_type( event   ,   alwayson   ).
audio_group_type( event   ,   nonsilent  ).
audio_group_type( event   ,   cstone     ).
audio_group_type( resource,   navigator  ).
audio_group_type( resource,   call       ).
audio_group_type( resource,   videoeditor).
audio_group_type( resource,   camera     ).
audio_group_type( resource,   ringtone   ).
audio_group_type( resource,   alarm      ).
audio_group_type( resource,   game       ).
audio_group_type( resource,   player     ).
audio_group_type( resource,   flash      ).
audio_group_type( internal,   othermedia ).
audio_group_type( resource,   event      ).
audio_group_type( event   ,   systemsound).
audio_group_type( event   ,   feedbacksound).
audio_group_type( event   ,   inputsound ).
audio_group_type( resource,   background ).
audio_group_type( internal,   idle       ).

/******************************************************
 *
 *
 *
 ******************************************************/
%                          ResourceClass  AudioGroup
%                          ----------------------------
audio_group_resource_class(   proclaimer ,   alwayson   ).
audio_group_resource_class(   navigator  ,   navigator  ).
audio_group_resource_class(   call       ,   call       ).
audio_group_resource_class(   videoeditor,   videoeditor).
audio_group_resource_class(   camera     ,   camera     ).
audio_group_resource_class(   ringtone   ,   ringtone   ).
audio_group_resource_class(   alarm      ,   alarm      ).
audio_group_resource_class(   game       ,   game       ).
audio_group_resource_class(   player     ,   player     ).
audio_group_resource_class(   player     ,   flash      ).
audio_group_resource_class(   player     ,   othermedia ).
audio_group_resource_class(   event      ,   event      ).
audio_group_resource_class(   background ,   background ).
audio_group_resource_class(   nobody     ,   idle       ).


/******************************************************
 *
 * Highest relative priority is 0.
 * The maximum value is 7 (ie. the lowest priority).
 *
 ******************************************************/
%                                 ResourceClass  AudioGroup RelativePriority
%                                 ------------------------------------------
audio_group_relative_class_priority(  player    , player    ,      0  ).
audio_group_relative_class_priority(  player    , flash     ,      1  ).
audio_group_relative_class_priority(  player    , othermedia,      2  ).



/******************************************************
 *
 *
 *
 ******************************************************/
notification_class(ringtone).
notification_class(alarm).
notification_class(event).
