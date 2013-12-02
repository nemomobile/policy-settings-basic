configuration([hwconfig, resource_classes, audio_groups,
	       accessory_rules,
	       audio_route_rules, audio_volume_rules, audio_cork_rules,
	       resource_rules]).
subsystems([resource, accessory, video_route,
	    audio_route, audio_volume, audio_cork, audio_mute,
	    telephony, context]).


% Load our 'configuration' and 'subsystems'. libprolog.pl relies on subsystems.
:- configuration(List), ensure_loaded(List).
:- subsystems(List), use_module(List).
