:- module(video_route, [set_video_routes/1]).

rules([set_video_routes/1]).

/*
 * lower case canonical names
 * for TV-OUT signaling standards
 */
canonical_tv_standard( pal , pal).
canonical_tv_standard('PAL', pal).
canonical_tv_standard('Pal', pal).

canonical_tv_standard( ntsc , ntsc).
canonical_tv_standard('NTSC', ntsc).
canonical_tv_standard('Ntsc', ntsc).

canonical_aspect_ratio( normal , normal).
canonical_aspect_ratio('NORMAL', normal).
canonical_aspect_ratio('Normal', normal).

canonical_aspect_ratio( wide , wide).
canonical_aspect_ratio('WIDE', wide).
canonical_aspect_ratio('Wide', wide).

/*
 * set_video_routes
 */
route_to_device(Device) :-
    video_device(Device),
    accessory:selectable_video(Device),
    !.

set_video_routes(ActionList) :-
    route_to_device(Device),
    current_tv_standard(Standard),
    current_aspect_ratio(Ratio),
    ActionList = [[video_route, [device, Device],
		                [tvstandard, Standard],
                                [aspectratio, Ratio]
                 ]].


/*
 * FactStore interface
 */


current_route(Where) :-
    fact_exists('com.nokia.policy.video_route', [device], [Where]).

current_tv_standard(Standard) :-
    fact_exists('com.nokia.policy.gconf',
		[key, value], ['/system/tvout', Value]),
    canonical_tv_standard(Value, Standard), !
    ;
    Standard = pal.

current_aspect_ratio(Ratio) :-
    fact_exists('com.nokia.policy.gconf',
		[key, value], ['/system/aspectratio', Value]),
    canonical_aspect_ratio(Value, Ratio), !
    ;
    Ratio = normal.
