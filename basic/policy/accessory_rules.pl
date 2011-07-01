/*
 *
 */

%
% *** bta2dp ***
%
% accessories that can coexists:
%   - bthsp
%   - tvout (audio goes to bta2dp, but video goes to tvout)
%   - headmike
%
% accessories that are mutualy exclusive:
%
accessory_exclude( bta2dp, headset   ).
accessory_exclude( bta2dp, headphone ).
accessory_exclude( bta2dp, hac       ).

%
% *** bthsp ***
%
% accessories that can coexists:
%   - bta2dp
%   - tvout (audio goes to bthsp while video goes to tvout)
%
% accessories that are mutualy exclusive:
%
accessory_exclude( bthsp, headset   ).
accessory_exclude( bthsp, headphone ).
accessory_exclude( bthsp, headmike  ).
accessory_exclude( bthsp, hac       ).

%
% *** headset ***
%
% accessories that can coexists:
%   - none
%
% accessories that are mutualy exclusive:
%
accessory_exclude( headset, bta2dp    ).
accessory_exclude( headset, bthsp     ).
accessory_exclude( headset, headphone ).
accessory_exclude( headset, tvout     ).
accessory_exclude( headset, headmike  ).
accessory_exclude( headset, hac       ).

%
% *** headphone ***
%
% accessories that can coexists:
%   - none
%
% accessories that are mutualy exclusive:
%
accessory_exclude( headphone, bta2dp   ).
accessory_exclude( headphone, bthsp    ).
accessory_exclude( headphone, headset  ).
accessory_exclude( headphone, tvout    ).
accessory_exclude( headphone, headmike ).
accessory_exclude( headphone, hac      ).

%
% *** tvout ***
%
% accessories that can coexists:
%   - bta2dp
%   - bthsp
%   - hac
%
% accessories that can coexists:
%   
accessory_exclude( tvout, headset   ).
accessory_exclude( tvout, headphone ).
accessory_exclude( tvout, headmike  ).

%
% *** headmike ***
%
% accessories that can coexists:
%   - bta2dp
%   - bthsp
%   - hac
%
% accessories that can coexists:
%   
accessory_exclude( headmike, headset   ).
accessory_exclude( headmike, headphone ).
accessory_exclude( headmike, tvout     ).

%
% *** hac ***
%
% accessories that can coexists:
%   - tvout
%   - headmike
%
% accessories that can coexists:
%   
accessory_exclude( hac, bta2dp    ).
accessory_exclude( hac, bthsp     ).
accessory_exclude( hac, headset   ).
accessory_exclude( hac, headphone ).

