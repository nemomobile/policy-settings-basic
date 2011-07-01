%           ActiveGroup  GroupToLimit Level
%           ----------------------------------
volume_limit( navigator ,  alwayson   , 100  ).
volume_limit( navigator ,  nonsilent  , 100  ).
volume_limit( navigator ,  cstone     , 100  ).
volume_limit( navigator ,  navigator  , 100  ).
volume_limit( navigator ,  call       , 100  ).
volume_limit( navigator ,  videoeditor, 100  ).
volume_limit( navigator ,  camera     , 100  ).
volume_limit( navigator ,  ringtone   , 100  ).
volume_limit( navigator ,  alarm      , 100  ).
volume_limit( navigator ,  game       , 0    ).
volume_limit( navigator ,  player     , 0    ).
volume_limit( navigator ,  flash      , 0    ).
volume_limit( navigator ,  event      , 100  ).
volume_limit( navigator ,  systemsound, 100  ).
volume_limit( navigator ,  inputsound , 0    ).
volume_limit( navigator ,  othermedia , 0    ).
volume_limit( navigator ,  background , 50   ).
volume_limit( navigator ,  idle       , 100  ).

volume_limit( call      ,  alwayson   , 100  ).
volume_limit( call      ,  nonsilent  , 100  ).
volume_limit( call      ,  cstone     , 100  ).
volume_limit( call      ,  navigator  , 100  ).
volume_limit( call      ,  call       , 100  ).
volume_limit( call      ,  videoeditor, 0    ).
volume_limit( call      ,  camera     , 100  ).
volume_limit( call      ,  ringtone   , 0    ).
volume_limit( call      ,  alarm      , 0    ).
volume_limit( call      ,  game       , 0    ).
volume_limit( call      ,  player     , 0    ).
volume_limit( call      ,  flash      , 0    ).
volume_limit( call      ,  event      , 0    ).
volume_limit( call      ,  systemsound, 100  ).
volume_limit( call      ,  inputsound , 0    ).
volume_limit( call      ,  othermedia , 0    ).
volume_limit( call      ,  background , 10   ).
volume_limit( call      ,  idle       , 100  ).

volume_limit( videoeditor,  alwayson   , 100  ).
volume_limit( videoeditor,  nonsilent  , 100  ).
volume_limit( videoeditor,  cstone     , 100  ).
volume_limit( videoeditor,  navigator  , 100  ).
volume_limit( videoeditor,  call       , 100  ).
volume_limit( videoeditor,  videoeditor, 100  ).
volume_limit( videoeditor,  camera     , 100  ).
volume_limit( videoeditor,  ringtone   , 0    ).
volume_limit( videoeditor,  alarm      , 0    ).
volume_limit( videoeditor,  game       , 0    ).
volume_limit( videoeditor,  player     , 0    ).
volume_limit( videoeditor,  flash      , 0    ).
volume_limit( videoeditor,  event      , 0    ).
volume_limit( videoeditor,  systemsound, 100  ).
volume_limit( videoeditor,  inputsound , 0    ).
volume_limit( videoeditor,  othermedia , 0    ).
volume_limit( videoeditor,  background , 10   ).
volume_limit( videoeditor,  idle       , 100  ).

volume_limit( camera    ,  alwayson   , 0    ).
volume_limit( camera    ,  nonsilent  , 0    ).
volume_limit( camera    ,  cstone     , 0    ).
volume_limit( camera    ,  navigator  , 100  ).
volume_limit( camera    ,  call       , 100  ).
volume_limit( camera    ,  videoeditor, 100  ).
volume_limit( camera    ,  camera     , 100  ).
volume_limit( camera    ,  ringtone   , 0    ).
volume_limit( camera    ,  alarm      , 0    ).
volume_limit( camera    ,  game       , 0    ).
volume_limit( camera    ,  player     , 0    ).
volume_limit( camera    ,  flash      , 0    ).
volume_limit( camera    ,  event      , 0    ).
volume_limit( camera    ,  systemsound, 0    ).
volume_limit( camera    ,  inputsound , 0    ).
volume_limit( camera    ,  othermedia , 0    ).
volume_limit( camera    ,  background , 0    ).
volume_limit( camera    ,  idle       , 0    ).

volume_limit( ringtone  ,  alwayson   , 100  ).
volume_limit( ringtone  ,  nonsilent  , 100  ).
volume_limit( ringtone  ,  cstone     , 100  ).
volume_limit( ringtone  ,  navigator  , 100  ).
volume_limit( ringtone  ,  call       , 100  ).
volume_limit( ringtone  ,  videoeditor, 100  ).
volume_limit( ringtone  ,  camera     , 100  ).
volume_limit( ringtone  ,  ringtone   , 100  ).
volume_limit( ringtone  ,  alarm      , 0    ).
volume_limit( ringtone  ,  game       , 0    ).
volume_limit( ringtone  ,  player     , 0    ).
volume_limit( ringtone  ,  flash      , 0    ).
volume_limit( ringtone  ,  event      , 0    ).
volume_limit( ringtone  ,  systemsound, 0    ).
volume_limit( ringtone  ,  inputsound , 0    ).
volume_limit( ringtone  ,  othermedia , 0    ).
volume_limit( ringtone  ,  background , 0    ).
volume_limit( ringtone  ,  idle       , 0    ).

volume_limit( alarm     ,  alwayson   , 100  ).
volume_limit( alarm     ,  nonsilent  , 100  ).
volume_limit( alarm     ,  cstone     , 0    ).
volume_limit( alarm     ,  navigator  , 100  ).
volume_limit( alarm     ,  call       , 0    ).
volume_limit( alarm     ,  videoeditor, 0    ).
volume_limit( alarm     ,  camera     , 100  ).
volume_limit( alarm     ,  ringtone   , 0    ).
volume_limit( alarm     ,  alarm      , 100  ).
volume_limit( alarm     ,  game       , 0    ).
volume_limit( alarm     ,  player     , 0    ).
volume_limit( alarm     ,  flash      , 0    ).
volume_limit( alarm     ,  event      , 0    ).
volume_limit( alarm     ,  systemsound, 0    ).
volume_limit( alarm     ,  inputsound , 0    ).
volume_limit( alarm     ,  othermedia , 0    ).
volume_limit( alarm     ,  background , 0    ).
volume_limit( alarm     ,  idle       , 0    ).

volume_limit( game      ,  alwayson   , 100  ).
volume_limit( game      ,  nonsilent  , 100  ).
volume_limit( game      ,  cstone     , 100  ).
volume_limit( game      ,  navigator  , 100  ).
volume_limit( game      ,  call       , 100  ).
volume_limit( game      ,  videoeditor, 100  ).
volume_limit( game      ,  camera     , 100  ).
volume_limit( game      ,  ringtone   , 100  ).
volume_limit( game      ,  alarm      , 100  ).
volume_limit( game      ,  game       , 100  ).
volume_limit( game      ,  player     , 100  ).
volume_limit( game      ,  flash      , 0    ).
volume_limit( game      ,  event      , 100  ).
volume_limit( game      ,  systemsound, 100  ).
volume_limit( game      ,  inputsound , 0    ).
volume_limit( game      ,  othermedia , 0    ).
volume_limit( game      ,  background , 100  ).
volume_limit( game      ,  idle       , 100  ).

volume_limit( player    ,  alwayson   , 100  ).
volume_limit( player    ,  nonsilent  , 100  ).
volume_limit( player    ,  cstone     , 100  ).
volume_limit( player    ,  navigator  , 100  ).
volume_limit( player    ,  call       , 100  ).
volume_limit( player    ,  videoeditor, 100  ).
volume_limit( player    ,  camera     , 100  ).
volume_limit( player    ,  ringtone   , 100  ).
volume_limit( player    ,  alarm      , 100  ).
volume_limit( player    ,  game       , 100  ).
volume_limit( player    ,  player     , 100  ).
volume_limit( player    ,  flash      , 0    ).
volume_limit( player    ,  event      , 100  ).
volume_limit( player    ,  systemsound, 100  ).
volume_limit( player    ,  inputsound , 0    ).
volume_limit( player    ,  othermedia , 0    ).
volume_limit( player    ,  background , 100  ).
volume_limit( player    ,  idle       , 100  ).

volume_limit( flash     ,  alwayson   , 100  ).
volume_limit( flash     ,  nonsilent  , 100  ).
volume_limit( flash     ,  cstone     , 100  ).
volume_limit( flash     ,  navigator  , 100  ).
volume_limit( flash     ,  call       , 100  ).
volume_limit( flash     ,  videoeditor, 100  ).
volume_limit( flash     ,  camera     , 100  ).
volume_limit( flash     ,  ringtone   , 100  ).
volume_limit( flash     ,  alarm      , 100  ).
volume_limit( flash     ,  game       , 100  ).
volume_limit( flash     ,  player     , 100  ).
volume_limit( flash     ,  flash      , 100  ).
volume_limit( flash     ,  event      , 100  ).
volume_limit( flash     ,  systemsound, 100  ).
volume_limit( flash     ,  inputsound , 0    ).
volume_limit( flash     ,  othermedia , 0    ).
volume_limit( flash     ,  background , 100  ).
volume_limit( flash     ,  idle       , 100  ).

volume_limit( othermedia,  alwayson   , 100  ).
volume_limit( othermedia,  nonsilent  , 100  ).
volume_limit( othermedia,  cstone     , 100  ).
volume_limit( othermedia,  navigator  , 100  ).
volume_limit( othermedia,  call       , 100  ).
volume_limit( othermedia,  videoeditor, 100  ).
volume_limit( othermedia,  camera     , 100  ).
volume_limit( othermedia,  ringtone   , 100  ).
volume_limit( othermedia,  alarm      , 100  ).
volume_limit( othermedia,  game       , 100  ).
volume_limit( othermedia,  player     , 100  ).
volume_limit( othermedia,  flash      , 100  ).
volume_limit( othermedia,  event      , 100  ).
volume_limit( othermedia,  systemsound, 100  ).
volume_limit( othermedia,  inputsound , 100  ).
volume_limit( othermedia,  othermedia , 100  ).
volume_limit( othermedia,  background , 100  ).
volume_limit( othermedia,  idle       , 100  ).

volume_limit( event     ,  alwayson   , 100  ).
volume_limit( event     ,  nonsilent  , 100  ).
volume_limit( event     ,  cstone     , 0    ).
volume_limit( event     ,  navigator  , 100  ).
volume_limit( event     ,  call       , 0    ).
volume_limit( event     ,  videoeditor, 0    ).
volume_limit( event     ,  camera     , 100  ).
volume_limit( event     ,  ringtone   , 0    ).
volume_limit( event     ,  alarm      , 0    ).
volume_limit( event     ,  game       , 0    ).
volume_limit( event     ,  player     , 0    ).
volume_limit( event     ,  flash      , 0    ).
volume_limit( event     ,  event      , 100  ).
volume_limit( event     ,  systemsound, 100  ).
volume_limit( event     ,  inputsound , 0    ).
volume_limit( event     ,  othermedia , 0    ).
volume_limit( event     ,  background , 10   ).
volume_limit( event     ,  idle       , 100  ).

volume_limit( background,  alwayson   , 100  ).
volume_limit( background,  nonsilent  , 100  ).
volume_limit( background,  cstone     , 100  ).
volume_limit( background,  navigator  , 100  ).
volume_limit( background,  call       , 100  ).
volume_limit( background,  videoeditor, 100  ).
volume_limit( background,  camera     , 100  ).
volume_limit( background,  ringtone   , 100  ).
volume_limit( background,  alarm      , 100  ).
volume_limit( background,  game       , 100  ).
volume_limit( background,  player     , 100  ).
volume_limit( background,  flash      , 100  ).
volume_limit( background,  event      , 100  ).
volume_limit( background,  systemsound, 100  ).
volume_limit( background,  inputsound , 100  ).
volume_limit( background,  othermedia , 100  ).
volume_limit( background,  background , 100  ).
volume_limit( background,  idle       , 100  ).

volume_limit( idle      ,  alwayson   , 100  ).
volume_limit( idle      ,  nonsilent  , 100  ).
volume_limit( idle      ,  cstone     , 100  ).
volume_limit( idle      ,  navigator  , 100  ).
volume_limit( idle      ,  call       , 100  ).
volume_limit( idle      ,  videoeditor, 100  ).
volume_limit( idle      ,  camera     , 100  ).
volume_limit( idle      ,  ringtone   , 100  ).
volume_limit( idle      ,  alarm      , 100  ).
volume_limit( idle      ,  game       , 100  ).
volume_limit( idle      ,  player     , 100  ).
volume_limit( idle      ,  flash      , 100  ).
volume_limit( idle      ,  event      , 100  ).
volume_limit( idle      ,  systemsound, 100  ).
volume_limit( idle      ,  inputsound , 100  ).
volume_limit( idle      ,  othermedia , 100  ).
volume_limit( idle      ,  background , 100  ).
volume_limit( idle      ,  idle       , 100  ).

