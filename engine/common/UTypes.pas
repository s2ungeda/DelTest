unit UTypes;

interface

type

  TAppStatus = ( asNone, asInit );


  TAppStatusEvent  = procedure( asType : TAppStatus ) of object;

implementation

end.
