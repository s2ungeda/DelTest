unit UDalinStatusEvent;

interface

uses
  UTypes
  ;

procedure AppStatusEvent( asType : TAppStatus );

implementation

uses
  GApp
  ;

procedure AppStatusEvent( asType : TAppStatus );
begin
  case asType of
    asNone: ;
    asInit: ;
  end;
end;

end.
