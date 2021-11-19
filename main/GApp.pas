unit GApp;

interface

uses

  UDalinEngine, ULogThread , UConfig
  ;

type

  TApp = class
  private
    FEngine : TDalinEngine;
    FLog    : TLogThread;
  public
    constructor Create;
    destructor  Destroy; override;
  end;

var
  App : TApp;

implementation

{ TApp }

constructor TApp.Create;
begin
  FEngine := TDalinEngine.Create;
  FLog    := TLogThread.Create;

end;

destructor TApp.Destroy;
begin

  FEngine.Free;
  FLog.Terminate;

  inherited;
end;

end.
