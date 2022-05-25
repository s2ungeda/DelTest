unit URestThread;

interface

uses
  system.Classes, system.SysUtils, system.DateUtils

  ,Windows, SyncObjs

  ;

type

  TRestThread = class( TThread)
  private
    FEvent  : TEvent;
    FMutex  : HWND;
    FQueue  : TList;
  protected
    procedure Execute; override;
    procedure SyncProc;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TRestThread }

constructor TRestThread.Create;
begin
  inherited Create(false);
  FreeOnTerminate := false;
  Priority  := tpNormal;

  FEvent  := TEvent.Create( nil, False, False, '');
  FMutex  := CreateMuTex( nil, false, '' );
  FQueue  := TList.Create;

end;

destructor TRestThread.Destroy;
begin

  CloseHandle( FMutex );
  FEvent.Free;
  FQueue.Free;
  inherited;
end;

procedure TRestThread.Execute;
begin
  inherited;

  while not Terminated do
  begin

    if not( FEvent.WaitFor( 300 ) in [wrSignaled] ) then Continue;

//    while FQueue.Count > 0 do begin
//      FData := PopData;
//      if FData <> nil then begin
//        Synchronize( SyncProc );
//        Dispose( FData );
//      end;
//      Application.ProcessMessages;
//    end;
  end;


end;

procedure TRestThread.SyncProc;
begin

end;

end.

