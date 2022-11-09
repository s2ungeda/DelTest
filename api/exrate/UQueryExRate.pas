unit UQueryExRate;

interface

uses
 System.Classes , System.SyncObjs, System.SysUtils, System.DateUtils
 , PythonEngine
 ;

type
  TPhtnToDlph = class( TThread )
  private
    FParent: TObject;
    FPhtnEngine: TPythonEngine;
    FRtnValue: TPythonDelphiVar;
    FStrings : TStringLIst;
    FEvent  : TEvent;
    FOnNotifiEvent: TGetStrProc;
    FLastTime: TDateTime;
    function GetLastValue: string;

  protected
    procedure Execute; override;
    procedure SyncProc;
  public
    constructor Create( var aEng : TObject; aParent : TObject ); overload;
    destructor  Destroy; override;

    function ExecutePhtn( aStrings : TStrings ) : string;

    procedure Fin;

    property PhtnEngine: TPythonEngine read FPhtnEngine;
    property Parent    : TObject  read FParent;
    property RtnValue  : TPythonDelphiVar read FRtnValue;
    property LastValue : string read GetLastValue;
    property LastTime  : TDateTime read FLastTime ;

    property OnNotifyEvent : TGetStrProc read FOnNotifiEvent write  FOnNotifiEvent;
  end;


implementation

{ TPhtnToDlph }

constructor TPhtnToDlph.Create(var aEng : TObject; aParent: TObject);
begin
  FParent := aParent;
  FPhtnEngine := aEng as TPythonEngine;
  FRtnValue   :=  TPythonDelphiVar.Create( nil );
  FRtnValue.Engine  := FPhtnEngine;

  FRtnValue.VarName := 'result';
  FRtnValue.Initialize;

  FStrings := TStringLIst.Create;
  FStrings.LoadFromFile('./Data/exRate.py');

  MaskFPUExceptions(True);

  FEvent  := TEvent.Create( nil, False, False, 'PYTHON_EVENT' );
  FlastTime := now;

  inherited Create(false);

//  FreeOnTerminate := false;
  Priority  := tpNormal;

end;

destructor TPhtnToDlph.Destroy;
begin

  FEvent.Free;
  FRtnValue.Free;
  FStrings.Free;
//  FPhtnEngine.Free;
  inherited;
end;

procedure TPhtnToDlph.Execute;
begin
  inherited;
  // 10초 동안 데이터가 없으면 안되니깐..
  ExecutePhtn( nil );

  while not Terminated do
  begin
    if FEvent.WaitFor( 1000 * 10 ) in [wrTimeout] then
    begin
      ExecutePhtn( nil );
      Synchronize(  SyncProc );
    end;
  end;
end;

function TPhtnToDlph.ExecutePhtn(aStrings: TStrings): string;
begin
  try
    if FStrings.Capacity > 0 then
    begin
      FPhtnEngine.ExecStrings( FStrings );
      FLastTime := now;
//      FGap := SecondsBetween( now, FlastTime);
    end;
  except
  end;
end;

procedure TPhtnToDlph.Fin;
begin
  Terminate;
  FEvent.SetEvent;
end;

function TPhtnToDlph.GetLastValue: string;
begin
  Result :=  FRtnValue.ValueAsString;
end;

procedure TPhtnToDlph.SyncProc;
begin
  if Assigned( FOnNotifiEvent ) then
    FOnNotifiEvent( FRtnValue.ValueAsString );

end;

end.
