unit GApp;

interface

uses
  system.SysUtils,

  UDalinEngine, ULogThread , UConfig,  UTypes
  , FWinConfig , FServerMessage
  ;

type

  TApp = class
  private
    FEngine : TDalinEngine;
    FLog    : TLogThread;
    FRootDir: string;
    FLogDir: string;
    FQuoteDir: string;
    FConfig: TConfig;
    FDataDir: string;
    FAppStatus: TAppStatus;
    FOnAppStatusEvent: TAppStatusEvent;
    FErrorString: string;

    function  IsLogLevel(lLevel: TLogLevel): boolean;
    procedure SetAppStatus(const Value: TAppStatus);
    procedure PushLog(lLevel: TLogLevel; stPreFix, stData: string);
  public

    LogItems: array [TLogLevel] of TAppLogItems;

    constructor Create;
    destructor  Destroy; override;

    function LoadConfig : boolean;
    function SetDirInfo : boolean;

    procedure Log( lLevel : TLogLevel; stPrefix, stData : string ); overload;
    procedure Log( lLevel : TLogLevel; stData : string ); overload;
    procedure Log( lLevel : TLogLevel; const fmt: string; const Args: array of const ); overload;
    procedure Log( lLevel : TLogLevel; stPrefix : string; const fmt: string; const Args: array of const ); overload;
    procedure DebugLog( const fmt: string; const Args: array of const ); overload;
    procedure DebugLog( const fmt: string ); overload;

    procedure SaveData( aType : TLogDataType; sData : string);

    procedure CreateWinConfig;

    function GetPrecision : integer;

    property Engine : TDalinEngine read FEngine;

    property  Config : TConfig read FConfig ;
    property  LogDir : string read FLogDir write FLogDir;
    property  RootDir: string read FRootDir write FRootDir;
    property  QuoteDir  : string read FQuoteDir write FQuoteDir;
    property  DataDir  : string read FDataDir write FDataDir;

    property  ErrorString : string read FErrorString write FErrorString;
      // type
    property  AppStatus : TAppStatus read FAppStatus write SetAppStatus;
      // event
    property  OnAppStatusEvent : TAppStatusEvent read FOnAppStatusEvent write FOnAppStatusEvent;
  end;

var
  App : TApp;
  gWinCfg : TFrmWinConfig;
  gMessage: TFrmServerMessage;

implementation

uses
  GLibs   ,
  UConsts ,
  Windows
  ;

{ TApp }

constructor TApp.Create;
var
  alv : TLogLevel;
begin
//( llFatal, llError, llWarning, llInfo, llDebug, llTrace );
  FEngine := TDalinEngine.Create;
  FLog    := TLogThread.Create;
  FAppStatus := asNone;

  for alv := llFatal to High(TLogLevel) do
    LogItems[alv] := TAppLogItems.Create;

  gMessage   := TFrmServerMessage.Create( nil );
end;


destructor TApp.Destroy;
var
  alv : TLogLevel;
begin

  if gWinCfg <> nil then
    gWinCfg.Free;

  FEngine.Free;
  App.Log(llInfo, '', '--- Engine free ---');
  FLog.Terminate;

  if gMessage <> nil then
    gMessage.Free;

  for alv := llFatal to High(TLogLevel) do
    LogItems[alv].Free;

  inherited;
end;


function TApp.GetPrecision: integer;
begin
  if FConfig.VerifyMod then
    Result := 3
  else
    Result := 2;
end;

function TApp.LoadConfig: boolean;
begin
  if not FConfig.LoadConfig then Exit(false);
  if not FEngine.ApiConfig.LoadExchangeConfig then Exit(false);

  Result := SetDirInfo;

  FLog.LogDir := FLogDir;
  FLog.DataDir:= FDataDir;
end;



procedure TApp.SetAppStatus(const Value: TAppStatus);
begin

  if Assigned( OnAppStatusEvent ) then
    if FAppStatus <> Value then
    begin
//      if Engine.AppStatus = asLoad then

//        Exit;


      FAppStatus :=  Value;
      Log(llInfo, 'App status is %s ', [ TAppStatusDesc[Value] ] );
      OnAppStatusEvent( Value );
    end;

end;

function TApp.SetDirInfo: boolean;
begin
  Result := true;
  try
    FRootDir  := AppDir;
    FLogDir   := ComposeFilePath([FRootDir, FConfig.LOG_DIR]);
    FQuoteDir := ComposeFilePath([FRootDir, FConfig.QUOTE_DIR]);
    FDataDir  := ComposeFilePath([FRootDir, FConfig.DATA_DIR]);
  except
    Result := false;
  end;
end;

function TApp.IsLogLevel( lLevel : TLogLevel ) : boolean;
begin

  if Integer(lLevel) <= FConfig.LOG_LEVEL then
    result := true
  else
    result := false;

  if FLog = nil then
    result := false;
end;

////  LOG

procedure TApp.PushLog(lLevel : TLogLevel; stPreFix, stData: string);
var
  aLog : TAppLogItem;
begin
  aLog := nil;

  case lLevel of
    llFatal,
    llError,
    llWarning:
      begin
        aLog := LogItems[lLevel].New(lLevel);
        with aLog do
        begin
//          LogSource := stSource;
          LogTitle  := stPreFix;
          LogDesc   := stData;
//          LogData   := stData;
        end;

      end;
    else exit;
  end;

  if (aLog <> nil) and ( gMessage <> nil ) then begin
    PostMessage( gMessage.Handle, WM_LOGARRIVED, Integer( lLevel ), 0);
    gMessage.Show;
  end;
end;

procedure TApp.Log(lLevel : TLogLevel; stPrefix: string; const fmt: string;
  const Args: array of const);
begin
  if IsLogLevel(lLevel) then begin
    PushLog(lLevel, stPrefix, Format( fmt, Args ));
    FLog.Log(integer(lLevel), stPrefix, Format( fmt, Args ) );
  end;
end;

procedure TApp.Log(lLevel: TLogLevel; stData: string);
begin
  if IsLogLevel(lLevel) then begin
    PushLog(lLevel, '', stData);
    FLog.Log(integer(lLevel), '', stData);
  end;
end;

procedure TApp.Log(lLevel: TLogLevel; const fmt: string;
  const Args: array of const);
begin
  if IsLogLevel(lLevel) then begin
    PushLog(lLevel, '', Format( fmt, Args ));
    FLog.Log(integer(lLevel), '', Format( fmt, Args ) );
  end;
end;

procedure TApp.Log(lLevel : TLogLevel; stPrefix, stData: string);
begin
  if IsLogLevel(lLevel) then begin
    PushLog(lLevel, stPrefix, stData);
    FLog.Log(integer(lLevel), stPrefix, stData);
  end;
end;

////

procedure TApp.DebugLog(const fmt: string; const Args: array of const);
begin
  if IsLogLevel(llDebug) then
    FLog.Log(integer(llDebug), '', Format( fmt, Args ) );
end;

procedure TApp.DebugLog(const fmt: string);
begin
  if IsLogLevel(llDebug) then
    FLog.Log(integer(llDebug), '', Format( '%s', [fmt] ) );
end;

procedure TApp.SaveData(aType: TLogDataType; sData: string);
begin
  FLog.SaveData( integer(aType), sData );
end;

procedure TApp.CreateWinConfig;
begin
  gWinCfg := TFrmWinConfig.Create( nil );
end;

end.
