unit GApp;

interface

uses
  System.Classes, System.SysUtils

  , UConfig, UTypes

  , ULogThread , UApiConfigManager

  , URestManager
  ;

type

  TLogLevel = ( llFatal, llError, llWarning, llInfo, llDebug, llTrace );

  TApp = class
  private

    FLog    : TLogThread;
    FRootDir: string;
    FLogDir: string;
    FQuoteDir: string;

    FDataDir: string;

    FErrorString: string;
    FApiConfig: TApiConfigManager;
    FConfig: TConfig;

    FPreFix : string;
    FRestManasger: TRestManager;
    function  IsLogLevel(lLevel: TLogLevel): boolean;

  public
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

    procedure CreateWinConfig;

    property  Config : TConfig read FConfig ;
    property  LogDir : string read FLogDir write FLogDir;
    property  RootDir: string read FRootDir write FRootDir;
    property  ApiConfig : TApiConfigManager read FApiConfig;

    property  RestManager : TRestManager read FRestManasger;

    property  ErrorString : string read FErrorString write FErrorString;

  end;

var
  App : TApp;

implementation

uses
  GLibs   ,
  UConsts
  ;

{ TApp }

constructor TApp.Create;
begin
  FPreFix := 'Rest';
  FApiConfig    := TApiConfigManager.Create;
  FLog    := TLogThread.Create;

  FRestManasger:= TRestManager.Create;
end;

procedure TApp.CreateWinConfig;
begin

end;

procedure TApp.DebugLog(const fmt: string; const Args: array of const);
begin
  if IsLogLevel(llDebug) then
    FLog.Log(integer(llDebug), FPreFix, Format( fmt, Args ) );
end;

procedure TApp.DebugLog(const fmt: string);
begin
  if IsLogLevel(llDebug) then
    FLog.Log(integer(llDebug),FPreFix, Format( '%s', [fmt] ) );
end;

destructor TApp.Destroy;
begin
  App.Log(llInfo, '', '--- Engine free ---');
  FLog.Terminate;
  FApiConfig.Free;
  FRestManasger.Free;
  inherited;
end;

function TApp.IsLogLevel(lLevel: TLogLevel): boolean;
begin
  if Integer(lLevel) <= FConfig.LOG_LEVEL then
    result := true
  else
    result := false;
end;

function TApp.LoadConfig: boolean;
begin
  if not FConfig.LoadConfig then Exit(false);
  if not ApiConfig.LoadExchangeConfig then Exit(false);

  Result := SetDirInfo;

  FLog.LogDir := FLogDir;
  FLog.DataDir:= FDataDir;
end;

procedure TApp.Log(lLevel : TLogLevel; stPrefix: string; const fmt: string;
  const Args: array of const);
begin

  if stPreFix = '' then
    stPreFix := FPreFix;

  if IsLogLevel(lLevel) then
    FLog.Log(integer(lLevel), stPrefix, Format( fmt, Args ) );
end;


procedure TApp.Log(lLevel: TLogLevel; stData: string);
begin
  if IsLogLevel(lLevel) then
    FLog.Log(integer(lLevel), FPreFix, stData);
end;

procedure TApp.Log(lLevel: TLogLevel; const fmt: string;
  const Args: array of const);
begin
  if IsLogLevel(lLevel) then
    FLog.Log(integer(lLevel), FPreFix, Format( fmt, Args ) );
end;

procedure TApp.Log(lLevel : TLogLevel; stPrefix, stData: string);
begin

  if stPreFix = '' then
    stPreFix := FPreFix;

  if IsLogLevel(lLevel) then
    FLog.Log(integer(lLevel), stPrefix, stData);
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

end.
