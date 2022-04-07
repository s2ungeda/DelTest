unit DalinMain;
interface
uses
  Winapi.Windows, Winapi.Messages
  , System.SysUtils, System.Variants, System.Classes, System.IniFiles
  , Vcl.Graphics,  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls ,

  UStorage,

  NMainMenu,

  UTypes, Vcl.ExtCtrls, Vcl.ComCtrls


  ;
type
  TFrmDalinMain = class(TForm)
    QryTimer: TTimer;
    stsBar: TStatusBar;
    Panel1: TPanel;
    edtExInterval: TLabeledEdit;
    Button1: TButton;
    Edit1: TEdit;
    cbManual: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);

    procedure QryTimerTimer(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    FExRate : integer;
    FDnw    : integer;
    procedure init;
    function IsRealyClose: boolean;
    procedure SetEnv;
    procedure GetExRate( bInit : boolean );
    procedure DalinStatusEvent( asType : TAppStatus );
    procedure ReadExRate( bInit : boolean = false );
    procedure AppException(Sender: TObject; E: Exception);
    function GetInterval: integer;

  public
    { Public declarations }
    procedure Start;
    procedure SetValue;
    procedure LoadEnv(aStorage: TStorage);
    procedure SaveEnv(aStorage: TStorage);
  end;

var
  FrmDalinMain : TFrmDalinMain;

implementation
{$R *.dfm}
uses
  GApp , GLibs ,
  UDalinStatusEvent
;


procedure TFrmDalinMain.AppException(Sender: TObject; E: Exception);
begin
  App.log( llError, 'Application Error : ' + E.Message );
  //gEnv.AppMsg( WIN_ERR, 'Application Error : ' + E.Message );
end;
procedure TFrmDalinMain.FormCreate(Sender: TObject);
begin
  //
  Application.OnException := AppException;

  init;

  if not App.LoadConfig then
  begin
    ShowMessage('환경설정 파일을 읽을수 없음');
    Application.Terminate;
  end;

  // 필수 디렉토리 생성..
  SetEnv;

  App.Log(llInfo, '', '---start---');

end;

procedure TFrmDalinMain.DalinStatusEvent(asType: TAppStatus);
begin
  AppStatusEvent(asType);
end;

procedure TFrmDalinMain.FormClose(Sender: TObject; var Action: TCloseAction);
var
  i : integer;
begin
  if not IsRealyClose then
  begin
    Action := caNone;
    Exit;
  end;

  QryTimer.Enabled := false;

  if App.AppStatus > asLoad then
    App.Engine.FormBroker.Save( ComposeFilePath( [App.DataDir, App.Config.DATA_FILE] ) );

  for i := ComponentCount-1 downto 0 do
    if Components[i] is TForm then
      Components[i].Free;

  App.Log(llInfo, '', '--- Form Saved ---');
  App.Engine.ApiManager.DisConnectAll;
  App.Log(llInfo, '', '--- DisConnectAll ---');

  CloseApp( App.Config.ClassName );

  Action := caFree;
end;


procedure TFrmDalinMain.FormDestroy(Sender: TObject);
begin
  //
  App.Free;
end;



procedure TFrmDalinMain.GetExRate( bInit : boolean );
begin

  ReadExRate( bInit );
  stsBar.Panels[0].Text := Format(' %.2f', [ App.Engine.ApiManager.ExRate.Value ] );
end;

procedure TFrmDalinMain.init;
begin
  App := TApp.Create;

  FExRate := 0;
  FDnw    := 0;
end;

function  TFrmDalinMain.IsRealyClose : boolean;
var
  stLog : string;
begin
  Exit (true);
  stLog  := '정말로 종료하시겠습니까?';
  if (MessageDlg( stLog, mtInformation, [mbYes, mbNo], 0) = mrYes ) then
    Result := true
  else
    Result := false;
end;

procedure TFrmDalinMain.SetEnv;
begin
  if not DirectoryExists(App.DataDir) then CreateDir(App.DataDir);
  if not DirectoryExists(App.QuoteDir) then CreateDir(App.QuoteDir);
  if not DirectoryExists(App.LogDir) then CreateDir(App.LogDir);

  App.OnAppStatusEvent := DalinStatusEvent;

  if App.Config.VerifyMod then
  begin
    Panel1.Visible  := true;
  end;

end;

procedure TFrmDalinMain.SetValue;
begin


  GetExRate( true );
  QryTimer.Enabled := true;
  // 구독/취소 이벤트 연결..
  // 실행시 전종목 구독 으로 변경.
  App.Engine.QuoteBroker.SetEvent;
end;

procedure TFrmDalinMain.Start;
begin

 // Exit;
//  App.AppStatus := asinit;
  App.AppStatus := asload;
//  ExcuteApp( Handle, App.Config.ClassName, App.Config.AppName  );
end;

procedure TFrmDalinMain.QryTimerTimer(Sender: TObject);
var
  I: Integer;
  sTmp : string;
begin
  if FExRate >= GetInterval then
  begin
    FExRate := 0;
    GetExRate( false );
  end;
  inc(FExRate);
  inc(FDnw);

  App.Engine.ApiManager.CheckCount;

  sTmp := '';
  for I := 0 to App.Engine.ApiManager.SockState.Count-1 do
    if sTmp = '' then
      sTmp := App.Engine.ApiManager.SockState[i]
    else
      sTmp := sTmp + ',' + App.Engine.ApiManager.SockState[i];

  stsBar.Panels[1].Text := FormatDateTime('hh:nn:ss', now) ;
  stsBar.Panels[2].Text := stmp;
end;

function TFrmDalinMain.GetInterval : integer;
begin
  Result := StrToIntDef( edtExInterval.Text, 10 ) ;
end;

procedure TFrmDalinMain.Button1Click(Sender: TObject);
begin
  GetExRate( false );
  QryTimer.Interval :=  GetInterval;
end;

procedure TFrmDalinMain.SaveEnv( aStorage : TStorage );
begin
  if aStorage = nil then Exit;

  aStorage.FieldByName('Left').AsInteger := Left;
  aStorage.FieldByName('Top').AsInteger := Top;
  aStorage.FieldByName('width').AsInteger := Width;
  aStorage.FieldByName('Height').AsInteger := Height;

  aStorage.FieldByName('ExRateInterval').AsString := edtExInterval.Text;
  aStorage.FieldByName('mExRate').AsString := edit1.Text  ;
  aStorage.FieldByName('UseManual').AsBoolean := cbManual.Checked;

end;

procedure TFrmDalinMain.LoadEnv( aStorage : TStorage );
var
  isSave : boolean;
begin
  if aStorage = nil then Exit;
  Left  := aStorage.FieldByName('Left').AsInteger;
  Top   := aStorage.FieldByName('Top').AsInteger;

  Width := aStorage.FieldByName('width').AsInteger;
  Height:= aStorage.FieldByName('Height').AsInteger;

  edtExInterval.Text  := aStorage.FieldByName('ExRateInterval').AsStringDef( '10');
  edit1.Text          := aStorage.FieldByName('mExRate').AsStringDef('0');
  cbManual.Checked    := aStorage.FieldByName('UseManual').AsBooleanDef(false);
end;


procedure TFrmDalinMain.ReadExRate( bInit : boolean );
var
  f : TextFile;  bOK : boolean;
  stData, FileName : string;
  bExist : boolean;

  dTmp : double;
  function IsErrorOccur(iRes: integer; st: string): Boolean;
  begin
    Result := true;

    if iRes <> 0 then begin
      MessageDlg('Order No File IO Error ( '+ st + InttoStr( GetLastError ) +' ) ', mtInformation, [mbOK], 0 );
      Result := false;
    end;
  end;

begin

  if (cbManual.Checked) and ( not bInit ) then
  begin
    dTmp :=  App.Engine.ApiManager.ExRate.Value;
    App.Engine.ApiManager.ExRate.Value := StrToFloatDef( edit1.Text, dtmp);
    Exit;
  end;

  FileName := ExtractFilePath(ParamStr(0))+'Data\';
  FileName := FileName + 'exchangeRate.log';

  bExist  := FileExists(FileName);

  if not bExist then
    Exit;

  AssignFile( f, FileName );
  {$I-}
  Reset( f );
  bOK := IsErrorOccur(IOResult, 'Reset');    // 런타임시 IO 에러 체크..
  {$I+}

  if not bOK then
  begin
    CloseFile(f);
    Exit;
  end;

  Try
    While( not Eoln(f) ) do
    Begin
      {$I-}
      ReadLn( f, stData );
      bOK := IsErrorOccur(IOResult, 'ReadLn');    // 런타임시 IO 에러 체크..
      {$I+}
      If not bOK then ConTinue;

      App.Engine.ApiManager.ExRate.Value := StrToFloat( stData);
      //gEnv.ConConfig.LastOrderNo := StrToIntDef( stData, gEnv.ConConfig.LastOrderNo );

    End;

  finally
    CloseFile(f);
  end;
end;
end.
