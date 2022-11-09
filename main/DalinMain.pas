unit DalinMain;
interface
uses
  Winapi.Windows, Winapi.Messages
  , System.SysUtils, System.Variants, System.Classes, System.IniFiles
  , Vcl.Graphics,  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls ,

  UStorage,

  NMainMenu,

  UTypes, Vcl.ExtCtrls, Vcl.ComCtrls, PythonEngine


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
    procedure stsBarDrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel;
      const Rect: TRect);
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
    function GetRect(oRect: TRect): TRect;

  public
    { Public declarations }
    procedure Start;
    procedure SetValue;
    procedure LoadEnv(aStorage: TStorage);
    procedure SaveEnv(aStorage: TStorage);
//		procedure OnExRateMessage(var msg: TMessage); message wm_CopyData;
    procedure OnExRateMessage(var msg: TMessage); message WM_EXRATE_MESSAGE;

  end;

var
  FrmDalinMain : TFrmDalinMain;

implementation
{$R *.dfm}
uses
  GApp , GLibs ,
  UDalinStatusEvent
  , UApiTypes
  , UQueryExRate
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

  Caption := Format('%s ver.%s', [Caption  , FileVersionToStr(Application.ExeName)]);

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

  App.Engine.SymbolCore.PreUnSubscribe;

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
  // 삭제될 함수..이젠 파일에서 안 읽음
  ReadExRate( bInit );
  stsBar.Panels[0].Text := Format(' %.2f', [ App.Engine.ApiManager.ExRate.GetExRate ] );
end;

procedure TFrmDalinMain.init;
begin
  App := TApp.Create;
  // 파이썬 엔진은 하나만..생성해야 해서.
  //App.Engine.ApiManager.ExRate.QueryExRate := TPhtnToDlph.Create( TObject(PyThonEngine), self );

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

//  sTmp := '';
//  for I := 0 to App.Engine.ApiManager.SockState.Count-1 do
//    if sTmp = '' then
//      sTmp := App.Engine.ApiManager.SockState[i]
//    else
//      sTmp := sTmp + ',' + App.Engine.ApiManager.SockState[i];

  if App.Engine.ApiManager.ExManagers[ekBinance].QuoteSock[1] <> nil  then
    Self.Canvas.TextOut(5,4 ,Format( 'BN(%d):%d', [App.Engine.ApiManager.ExManagers[ekBinance].QuoteSock[1].RcvRat,
       App.Engine.ApiManager.ExManagers[ekBinance].QuoteSock[1].RcvCnt]));

  if App.Engine.ApiManager.ExManagers[ekUpbit].QuoteSock[0] <> nil  then
    Self.Canvas.TextOut(5,24 ,Format( 'UP(%d):%d', [App.Engine.ApiManager.ExManagers[ekUpbit].QuoteSock[0].RcvRat,
       App.Engine.ApiManager.ExManagers[ekUpbit].QuoteSock[0].RcvCnt]));

  if App.Engine.ApiManager.ExManagers[ekBithumb].QuoteSock[0] <> nil  then
    Self.Canvas.TextOut(5,44 ,Format( 'BT(%d):%d', [App.Engine.ApiManager.ExManagers[ekBithumb].QuoteSock[0].RcvRat,
       App.Engine.ApiManager.ExManagers[ekBithumb].QuoteSock[0].RcvCnt]));

  stsBar.Panels[1].Text := FormatDateTime('hh:nn:ss', now) ;
 // stsBar.Panels[2].Text := stmp;
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
      App.Log(llError, 'Order No File IO Error ( '+ st + InttoStr( GetLastError ) +' ) ');
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
      // stData : 07:36:23,1417.00
      var tmp  : TArray<string>;
      var iLen : integer;
      tmp := stData.Split([',']);

      iLen:= Length(tmp);
      if iLen = 2 then begin
        App.Engine.ApiManager.ExRate.LastTime := GetStrToTime( tmp[0], 1 );
        App.Engine.ApiManager.ExRate.Value := StrToFloat( tmp[1]);
      end;

    End;

  finally
    CloseFile(f);
  end;
end;


procedure TFrmDalinMain.Start;
begin

 // Exit;
  App.AppStatus := asinit;
//  App.AppStatus := asload;
  ExcuteApp( Handle, App.Config.ClassName, App.Config.AppName  );
end;

function TFrmDalinMain.GetRect( oRect : TRect ) : TRect ;
begin
  Result := Rect( oRect.Left, oRect.Top, oRect.Right, oRect.Bottom );
end;

procedure TFrmDalinMain.stsBarDrawPanel(StatusBar: TStatusBar;
  Panel: TStatusPanel; const Rect: TRect);
  var
    oRect : TRect;
begin
  if stsBar.Tag >= 0 then begin
    StatusBar.Canvas.Brush.Color := clBtnFace;
    StatusBar.Canvas.Font.Color := clBlack;
  end
  else if stsBar.Tag < 0 then begin
    StatusBar.Canvas.Brush.Color := clRed;
    StatusBar.Canvas.Font.Color := clWhite;
  end;

  StatusBar.Canvas.FillRect( Rect );
  oRect := GetRect( Rect );
  DrawText( stsBar.Canvas.Handle, PChar( stsBar.Panels[2].Text ),
    Length( stsBar.Panels[2].Text ),
    oRect, DT_VCENTER or DT_LEFT )    ;
end;

procedure TFrmDalinMain.OnExRateMessage(var msg: TMessage);
begin
  // 상태 메세지
  if msg.WParam = 100 then
  begin
    if msg.LParam = 99 then begin
      stsBar.Panels[2].Text := 'ExRate : Closed';
      stsBar.Tag := -1;
    end
    else begin
      stsBar.Panels[2].Text := 'ExRate : Open';
      stsBar.Tag := 0;
    end;
  end;
//	copyDataStruct := Pointer(Msg.LParam);  	
//	Filename := PChar(copyDataStruct.lpData);
//  caption  := FileName + ' ' + intTostr(msg.WParam);
end;

end.
