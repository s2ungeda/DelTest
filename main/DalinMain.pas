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
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);

    procedure QryTimerTimer(Sender: TObject);
  private
    { Private declarations }
    FExRate : integer;
    FDnw    : integer;
    procedure init;
    function IsRealyClose: boolean;
    procedure SetEnv;
    procedure GetExRate;
    procedure DalinStatusEvent( asType : TAppStatus );

  public
    { Public declarations }
    procedure Start;
    procedure SetValue;
    procedure LoadEnv(aStorage: TStorage);
    procedure SaveEnv(aStorage: TStorage);
  end;

var
  FrmDalin : TFrmDalinMain;

implementation
{$R *.dfm}
uses
  GApp , GLibs ,
  UDalinStatusEvent
;


procedure TFrmDalinMain.FormCreate(Sender: TObject);
begin
  //
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
begin
  if not IsRealyClose then
  begin
    Action := caNone;
    Exit;
  end;

  QryTimer.Enabled := false;
  App.Engine.FormBroker.Save( ComposeFilePath( [App.DataDir, App.Config.DATA_FILE] ) );

  App.Engine.ApiManager.DisConnectAll;

  Action := caFree;
end;


procedure TFrmDalinMain.FormDestroy(Sender: TObject);
begin
  //
  App.Free;
end;



procedure TFrmDalinMain.GetExRate;
begin
  App.Engine.ApiManager.RequestExRate;
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

end;

procedure TFrmDalinMain.SetValue;
begin
  GetExRate;
  QryTimer.Enabled := true;
  // 구독/취소 이벤트 연결..
  App.Engine.QuoteBroker.SetEvent;
end;

procedure TFrmDalinMain.Start;
begin
  App.AppStatus := asinit;
  //App.AppStatus := asSetValue;
end;

procedure TFrmDalinMain.QryTimerTimer(Sender: TObject);
begin
  if FExRate >= ( 60 * 2 ) then
  begin
    FExRate := 0;
    GetExRate;
  end;
  inc(FExRate);
  inc(FDnw);

//  App.Engine.ApiManager.CheckCount;

  stsBar.Panels[1].Text := FormatDateTime('hh:nn:ss', now) ;
end;

procedure TFrmDalinMain.SaveEnv( aStorage : TStorage );
begin
  if aStorage = nil then Exit;

  aStorage.FieldByName('Left').AsInteger := Left;
  aStorage.FieldByName('Top').AsInteger := Top;
  aStorage.FieldByName('width').AsInteger := Width;
  aStorage.FieldByName('Height').AsInteger := Height;

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

end;
end.
