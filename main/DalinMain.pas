unit DalinMain;
interface
uses
  Winapi.Windows, Winapi.Messages
  , System.SysUtils, System.Variants, System.Classes, System.IniFiles
  , Vcl.Graphics,  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls ,

  NMainMenu,

  UTypes
  ;
type
  TFrmDalinMain = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    procedure init;
    function IsRealyClose: boolean;
    procedure SetEnv;
    procedure DalinStatusEvent( asType : TAppStatus );
  public
    { Public declarations }

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

  if not App.Engine.ApiManager.GetMaster then
    App.Log(llError, '', 'Failed PrepareMaster');
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

  App.Engine.FormBroker.Save( ComposeFilePath( [App.DataDir, App.Config.DATA_FILE] ) );


  Action := caFree;
end;


procedure TFrmDalinMain.FormDestroy(Sender: TObject);
begin
  //
  App.Free;
end;



procedure TFrmDalinMain.init;
begin
  App := TApp.Create;
end;

function  TFrmDalinMain.IsRealyClose : boolean;
var
  stLog : string;
begin
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

end.
