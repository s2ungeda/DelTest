unit DalinMain;
interface
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls ,

  GApp,
  NMainMenu
  ;
type
  TFrmMain = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);

    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    procedure init;
  public
    { Public declarations }

  end;
var
  FrmMain: TFrmMain;
implementation
{$R *.dfm}


procedure TFrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;
procedure TFrmMain.FormCreate(Sender: TObject);
begin
  //
  init;
end;
procedure TFrmMain.FormDestroy(Sender: TObject);
begin
  //
  App.Free;
end;



procedure TFrmMain.init;
begin
  App := TApp.Create;
end;

end.
