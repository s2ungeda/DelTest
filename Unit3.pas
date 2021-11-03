unit Unit3;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls ,

  ULogThread
  ;

type
  TeSaac = class(TForm)
    Button2: TButton;
    Button3: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Log : TLogThread;
  end;

var
  eSaac: TeSaac;

implementation

{$R *.dfm}

procedure TeSaac.Button1Click(Sender: TObject);
begin
  if Log <> nil then
   Log.Terminate;
end;

procedure TeSaac.Button3Click(Sender: TObject);
begin
  if Log <> nil then
    Log.LogPushQueue( 'test', 'aaaaaaaaaaaaaaaaa');

end;

procedure TeSaac.FormClose(Sender: TObject; var Action: TCloseAction);
begin

  Action := caFree;
end;

procedure TeSaac.FormCreate(Sender: TObject);
begin
  Log := TLogThread.Create;
end;

end.
