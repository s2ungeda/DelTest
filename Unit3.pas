unit Unit3;
interface
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls ,

  ULogThread,
  UStorage
  ;
type
  TeSaac = class(TForm)
    Button2: TButton;
    Button3: TButton;
    Button1: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button3Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    procedure makeTrhead;
  public
    { Public declarations }
    Log : TLogThread;

    procedure LoadEnv( aStorage : TStorage );
    procedure SaveEnv( aStorage : TStorage );
  end;
var
  eSaac: TeSaac;
implementation
{$R *.dfm}
procedure TeSaac.Button3Click(Sender: TObject);
begin
  if Log <> nil then
    //Log.LogPushQueue( 'test', 'aaaaaaaaaaaaaaaaa');
  begin
    Log.Log('L', 'Test', 'abcedfe');
    Log.Log('C', 'Test', '%d, %s', [ 1 , FormatDateTime('YYYYMMDD',Date)]);
  end;


end;
procedure TeSaac.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;
procedure TeSaac.FormCreate(Sender: TObject);
begin
  Log := TLogThread.Create;
end;
procedure TeSaac.FormDestroy(Sender: TObject);
begin
  if Log <> nil then
   Log.Terminate;
end;



procedure TeSaac.makeTrhead;
begin
  if Log = nil then
    Log := TLogThread.Create;
end;

procedure TeSaac.SaveEnv(aStorage: TStorage);
begin
  if aStorage = nil then Exit;
  aStorage.FieldByName('test1').AsString := 'test1';
  aStorage.FieldByName('test2').AsFloat := 0.123145;
  aStorage.FieldByName('test3').AsBoolean := true;

end;

procedure TeSaac.LoadEnv(aStorage: TStorage);
begin
  if aStorage = nil then Exit;
  edit1.Text := aStorage.FieldByName('test1').AsString;
  edit2.Text := format('%.f', [ aStorage.FieldByName('test2').AsFloat ]);
  if ( aStorage.FieldByName('test3').AsBoolean = true ) then edit3.Text := 'true' else edit3.Text := 'false';
end;

end.
