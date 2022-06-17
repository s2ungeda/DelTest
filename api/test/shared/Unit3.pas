unit Unit3;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,

  Unit1
  ;

type
  TForm2 = class(TForm)
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    mt : TMmfThread;
    procedure OnNotify(const S: string);
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.FormCreate(Sender: TObject);
begin
  mt := TMmfThread.Create( OnNotify );
end;

procedure TForm2.FormDestroy(Sender: TObject);
begin
  mt.Terminate;
end;

procedure TForm2.OnNotify(const S: string);
begin
  memo1.Lines.Add( s );
end;

end.
