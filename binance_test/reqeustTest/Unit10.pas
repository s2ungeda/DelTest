unit Unit10;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs
  , URestThread, Vcl.StdCtrls

  ;

type
  TForm10 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FThread : TRestThread;
  public
    { Public declarations }

  end;

var
  Form10: TForm10;

implementation

{$R *.dfm}

procedure TForm10.Button1Click(Sender: TObject);
begin
  //
end;

procedure TForm10.Button2Click(Sender: TObject);
begin
//
end;

procedure TForm10.FormCreate(Sender: TObject);
begin
  FThread := nil;
end;

end.
