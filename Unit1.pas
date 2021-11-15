unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.ExtCtrls;

type
  TFrmPriceTable = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    plClient: TPanel;
    StatusBar1: TStatusBar;
    plTopLeft: TPanel;
    plTopClient: TPanel;
    plLeftTop: TPanel;
    plLeftClient: TPanel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmPriceTable: TFrmPriceTable;

implementation

{$R *.dfm}

end.
