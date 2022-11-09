unit FSPOrder;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls,
  Vcl.Grids;

type
  TFrmSPOrder = class(TForm)
    StatusBar1: TStatusBar;
    LabeledEdit1: TLabeledEdit;
    LabeledEdit2: TLabeledEdit;
    edtCount: TLabeledEdit;
    UpDown1: TUpDown;
    sgPos: TStringGrid;
    StringGrid1: TStringGrid;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmSPOrder: TFrmSPOrder;

implementation

{$R *.dfm}

end.
