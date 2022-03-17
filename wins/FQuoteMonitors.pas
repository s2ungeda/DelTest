unit FQuoteMonitors;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Grids,
  Vcl.ComCtrls

  , UStorage, USymbols
  , UApiTypes, UDistributor
  ;

type
  TFrmQuoteMonitors = class(TForm)
    StatusBar1: TStatusBar;
    plLeft: TPanel;
    plLeftTop: TPanel;
    plLeftClient: TPanel;
    sgKimp: TStringGrid;
    cbOSEx: TComboBox;
    cbKREx1: TComboBox;
    cbKREx2: TComboBox;
    plExRate: TPanel;
    Panel2: TPanel;
    Panel1: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    cbUB: TCheckBox;
    cbBT: TCheckBox;
    Label1: TLabel;
    Button1: TButton;
    edtAmt: TEdit;
    Label2: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
    procedure SaveEnv( aStorage : TStorage );
    procedure LoadEnv( aStorage : TStorage );
  end;

var
  FrmQuoteMonitors: TFrmQuoteMonitors;

implementation

{$R *.dfm}

{ TFrmQuoteMonitors }

procedure TFrmQuoteMonitors.LoadEnv(aStorage: TStorage);
begin
  if aStorage = nil  then Exit;

  edtAmt.Text := aStorage.FieldByName('Amount').AsStringDef('50');
  cbUB.Checked:= aStorage.FieldByName('UB').AsBooleanDef(true);
  cbBT.Checked:= aStorage.FieldByName('BT').AsBooleanDef(true);
end;

procedure TFrmQuoteMonitors.SaveEnv(aStorage: TStorage);
begin
  if aStorage = nil  then Exit;

  aStorage.FieldByName('Amount').AsString := edtAmt.Text;
  aStorage.FieldByName('UB').AsBoolean := cbUB.Checked;
  aStorage.FieldByName('BT').AsBoolean := cbBT.Checked;
end;

end.
