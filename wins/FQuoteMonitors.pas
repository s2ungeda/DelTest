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
    sgQuote: TStringGrid;
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
    Timer1: TTimer;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure sgQuoteDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect;
      State: TGridDrawState);
  private
    { Private declarations }
    procedure initControls;
  public
    { Public declarations }
    procedure SaveEnv( aStorage : TStorage );
    procedure LoadEnv( aStorage : TStorage );
  end;

var
  FrmQuoteMonitors: TFrmQuoteMonitors;

implementation

uses
  GApp   , UTableConsts
  , UConsts
  , GLibs
  ;

{$R *.dfm}

{ TFrmQuoteMonitors }

procedure TFrmQuoteMonitors.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TFrmQuoteMonitors.FormCreate(Sender: TObject);
begin
  initControls;
end;

procedure TFrmQuoteMonitors.FormDestroy(Sender: TObject);
begin
  //
  App.Engine.QuoteBroker.Cancel( self );
end;

procedure TFrmQuoteMonitors.initControls;
var
  i : integer;
begin

  for I := 0 to quoteMon_TitleCnt - 1 do
  begin
    sgQuote.Cells[i,0] := quoteMon_Title[i];
    if i = 0 then
      sgQuote.RowHeights[i] := 20;
    sgQuote.ColWidths[i] :=  quoteMon_Width[i];
//    sgInOut.Cells[i,0]:= prcTbll1_Title[i];
  end;


end;

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

procedure TFrmQuoteMonitors.sgQuoteDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
  var
    stTxt : string;
    aRect : TRect;
    aFont, aBack : TColor;
    dFormat : Word;
    iMode : integer;
begin
  aFont   := clBlack;
  dFormat := DT_CENTER ;
  aRect   := Rect;
  aBack   := clWhite;

  with sgQuote do
  begin
    stTxt := Cells[ ACol, ARow];

    if ARow = 0 then
    begin
      aBack := clMoneyGreen;

    end else
    begin

    end;

    Canvas.Font.Name    := '³ª´®°íµñ';
    Canvas.Font.Size    := 10;
    Canvas.Font.Color   := aFont;
    Canvas.Brush.Color  := aBack;

//    if GetMajorRow( ARow ) > 3  then
//      stTxt := IntTostr(  GetMajorRow( ARow ) );
    aRect.Top := Rect.Top + 4;
    if ( ARow > 0 ) and ( dFormat = DT_RIGHT ) then
      aRect.Right := aRect.Right - 2;
    dFormat := dFormat or DT_VCENTER;

    Canvas.FillRect( Rect);
    DrawText( Canvas.Handle, PChar( stTxt ), Length( stTxt ), aRect, dFormat );

    if ARow = 0 then begin
      Canvas.Pen.Color := clBlack;
      Canvas.PolyLine([Point(Rect.Left,  Rect.Bottom),
                       Point(Rect.Right, Rect.Bottom),
                       Point(Rect.Right, Rect.Top)]);
      Canvas.Pen.Color := clWhite;
      Canvas.PolyLine([Point(Rect.Left,  Rect.Bottom),
                       Point(Rect.Left,  Rect.Top),
                       Point(Rect.Right, Rect.Top)]);
    end;
  end;

end;

procedure TFrmQuoteMonitors.Timer1Timer(Sender: TObject);
begin
  plExRate.Caption  := Format('%.*n', [ 2, App.Engine.ApiManager.ExRate.Value ] )
end;

end.
