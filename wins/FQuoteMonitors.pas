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
    procedure Button1Click(Sender: TObject);
    procedure cbUBClick(Sender: TObject);
    procedure edtAmtKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { Private declarations }
    procedure initControls;
    function CheckFilter(aSymbol: TSymbol): boolean;
    procedure UpdateData(aSymbol: TSymbol; iRow: integer);
    procedure PutData(var iCol, iRow: integer; sData: string);
  public
    { Public declarations }
    procedure RefreshData ;
    procedure SaveEnv( aStorage : TStorage );
    procedure LoadEnv( aStorage : TStorage );
  end;

var
  FrmQuoteMonitors: TFrmQuoteMonitors;

implementation

uses
  GApp   , UTableConsts
  , UConsts, UApiConsts
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

  RefreshData;
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
  sgQuote.RowCount := 1;
  App.Engine.SymbolCore.CommSymbols.SortByDailyAmount;
end;

procedure TFrmQuoteMonitors.LoadEnv(aStorage: TStorage);
begin
  if aStorage = nil  then Exit;

  edtAmt.Text := aStorage.FieldByName('Amount').AsStringDef('50');
  cbUB.Checked:= aStorage.FieldByName('UB').AsBooleanDef(true);
  cbBT.Checked:= aStorage.FieldByName('BT').AsBooleanDef(true);
end;

procedure TFrmQuoteMonitors.Button1Click(Sender: TObject);
begin
  RefreshData;
end;

procedure TFrmQuoteMonitors.cbUBClick(Sender: TObject);
begin
  RefreshData;
end;

function TFrmQuoteMonitors.CheckFilter(aSymbol : TSymbol): boolean;

  function compare( d : double ) : boolean ;
  begin
      if aSymbol.DayAmount + DOUBLE_EPSILON < d then
        Exit (false)
      else
        Exit ( true );
  end;
begin
  if ( not cbUB.Checked ) and ( not cbBT.Checked ) then
    Exit  ( true )
  else  begin
    var dAmt : double;
    dAmt  := StrToFloat( edtAmt.Text );

    if cbUB.Checked and cbBT.Checked then
      Exit ( compare( dAmt))
    else if cbBT.Checked  and ( not cbUB.Checked ) then begin
      if aSymbol.Spec.ExchangeType = ekBithumb then
        Exit ( compare( dAmt))
      else Exit (true);
    end
    else if cbUB.Checked  and ( not cbBT.Checked ) then begin
      if aSymbol.Spec.ExchangeType = ekUpbit then
        Exit ( compare( dAmt))
      else Exit (true);
    end;
  end;
end;

procedure TFrmQuoteMonitors.edtAmtKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
  var
    sText : string;
begin
  if Key = VK_RETURN then
  begin
    sText := edtAmt.Text;
    if ( sText = '' ) or ( StrToInt( sText) < 1 ) then Exit;

    refreshData;
  end;

end;

procedure TFrmQuoteMonitors.RefreshData;
var
  I: Integer;
  aSymbol : TSymbol;
  aList : TList;
begin
  InitGrid( sgQuote, true, 1 );

  //sgQuote.RowCount := App.Engine.SymbolCore.CommSymbols.Count + 1;
  aList := TList.Create;
  try
    for I := 0 to App.Engine.SymbolCore.CommSymbols.Count-1 do
    begin
      aSymbol := App.Engine.SymbolCore.CommSymbols.CommSymbols[i];
      if not CheckFilter( aSymbol ) then begin
        sgQuote.RowCount := sgQuote.RowCount -1;
        continue;
      end;
      aList.Add( aSymbol );
    end;

    sgQuote.RowCount := aList.Count + 1;

    for I := 0 to aList.Count-1 do
    begin
      aSymbol := TSymbol( aList.Items[i] );
      UpdateData( aSymbol, i+1);
    end;

    if sgQuote.RowCount > 1 then
      sgQuote.FixedRows := 1;
  finally
    aList.Free;
  end;
end;

procedure TFrmQuoteMonitors.PutData( var iCol, iRow : integer; sData : string );
begin
  sgQuote.Cells[iCol, iRow] := sData;
  inc( iCol );
end;

procedure TFrmQuoteMonitors.UpdateData( aSymbol : TSymbol; iRow : integer );
var
  iCol : integer;
  dTmp : double;
begin
  iCol := 0;
  with sgQuote do
  begin
    Objects[CoinCol, iRow]  := aSymbol;
    PutData( iCol, iRow, aSymbol.Code );
    PutData( iCol, iRow, TExchangeKindShortDesc[ aSymbol.Spec.ExchangeType ]  );
    PutData( iCol, iRow, Format('%.2f %%', [ aSymbol.KimpPrice] ) );

    PutData( iCol, iRow, aSymbol.PriceToStr( aSymbol.Asks[0].Price ) );
    PutData( iCol, iRow, aSymbol.PriceToStr( aSymbol.Bids[0].Price ) );
    PutData( iCol, iRow, aSymbol.PriceToStr( aSymbol.Last ) );

    if aSymbol.DayOpen <= 0 then  dTmp := 1
    else dTmp := aSymbol.DayOpen;

    PutData( iCol, iRow, Format('%.1f %%', [ (aSymbol.DayHigh - aSymbol.DayOpen) / dTmp * 100 ] ) );
    PutData( iCol, iRow, Format('%.1f %%', [ (aSymbol.Last    - aSymbol.DayOpen) / dTmp * 100 ] ) );
    PutData( iCol, iRow, Format('%.1f %%', [ (aSymbol.DayLow  - aSymbol.DayOpen) / dTmp * 100 ] ) );

    PutData( iCol, iRow, Format('%.*n', [ 0, aSymbol.DayAmount ]) );

  end;
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
