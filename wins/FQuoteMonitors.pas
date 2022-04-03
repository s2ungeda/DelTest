unit FQuoteMonitors;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Grids,
  Vcl.ComCtrls

  , UStorage, USymbols
  , UApiTypes, UDistributor, Vcl.Samples.Spin
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
    SpinButton1: TSpinButton;
    SpinButton2: TSpinButton;
    SpinButton3: TSpinButton;
    SpinButton4: TSpinButton;
    SpinButton5: TSpinButton;
    SpinButton6: TSpinButton;
    SpinButton7: TSpinButton;
    SpinButton8: TSpinButton;
    edtSec: TLabeledEdit;
    cbAuto: TCheckBox;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure sgQuoteDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect;
      State: TGridDrawState);
    procedure Button1Click(Sender: TObject);
    procedure cbUBClick(Sender: TObject);
    procedure edtAmtKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure SpinButton1DownClick(Sender: TObject);
    procedure SpinButton1UpClick(Sender: TObject);
  private
    FSubList: TList;
    FDataList: TList;
    { Private declarations }
    procedure initControls;
    function CheckFilter(aSymbol: TSymbol): boolean;
    procedure UpdateData(aSymbol: TSymbol; iRow: integer);
    procedure PutData(var iCol, iRow: integer; sData: string);
    procedure QuoteProc(Sender, Receiver: TObject; DataID: Integer;  DataObj: TObject; EventID: TDistributorID);
    procedure SortGrid(Grid: TStringGrid; SortCol: Integer; bAsc : boolean = true);
  public
    { Public declarations }
    procedure RefreshData ;
    procedure SimpleRefreshData ;
    procedure SaveEnv( aStorage : TStorage );
    procedure LoadEnv( aStorage : TStorage );

    property  SubList : TList read FSubList;
    property  DataList: TList read FDataList;
  end;

var
  FrmQuoteMonitors: TFrmQuoteMonitors;

implementation

uses
  GApp   , UTableConsts
  , UQuoteBroker  , USymbolUtils
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
  FDataList.Free;
  FSubList.Free;
//  App.Engine.QuoteBroker.Cancel( self );
end;

procedure TFrmQuoteMonitors.initControls;
var
  i, iSum : integer;
  iLeft : array [0..7] of integer;

  function getwith( Components : TComponent; idx : integer ) : integer;
  begin

  end;
begin

  iSum := 0;
  for I := 0 to quoteMon_TitleCnt - 1 do
  begin
    sgQuote.Cells[i,0] := quoteMon_Title[i];
    if i = 0 then
      sgQuote.RowHeights[i] := 20;
    sgQuote.ColWidths[i] :=  quoteMon_Width[i];
//    sgInOut.Cells[i,0]:= prcTbll1_Title[i];
    iSum := iSum +  quoteMon_Width[i] + 1;//(i*2);
    if i <= (high(iLeft)-1) then
      iLeft[i] := iSum;

    if i=9 then
      iLeft[7] := iSum;

  end;
  sgQuote.RowCount := 1;
  sgQuote.RowHeights[0] := 22;
  App.Engine.SymbolCore.CommSymbols.SortByDailyAmount;
  FSubList := TList.Create;
  FDataList:= TList.Create;


  for i := ComponentCount-1 downto 0 do
    if Components[i] is TSpinButton then
    begin
      case Components[i].Tag of
       0 : (Components[i] as TSpinButton).Left := iLeft[0] - SpinButton1.Width;
       1 : (Components[i] as TSpinButton).Left := iLeft[1] - SpinButton1.Width;
       2 : (Components[i] as TSpinButton).Left := iLeft[2] - SpinButton1.Width;
       3 : (Components[i] as TSpinButton).Left := iLeft[3] - SpinButton1.Width;
       4 : (Components[i] as TSpinButton).Left := iLeft[4] - SpinButton1.Width;
       5 : (Components[i] as TSpinButton).Left := iLeft[5] - SpinButton1.Width;
       6 : (Components[i] as TSpinButton).Left := iLeft[6] - SpinButton1.Width;
       7 : (Components[i] as TSpinButton).Left := iLeft[7] - SpinButton1.Width;
      end;

    end;

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
begin
//  App.Engine.QuoteBroker.Cancel( Self );
  InitGrid( sgQuote, true, 1 );
  FDataList.Clear;

  for I := 0 to App.Engine.SymbolCore.CommSymbols.Count-1 do
  begin
    aSymbol := App.Engine.SymbolCore.CommSymbols.CommSymbols[i];
    if not CheckFilter( aSymbol ) then begin
      sgQuote.RowCount := sgQuote.RowCount -1;
      continue;
    end;
    FDataList.Add( aSymbol );
  end;

  sgQuote.RowCount := FDataList.Count + 1;

  for I := 0 to FDataList.Count-1 do
  begin
    aSymbol := TSymbol( FDataList.items[i] );
    UpdateData( aSymbol, i+1);
  end;

  // 자리 배치 후 구독
//  for I := 0 to FDataList.Count-1 do
//  begin
//    aSymbol := TSymbol( FDataList.items[i] );
//    App.Engine.QuoteBroker.Brokers[ aSymbol.Spec.ExchangeType ].Subscribe( Self, aSymbol, QuoteProc);
//  end;

  if sgQuote.RowCount > 1 then
    sgQuote.FixedRows := 1;
end;

procedure TFrmQuoteMonitors.SimpleRefreshData;
var
  I: Integer;
  aSymbol : TSymbol;
begin

  InitGrid( sgQuote, false, 1 );

  for I := 0 to FDataList.Count-1 do
  begin
    aSymbol := TSymbol( FDataList.items[i] );
    UpdateData( aSymbol, i+1);
  end;

end;


procedure TFrmQuoteMonitors.PutData( var iCol, iRow : integer; sData : string );
begin
  sgQuote.Cells[iCol, iRow] := sData;
  inc( iCol );
end;

procedure TFrmQuoteMonitors.QuoteProc(Sender, Receiver: TObject;
  DataID: Integer; DataObj: TObject; EventID: TDistributorID);
  var
    aSymbol : TSymbol;
    iRow : integer;
begin
  if ( Receiver <> Self ) or ( DataObj = nil ) then Exit;

  aSymbol := (DataObj as TQuote).Symbol;
  iRow := sgQuote.Cols[CoinCol].IndexOfObject( aSymbol );

  if iRow <= 0 then Exit;

  UpdateData( asymbol,  iRow );

end;

procedure TFrmQuoteMonitors.UpdateData( aSymbol : TSymbol; iRow : integer );
var
  iCol : integer;
  dTmp : double;
  bSymbol : TSymbol;
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

    bSymbol := App.Engine.SymbolCore.BaseSymbols.FindSymbol( aSymbol.Spec.BaseCode, ekBinance);
    if bSymbol <> nil then
    begin
      PutData( iCol, iRow, ifThenStr( bSymbol.IsFuture, '○', 'X') );
      PutData( iCol, iRow, ifThenStr( bSymbol.IsMargin, '○', 'X') );
    end;

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

    Canvas.Font.Name    := '나눔고딕';
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


procedure TFrmQuoteMonitors.SortGrid(Grid: TStringGrid; SortCol: Integer; bAsc : boolean);
var

I, J: Integer;
temp: TStringList;
aSymbol, aSymbol2 : TSymbol ;
bComp : boolean;
iStr, jStr : string;
begin

  temp := TStringList.create;
  with Grid do

  for I := FixedRows to RowCount - 2 do
    for J := I + 1 to RowCount - 1 do
    begin

      if bAsc then
        bComp := CompareText(Cells[SortCol, I], Cells[SortCol, J]) > 0
      else
        bComp := CompareText(Cells[SortCol, I], Cells[SortCol, J]) < 0  ;

      if bComp then
      begin
        temp.assign(Rows[J]);

        Rows[J].assign(Rows[I]);
        Rows[I].assign(temp);
      end;
    end;
  temp.free;
end;

// 올림차순
procedure TFrmQuoteMonitors.SpinButton1DownClick(Sender: TObject);
var
  iTag : integer;
begin
  iTag := (Sender as TComponent).Tag ;

  //
  with sgQuote do
    case (Sender as TComponent).Tag of
      0 ,
      1 : SortGrid( sgQuote, iTag);     // 거래소
      2 : FDataList.Sort(CompareKimpPrice2) ;     // 김프
      3 : FDataList.Sort(CompareAskPrice2);     // 매도가
      4 : FDataList.Sort(CompareBidPrice2);     // 매수가
      5 : FDataList.Sort(CompareLast2);     // 현재가
      6 : FDataList.Sort(CompareDayUpDown2);     // 등락
      7 : FDataList.Sort(CompareDailyAmount2);     // 일거래액
    end;

  if iTag in [2..7] then
    SimpleRefreshData;
end;

// 내림차순..
procedure TFrmQuoteMonitors.SpinButton1UpClick(Sender: TObject);
var
  iTag : integer;
begin
  iTag := (Sender as TComponent).Tag ;

  //
  with sgQuote do
    case (Sender as TComponent).Tag of
      0 ,
      1 : SortGrid( sgQuote, iTag, false);     // 거래소
      2 : FDataList.Sort(CompareKimpPrice) ;     // 김프
      3 : FDataList.Sort(CompareAskPrice);     // 매도가
      4 : FDataList.Sort(CompareBidPrice);     // 매수가
      5 : FDataList.Sort(CompareLast);     // 현재가
      6 : FDataList.Sort(CompareDayUpDown);     // 등락
      7 : FDataList.Sort(CompareDailyAmount);     // 일거래액
    end;

  if iTag in [2..7] then
    SimpleRefreshData;
end;

procedure TFrmQuoteMonitors.Timer1Timer(Sender: TObject);
begin
  plExRate.Caption  := Format('%.*n', [ 2, App.Engine.ApiManager.ExRate.Value ] )
end;

end.
