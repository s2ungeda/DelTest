unit FPriceTable;
interface
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.Buttons,
  Vcl.Grids, Vcl.ValEdit, Vcl.CategoryButtons, Vcl.StdCtrls

  , UStorage, USymbols
  , UApiTypes, UDistributor
  ;
type
  TFrmPriceTable = class(TForm)
    StatusBar1: TStatusBar;
    plLeft: TPanel;
    plLeftTop: TPanel;
    plLeftClient: TPanel;
    sgKimp: TStringGrid;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure sgKimpDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect;
      State: TGridDrawState);
    procedure sgKimpMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure sgKimpKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

  private
    FFontSize: integer;
    FSaveRow, FSaveCol,  FRow   : integer;

    { Private declarations }
    procedure initControls;
    procedure InitObject;
    procedure UpdateSymbol(aSymbol: TSymbol; iRow : integer);
    procedure SetFontSize(const Value: integer);

    procedure EnableEdit( bAble : boolean ) ;

    procedure QuoteEvnet(Sender, Receiver: TObject; DataID: Integer;  DataObj: TObject; EventID: TDistributorID);
    procedure SetSymbolToGrid(sCode: string; bLoad : boolean = false );
    procedure ClearGrid;

  public
    { Public declarations }
    procedure SaveEnv( aStorage : TStorage );
    procedure LoadEnv( aStorage : TStorage );

    property FontSize : integer read FFontSize  write SetFontSize;
  end;
var
  FrmPriceTable: TFrmPriceTable;
implementation
{$R *.dfm}
uses
  GApp  , GLibs
  , UTableConsts
  , UConsts , UApiConsts
  , UQuoteBroker
  ;
{ TFrmPriceTable }

procedure TFrmPriceTable.FormCreate(Sender: TObject);
begin
  initControls;
  InitObject;
end;

procedure TFrmPriceTable.initControls;
var
  i : integer;
begin

  for I := 0 to prcTbl1_TitleCnt - 1 do
  begin
    sgKimp.Cells[i,0] := prcTbll1_Title[i];
    if i = 0 then
      sgKimp.RowHeights[i] := 20;
    sgKimp.ColWidths[i] :=  prcTbll1_Width[i];
//    sgInOut.Cells[i,0]:= prcTbll1_Title[i];
  end;

  FontSize  := 10;
  FRow      := -1;
  FSaveRow  := -1;
  FSaveCol  := -1;
//  for I := 0 to prcTbl2_TitleCnt - 1 do
//  begin
//    sgQuote.Cells[i,0] := prcTbll2_Title[i];
//
//  end;

end;


procedure TFrmPriceTable.InitObject;
var
  i : TMajorSymbolKind;
//  iRow : integer;
//  j : TExchangeKind;
//  aSymbol : TSymbol;
begin
  for I := msBTC to High(TMajorSymbolKind) do
  begin
    FSaveRow := GetMajorRow( integer(i) );
    SetSymbolToGrid(TMajorSymbolCode[i], true );
  end;
  FSaveRow := -1;
end;

procedure TFrmPriceTable.ClearGrid;
var
  j : TExchangeKind;
  iRow : integer;
  aSymbol : TSymbol;
begin
  for j := ekBinance to High(TExchangeKind) do
  begin
    iRow := FSaveRow + integer(j) ;
    if sgKimp.Objects[CoinCol, iRow ] <> nil then
    begin
      aSymbol := TSymbol(sgKimp.Objects[CoinCol, iRow ]);
      App.Engine.QuoteBroker.Brokers[j].Cancel(Self, aSymbol);
    end;
    sgKimp.Objects[CoinCol, iRow ] := nil;
    sgKimp.Objects[ExCol, iRow]    := nil;
    sgKimp.Rows[iRow].Clear;
  end;
end;

procedure TFrmPriceTable.SetSymbolToGrid( sCode: string ; bLoad : boolean);
var
  j : TExchangeKind;
  iRow : integer;
  aSymbol : TSymbol;
begin

  ClearGrid;

  for j := ekBinance to High(TExchangeKind) do
  begin

    aSymbol := App.Engine.SymbolCore.FindQuoteSymbol( j, sCode );
    if aSymbol = nil then Continue;

    iRow := FSaveRow + integer(j) ;
    sgKimp.Objects[CoinCol, iRow ] := aSymbol;

    sgKimp.Cells[ExCol,   iRow ] := TExchangeKindShortDesc[j];
    UpdateSymbol( aSymbol, iRow );

    if j = ekBinance then
      sgKimp.Objects[ExCol, iRow] := Pointer(100);

    App.Engine.QuoteBroker.Brokers[j].Subscribe(Self, aSymbol, QuoteEvnet);
  end;

  if bLoad then
    sgKimp.Cells[CoinCol, FSaveRow] := sCode;
end;




procedure TFrmPriceTable.UpdateSymbol( aSymbol : TSymbol; iRow : integer );
var
  iBRow, iPre : integer;
  aSymbol2 : TSymbol;
  dTmp , dVal: double;
  dKip : array [0..1] of double;
  bMain : boolean;
begin
  if aSymbol <> nil then

  with sgKimp do
  begin
    bMain := aSymbol.Spec.ExchangeType = ekBinance ;
    dKip[0] := 0; dKip[1] := 0;
    if bMain then
    begin

      if Objects[CoinCol, iRow+1] <> nil then
      begin
        aSymbol2 := TSymbol( Objects[CoinCol, iRow+1] );   // Upbit
        dKip[0] := App.Engine.SymbolCore.CalcKimp( aSymbol, aSymbol2, -1 );
        dKip[1] := App.Engine.SymbolCore.CalcKimp( aSymbol, aSymbol2, 1 );

        Cells[ 2, iRow+1] := Format('%.2f %%', [ dKip[0] ]);
        Cells[ 3, iRow+1] := Format('%.2f %%', [ dKip[1] ]);
      end;

      if Objects[CoinCol, iRow+2] <> nil then
      begin
        aSymbol2 := TSymbol( Objects[CoinCol, iRow+2] );   // Upbit
        dKip[0] := App.Engine.SymbolCore.CalcKimp( aSymbol, aSymbol2, -1 );
        dKip[1] := App.Engine.SymbolCore.CalcKimp( aSymbol, aSymbol2, 1 );

        Cells[ 2, iRow+2] := Format('%.2f %%', [ dKip[0] ]);
        Cells[ 3, iRow+2] := Format('%.2f %%', [ dKip[1] ]);
      end;

      Cells[ CurCol - 3, iRow] := ifThenStr( aSymbol.IsFuture, '○', 'X');
      Cells[ CurCol - 2, iRow] := ifThenStr( aSymbol.IsMargin, '○', 'X');

      if aSymbol.DayOpen <= 0 then  dTmp := 1
      else dTmp := aSymbol.DayOpen;

      Cells[CurCol+1, iRow]   := Format('%.1f %%',[(aSymbol.DayHigh - aSymbol.DayOpen) / dTmp * 100 ]);
      Cells[CurCol+1, iRow+1] := Format('%.1f %%',[(aSymbol.Last    - aSymbol.DayOpen) / dTmp * 100 ]);
      Cells[CurCol+1, iRow+2] := Format('%.1f %%',[(aSymbol.DayLow  - aSymbol.DayOpen) / dTmp * 100 ]);

    end else
    begin
      iBRow := FindBinRow( iRow );

      if Objects[CoinCol, iBRow] <> nil then
      begin
        aSymbol2 := TSymbol( Objects[CoinCol, iBRow] );   // 바이낸스.

        dKip[0] := App.Engine.SymbolCore.CalcKimp( aSymbol2, aSymbol, -1 );
        dKip[1] := App.Engine.SymbolCore.CalcKimp( aSymbol2, aSymbol, 1 );

        Cells[ 2, iRow] := Format('%.2f %%', [ dKip[0] ]);
        Cells[ 3, iRow] := Format('%.2f %%', [ dKip[1] ]);
      end;

      Cells[ CurCol - 4, iRow] := Format('%*.n', [ aSymbol.Spec.Precision, aSymbol.Asks[0].Volume ]);
      Cells[ CurCol - 3, iRow] := Format('%*.n', [ aSymbol.Spec.Precision, aSymbol.Asks[0].Price ]);
      Cells[ CurCol - 2, iRow] := Format('%*.n', [ aSymbol.Spec.Precision, aSymbol.Bids[0].Price ]);
      Cells[ CurCol - 1, iRow] := Format('%*.n', [ aSymbol.Spec.Precision, aSymbol.Bids[0].Volume ]);
    end;

    Cells[ CurCol + 2, iRow] := ifThenStr( aSymbol.DepositState, '○', 'X');
    Cells[ CurCol + 3, iRow] := ifThenStr( aSymbol.WithDrawlState, '○', 'X');

    Cells[ CurCol , iRow]   := Format('%*.n', [ aSymbol.Spec.Precision, aSymbol.Last ]);
    Cells[ DAyAmtCol, iRow] := Format('%*.n', [ 0, aSymbol.DayAmount ]);

//    iBRow := FindBinRow( iRow );
//
//    if Objects[CoinCol, iBRow] <> nil then
//    begin
//      aSymbol2 := TSymbol( Objects[CoinCol, iBRow] );   // 바이낸스.
//      if aSymbol2.DayOpen <= 0 then  dTmp := 1
//      else dTmp := aSymbol2.DayOpen;
//
//      case aSymbol.Spec.ExchangeType of
//        ekBinance: dVal := (aSymbol2.DayHigh - aSymbol2.DayOpen) / dTmp * 100;
//        ekUpbit:   dVal := (aSymbol2.Last   - aSymbol2.DayOpen) / dTmp * 100;
//        ekBithumb: dVal := (aSymbol2.DayLow - aSymbol2.DayOpen) / dTmp * 100;
//      end;
//
//      Cells[CurCol+1, iRow] := Format('%.1f%%', [ dVal ]);
//
//      // 당일고가 - 전일종가 / 전일종가
//    end;
  end;
end;

procedure TFrmPriceTable.LoadEnv(aStorage: TStorage);
var
  i : integer;
  sCode : string;
begin
  if aStorage = nil  then Exit;


  for I := 10 to sgKimp.RowCount-1 do
  begin
    sCode := aStorage.FieldByName('Coin_'+inttostr(i) ).AsString;
    if sCode <> '' then
    begin
      FSaveRow := i;
      SetSymbolToGrid( sCode, true );
    end;
  end;

  FSaveRow := -1;
end;


procedure TFrmPriceTable.SaveEnv(aStorage: TStorage);
var
  i : integer;
  aSymbol : TSymbol;
begin
  if aStorage = nil  then Exit;

  for I := 10 to sgKimp.RowCount-1 do
  begin
    if (i Mod 3) <> 1 then continue;
    aSymbol := TSymbol( sgKimp.Objects[CoinCol, i ]);
    if aSymbol <> nil then
      aStorage.FieldByName('Coin_'+inttostr(i) ).AsString := aSymbol.Spec.BaseCode;
  end;

end;

procedure TFrmPriceTable.SetFontSize(const Value: integer);
begin
  FFontSize := Value;
end;

procedure TFrmPriceTable.sgKimpDrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
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

  with sgKimp do
  begin

    stTxt := Cells[ ACol, ARow];

    if ARow = 0 then
    begin
      aBack := clBtnFace;
    end else
    begin
      //  0,1,2 ->0    3,4,5 ->1   6,7,8 -> 2
      iMode := (ARow-1) div 3;
      if iMode mod 2 <> 0 then
        aBack := GRID_MOD_COLOR;

      if ( ACol in [ CurCol-6..CurCol] ) then
      begin
        dFormat := DT_RIGHT  ;
        if Objects[ExCol, ARow] <> nil then
          if ACol <> CurCol then
            dFormat := DT_CENTER;
      end;

      if ACol = DayAmtCol then
        dFormat := DT_RIGHT  ;

    end;

    Canvas.Font.Name    := '나눔고딕';
    Canvas.Font.Size    := FFontSize;
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

procedure TFrmPriceTable.sgKimpKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
  var
    sText : string;
begin
  if Key = VK_RETURN then
  begin

    sText := sgKimp.Cells[FSaveCol, FSaveRow];
    if sText <> '' then
      SetSymbolToGrid( sText, true );

    EnableEdit(false );
  end;
end;

procedure TFrmPriceTable.sgKimpMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
  var
   iMode, aCol : integer;
begin
  with (Sender as TStringGrid) do
  begin

    MouseToCell(X,y, aCol, FRow );

    if (FRow > 9) and  ((( FRow-1) mod 3 ) = 0 ) and  ( aCol = CoinCol ) then
    begin
      FSaveRow   := FRow;
      FSaveCol   := aCol;
      Options    := Options + [ goEditing, goAlwaysShowEditor] - [goRangeSelect];
    end else
    begin
      EnableEdit(false );
    end;
  end;
end;


procedure TFrmPriceTable.EnableEdit(bAble: boolean);
begin
  if bAble then
  begin

  end else
  begin
    FSaveRow    := -1;
    FSaveCol    := -1;
    with sgKimp do
    begin
      Options     := Options - [goEditing, goAlwaysShowEditor] + [goRangeSelect];
      EditorMode := false;
    end;
  end;
end;

procedure TFrmPriceTable.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TFrmPriceTable.FormDestroy(Sender: TObject);
begin
  //
  App.Engine.QuoteBroker.Cancel( Self );
end;


procedure TFrmPriceTable.QuoteEvnet(Sender, Receiver: TObject; DataID: Integer;
  DataObj: TObject; EventID: TDistributorID);
  var
    iRow :  integer;
    aSymbol : TSymbol;
begin
  if (Receiver <> Self) or (DataObj = nil) then Exit;

  aSymbol := (DataObj as TQuote).Symbol;
  iRow := sgKimp.Cols[CoinCol].IndexOfObject( aSymbol );

  if iRow <= 0 then Exit;

  UpdateSymbol( asymbol,  iRow );

end;


end.
