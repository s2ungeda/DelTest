unit FDnwStates;
interface
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Grids, Vcl.ExtCtrls,
  Vcl.ComCtrls
  , UStorage, USymbols
  , UApiTypes, UDistributor
  ;
type
  TFrmDnwStates = class(TForm)
    StatusBar1: TStatusBar;
    plLeft: TPanel;
    plLeftTop: TPanel;
    plLeftClient: TPanel;
    sgDnw: TStringGrid;
    Refresh: TButton;
    cbAuto: TCheckBox;
    edtSec: TLabeledEdit;
    refreshTimer: TTimer;
    btnSort: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure sgDnwDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect;
      State: TGridDrawState);
    procedure refreshTimerTimer(Sender: TObject);
    procedure cbAutoClick(Sender: TObject);
    procedure RefreshClick(Sender: TObject);
    procedure btnSortClick(Sender: TObject);
  private
    FFontSize: integer;
    FRow , FSaveRow , FTerm, FCount   : integer;
    FSymbols : TList;
    FPrecision : integer;
    FFontName: string;
    procedure initControls;
    procedure InitObject;

    procedure ClearGrid;
    procedure SetSymbolToGrid(sCode: string; bLoad: boolean);
//    procedure UpdateSymbol(aSymbol: TSymbol; iRow: integer); overload;
    procedure UpdateSymbol(iRow: integer; bInit : boolean = false);
    function GetData(aSymbol: TSymbol; iCol: integer; bDpst : boolean  ): string;
    function GetPriceData(aSymbol: TSymbol; iCol: integer; bDpst : boolean  ): double;
    { Private declarations }
  public
    { Public declarations }
    procedure SaveEnv( aStorage : TStorage );
    procedure LoadEnv( aStorage : TStorage );

    procedure SymbolProc(Sender, Receiver: TObject; DataID: Integer;
        DataObj: TObject; EventID: TDistributorID);

    property FontName : string read FFontName;
    property FontSize : integer read FFontSize;
  end;
var
  FrmDnwStates: TFrmDnwStates;
implementation
uses
  GApp  , GLibs  , Math
  , UTableConsts  , USymbolUtils
  , UConsts , UApiConsts
  , UQuoteBroker
  ;
{$R *.dfm}
procedure TFrmDnwStates.FormActivate(Sender: TObject);
begin
  //refreshTimer.Interval := StrToInt( edtSec.Text );
  refreshTimer.Enabled := true;
end;
procedure TFrmDnwStates.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;
procedure TFrmDnwStates.FormCreate(Sender: TObject);
begin
  initControls;
  InitObject;
  App.Engine.SymbolBroker.Subscribe( Self, DNW_EVENT, SymbolProc );
end;
procedure TFrmDnwStates.FormDestroy(Sender: TObject);
begin
  //
  App.Engine.SymbolBroker.UnSubscribe( Self );
  FSymbols.Free;
end;


procedure TFrmDnwStates.initControls;
var
  i : integer;
begin
  for I := 0 to prcTbl2_TitleCnt - 1 do
  begin
    sgDnw.Cells[i,0] := prcTbll2_Title[i];
    if i = 0 then
      sgDnw.RowHeights[i] := 20;
    sgDnw.ColWidths[i] :=  prcTbll2_Width[i];
//    sgInOut.Cells[i,0]:= prcTbll1_Title[i];
  end;

  sgDnw.Canvas.Font.Name := 'Arial';
  sgDnw.Canvas.Font.Size := 10;

  FRow      := -1;
  FCount    := 0;
  FTerm     := StrToInt( edtSec.Text );
  FPrecision:= App.GetPrecision;
//  for I := 0 to prcTbl2_TitleCnt - 1 do
//  begin
//    sgQuote.Cells[i,0] := prcTbll2_Title[i];
//
//  end;
  FSymbols := TList.Create;
end;

procedure TFrmDnwStates.InitObject;
var
  I, iRow: Integer;
  aSymbol, bSymbol : TSymbol;

  function find( base : string ) : boolean;
  var
    j : integer;
    pSymbol : TSymbol;
    bFound  : boolean;
  begin
    bFound := false;
    for j := 0 to FSymbols.Count-1 do
    begin
      pSymbol := TSymbol( FSymbols.Items[j] );
      if pSymbol.Spec.BaseCode = base then
      begin
        bFound := true;
        break;
      end;
    end;

    Result := bFound;
  end;
begin


  for I := 0 to App.Engine.SymbolCore.SymbolDnwStates[ekBinance].Count-1 do begin
    FSymbols.Add( App.Engine.SymbolCore.SymbolDnwStates[ekBinance].Symbols[i] );
  end;


  for I := 0 to App.Engine.SymbolCore.SymbolDnwStates[ekUpbit].Count-1 do
  begin
    aSymbol := App.Engine.SymbolCore.SymbolDnwStates[ekUpbit].Symbols[i];
    if not find( aSymbol.Spec.BaseCode ) then
    begin
      bSymbol := App.Engine.SymbolCore.BaseSymbols.FindSymbolEx( aSymbol.Spec.BaseCode, ekBinance);
      if bSymbol <> nil then
        FSymbols.Add(bSymbol);
    end;
  end;

  for I := 0 to App.Engine.SymbolCore.SymbolDnwStates[ekBithumb].Count-1 do
  begin
    aSymbol := App.Engine.SymbolCore.SymbolDnwStates[ekBithumb].Symbols[i];
    if not find( aSymbol.Spec.BaseCode ) then
    begin
      bSymbol := App.Engine.SymbolCore.BaseSymbols.FindSymbolEx( aSymbol.Spec.BaseCode, ekBinance);
      if bSymbol <> nil then
        FSymbols.Add(bSymbol);
    end;
  end;

  FSymbols.Sort( CompareDailyAmount2 );
  sgDnw.RowCount := 1;
  for I := 0 to FSymbols.Count-1 do
  begin
    aSymbol := TSymbol( FSymbols.Items[i] );
    if aSymbol <> nil then
      SetSymbolToGrid( aSymbol.Spec.BaseCode , true );
  end;
end;

procedure TFrmDnwStates.SetSymbolToGrid( sCode: string ; bLoad : boolean);
var
  j : TExchangeKind;
  iRow, iCol : integer;
  aSymbol : TSymbol;
  I, k: Integer;
  aList : TSymbolList;
  bDpst : boolean;
  ii: Integer;
begin

  aList := App.Engine.SymbolCore.BaseSymbols.FindSymbolList( sCode );
  if aList <> nil  then
  begin

    InsertLine( sgDnw, 1 );
    InsertLine( sgDnw, 1 );

    for k := 0 to aList.Count-1 do
    begin
      aSymbol := aList.Symbols[k];
      iCol := -1;
      case aSymbol.Spec.ExchangeType of
        ekBinance:
          begin
            if aSymbol.Spec.Market <> mtSpot then continue;
            iCol := BN_CoinCol;
          end;

        ekUpbit  : iCol := UP_CoinCol;
        ekBithumb: iCol := BT_CoinCol;
      end;

      if iCol > 0 then begin
        sgDnw.Objects[iCol, 1] := aSymbol;
        sgDnw.Objects[iCol, 2] := aSymbol;
      end;
    end;

    UpdateSymbol( 1 , true  );
  end;

end;

function TFrmDnwStates.GetData( aSymbol : TSymbol; iCol : integer; bDpst : boolean ) : string;
begin
  if aSymbol = nil then Exit ('');

  case iCol of
    0 : Result := aSymbol.Spec.BaseCode;
    2, 4, 6 : Result := ifThenStr( bDpst , ifThenStr( aSymbol.DepositState, 'O', 'X')
      , ifThenStr( aSymbol.WithDrawlState, 'O', 'X') )  ;
    3, 5, 7 : Result := ifThenStr( aSymbol.DnwTime > 100, FormatDateTime('hh:nn:ss', aSymbol.DnwTime),'');
    11 : Result := aSymbol.PriceToStr( aSymbol.Last );
    13 : Result := Format('%.*n', [ 0, aSymbol.DayAmount ]);
  end;
end;


function TFrmDnwStates.GetPriceData(aSymbol: TSymbol; iCol: integer;
  bDpst: boolean): double;
begin
  if aSymbol = nil then Exit (0.0);

  case iCol of
    9  : Result := aSymbol.KimpPrice;
    10 : REsult := aSymbol.WDCPrice;
    12 : if aSymbol.PrevClose <= 0 then
           Result := 0.0
         else
           Result := ((aSymbol.Last - aSymbol.PrevClose) / aSymbol.PrevClose ) * 100 ;

  end;
end;

procedure TFrmDnwStates.UpdateSymbol( iRow: integer; bInit : boolean);
var
  aSymbol : array [0..2] of TSymbol;
  I, j, iCol: integer;
begin

  try

  with sgDnw do
    for I := iRow to iRow+1 do
    begin
      if bInit then
      begin
        Cells[1, i] := ifThenStr( i = iRow, '입금', '출금');
        Cells[8, i] := ifThenStr( i = iRow, TExchangeKindShortDesc[ ekUpbit]
          , TExchangeKindShortDesc[ ekBithumb]);
      end;

      aSymbol[0] := TSymbol( Objects[BN_CoinCol, i] );
      aSymbol[1] := TSymbol( Objects[UP_CoinCol, i] );
      aSymbol[2] := TSymbol( Objects[BT_CoinCol, i] );

      if (i = iRow) and ( bInit ) then
          Cells[0, i] :=  GetData(aSymbol[0], 0, i = iRow);

      iCol := 2;
      Cells[iCol, i] := GetData(aSymbol[0], iCol, i = iRow);       inc(iCol);
      Cells[iCol, i] := GetData(aSymbol[0], iCol, i = iRow);       inc(iCol);
      Cells[iCol, i] := GetData(aSymbol[1], iCol, i = iRow);       inc(iCol);
      Cells[iCol, i] := GetData(aSymbol[1], iCol, i = iRow);       inc(iCol);
      Cells[iCol, i] := GetData(aSymbol[2], iCol, i = iRow);       inc(iCol);
      Cells[iCol, i] := GetData(aSymbol[2], iCol, i = iRow);       inc(iCol);

      inc(iCol);     j := ifThen( i = iRow, 1, 2 );
      Cells[iCol, i] := Format('%.*n %%', [ FPrecision,  GetPriceData(aSymbol[j], iCol, i = iRow) ]);       inc(iCol);
      Cells[iCol, i] := Format('%.1f',    [ GetPriceData(aSymbol[j], iCol, i = iRow) ]);       inc(iCol);
      Cells[iCol, i] := GetData(aSymbol[j], iCol, i = iRow);       inc(iCol) ;

      if i = iRow then
        Cells[iCol, i] := Format('%.1f %%', [ GetPriceData(aSymbol[1], iCol, i = iRow) ])
      else
        Cells[iCol, i] := Format('%.1f %%', [ GetPriceData(aSymbol[2], iCol, i = iRow) ]);

       inc(iCol);
      Cells[iCol, i] := GetData(aSymbol[j], iCol, i = iRow);       inc(iCol) ;
    end;
  except on e : exception do
    App.Log(llError, '%s, %d', [ e.Message, i ] );

  end;
end;

//procedure TFrmDnwStates.UpdateSymbol( aSymbol : TSymbol; iRow : integer );
//var
//  iBRow, iPre : integer;
//  aSymbol2 : TSymbol;
//  dTmp , dVal: double;
//  dKip : array [0..1] of double;
//  bMain : boolean;
//begin
//  if aSymbol <> nil then
//  with sgDnw do
//  begin
//    bMain := aSymbol.Spec.ExchangeType = ekBinance ;
//    dKip[0] := 0; dKip[1] := 0;
//    if bMain then
//    begin
//      if Objects[CoinCol, iRow+1] <> nil then
//      begin
//        aSymbol2 := TSymbol( Objects[CoinCol, iRow+1] );   // Upbit
//        Cells[ 2, iRow+1] := Format('%.*n %%', [ FPrecision, aSymbol2.KimpAskPrice ]);
//        Cells[ 3, iRow+1] := Format('%.*n %%', [ FPrecision, aSymbol2.KimpBidPrice ]);
//      end;
//      if Objects[CoinCol, iRow+2] <> nil then
//      begin
//        aSymbol2 := TSymbol( Objects[CoinCol, iRow+2] );   // Upbit
//        Cells[ 2, iRow+2] := Format('%.*n %%', [ FPrecision, aSymbol2.KimpAskPrice ]);
//        Cells[ 3, iRow+2] := Format('%.*n %%', [ FPrecision, aSymbol2.KimpBidPrice ]);
//      end;
//      Cells[ CurCol - 3, iRow] := ifThenStr( aSymbol.IsFuture, '○', 'X');
//      Cells[ CurCol - 2, iRow] := ifThenStr( aSymbol.IsMargin, '○', 'X');
//      if aSymbol.DayOpen <= 0 then  dTmp := 1
//      else dTmp := aSymbol.DayOpen;
//      Cells[CurCol+1, iRow]   := Format('%.1f %%',[(aSymbol.DayHigh - aSymbol.DayOpen) / dTmp * 100 ]);
//      Cells[CurCol+1, iRow+1] := Format('%.1f %%',[(aSymbol.Last    - aSymbol.DayOpen) / dTmp * 100 ]);
//      Cells[CurCol+1, iRow+2] := Format('%.1f %%',[(aSymbol.DayLow  - aSymbol.DayOpen) / dTmp * 100 ]);
//    end else
//    begin
//      iBRow := FindBinRow( iRow );
//      if Objects[CoinCol, iBRow] <> nil then
//      begin
//        aSymbol2 := TSymbol( Objects[CoinCol, iBRow] );   // 바이낸스.
//        Cells[ 2, iRow] := Format('%.*n %%', [ FPrecision, aSymbol.KimpAskPrice ]);
//        Cells[ 3, iRow] := Format('%.*n %%', [ FPrecision, aSymbol.KimpBidPrice ]);
//      end;
//      Cells[ CurCol - 4, iRow] := aSymbol.QtyToStr( aSymbol.Asks[0].Volume );// Format('%*.n', [ aSymbol.Spec.Precision, aSymbol.Asks[0].Volume ]);
//      Cells[ CurCol - 3, iRow] := aSymbol.PriceToStr( aSymbol.Asks[0].Price ); // Format('%*.n', [ aSymbol.Spec.Precision, aSymbol.Asks[0].Price ]);
//      Cells[ CurCol - 2, iRow] := aSymbol.PriceToStr( aSymbol.Bids[0].Price ); //Format('%*.n', [ aSymbol.Spec.Precision, aSymbol.Bids[0].Price ]);
//      Cells[ CurCol - 1, iRow] := aSymbol.QtyToStr( aSymbol.Bids[0].Volume );//  Format('%*.n', [ aSymbol.Spec.Precision, aSymbol.Bids[0].Volume ]);
//    end;
//    Cells[ CurCol + 2, iRow] := ifThenStr( aSymbol.DepositState, '○', 'X');
//    Cells[ CurCol + 3, iRow] := ifThenStr( aSymbol.WithDrawlState, '○', 'X');
//    Cells[ CurCol , iRow]   := aSymbol.PriceToStr( aSymbol.Last ); // Format('%*.n', [ aSymbol.Spec.Precision, aSymbol.Last ]);
//    Cells[ DAyAmtCol, iRow] := Format('%.*n', [ 0, aSymbol.DayAmount ]);
//
//  end;
//end;

procedure TFrmDnwStates.sgDnwDrawCell(Sender: TObject; ACol, ARow: Integer;
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
  with sgDnw do
  begin
    stTxt := Cells[ ACol, ARow];
    if ARow = 0 then
    begin
      aBack := clBtnFace;
    end else
    begin

      if ACol in [1..7] then
        aBack := LONG_COLOR
      else if ACol in [9..10] then
        aBack := SHORT_COLOR;//GRID_MOD_COLOR;

      if ACol in [9..13] then
        dFormat := DT_RIGHT;


      //  0,1,2 ->0    3,4,5 ->1   6,7,8 -> 2
//      iMode := (ARow-1) div 3;
//      if iMode mod 2 <> 0 then
//        aBack := GRID_MOD_COLOR;
//      if ( ACol in [ CurCol-6..CurCol] ) then
//      begin
//        dFormat := DT_RIGHT  ;
//        if Objects[ExCol, ARow] <> nil then
//          if ACol <> CurCol then
//            dFormat := DT_CENTER;
//      end else
//      begin
//        if ACol = CurCol+1 then begin
//          if Objects[ExCol, ARow] <> nil then
//            dFormat := DT_LEFT
//          else  if Objects[ExCol, ARow-2] <> nil then
//            dFormat := DT_RIGHT;
//        end
//        else if ACol = DayAmtCol then
//          dFormat := DT_RIGHT  ;
//      end;
    end;


    Canvas.Font.Color   := aFont;
    Canvas.Brush.Color  := aBack;
//    if GetMajorRow( ARow ) > 3  then
//      stTxt := IntTostr(  GetMajorRow( ARow ) );
    aRect.Top := Rect.Top + 2;
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
    end
    else if ARow > 1 then
    begin
     if (ARow mod 2 = 0) then begin

        Canvas.Pen.Color := clSilver;
        Canvas.Pen.Width := 1;
        Canvas.Pen.Style := psSolid;
        Canvas.MoveTo( rect.left-1, rect.bottom );
        Canvas.Lineto( rect.right, rect.bottom );

      end;
    end;
  end;
end;
procedure TFrmDnwStates.SymbolProc(Sender, Receiver: TObject; DataID: Integer;
  DataObj: TObject; EventID: TDistributorID);
  var
    aSymbol, pSymbol : TSymbol;
    iRow , iCol, i, j: integer;
begin
  if ( Receiver <> Self ) or ( DataObj = nil ) then Exit;
  aSymbol := DataObj as TSymbol;

  iCol := -1;
  case aSymbol.Spec.ExchangeType of
    ekBinance : iCol := BN_CoinCol;
    ekUpbit   : iCol := UP_CoinCol;
    ekBithumb : iCol := BT_CoinCol;
  end;

  if iCol <= 0 then Exit;

  iRow  := sgDnw.Cols[iCol].IndexOfObject( aSymbol );

  if iRow < 0  then begin
    if cbAuto.Checked then refreshTimer.Enabled := false;
    SetSymbolToGrid( aSymbol.Spec.BaseCode , true );
    if cbAuto.Checked then refreshTimer.Enabled := true;
  end
  else begin
    if (iRow mod 2 ) = 0 then
      dec( iRow );
    UpdateSymbol( iRow );
  end;

//  if iRow >= 0 then Exit;
//  try
//    if cbAuto.Checked then
//      refreshTimer.Enabled := false;
//    // 젤 위에 추가..
//    InsertLine( sgDnw, 1 );
//    InsertLine( sgDnw, 1 );
//    InsertLine( sgDnw, 1 );
//    FSaveRow := 1;
//    SetSymbolToGrid(  aSymbol.Spec.BaseCode, true );
//  finally
//    if cbAuto.Checked then
//      refreshTimer.Enabled := true;
//  end;
end;




procedure TFrmDnwStates.btnSortClick(Sender: TObject);
begin
  if cbAuto.Checked then
    refreshTimer.Enabled := false;

  FSymbols.Clear;
  InitGrid( sgDnw, true, 1 );
  InitObject;

//  FSymbols.Sort( CompareDailyAmount );
//  RefreshClick( nil );

  refreshTimer.Enabled := cbAuto.Checked;
end;

procedure TFrmDnwStates.cbAutoClick(Sender: TObject);
begin
  FTerm := StrToInt( edtSec.Text );
  refreshTimer.Enabled := cbAuto.Checked;
  FCount  := 0;
end;
procedure TFrmDnwStates.ClearGrid;
var
  j : TExchangeKind;
  iRow : integer;
  aSymbol : TSymbol;
begin
  for j := ekBinance to High(TExchangeKind) do
  begin
    iRow := FSaveRow + integer(j) ;
    if sgDnw.Objects[CoinCol, iRow ] <> nil then
    begin
 //     aSymbol := TSymbol(sgDnw.Objects[CoinCol, iRow ]);
 //     App.Engine.QuoteBroker.Brokers[j].Cancel(Self, aSymbol);
    end;
    sgDnw.Objects[CoinCol, iRow ] := nil;
    sgDnw.Objects[ExCol, iRow]    := nil;
    sgDnw.Rows[iRow].Clear;
  end;
end;
procedure TFrmDnwStates.LoadEnv(aStorage: TStorage);
begin
  if aStorage = nil  then Exit;
  edtSec.Text     := aStorage.FieldByName('Second' ).AsStringDef('10');
  cbAuto.Checked  := aStorage.FieldByName('Auto' ).AsBooleanDef(true);
end;

procedure TFrmDnwStates.RefreshClick(Sender: TObject);
var
  I: Integer;
  aSymbol : TSymbol;
begin
  for I := 1 to sgDnw.RowCount-1 do
  begin
    if (i Mod 2) > 0 then
      UpdateSymbol( i );

//    aSymbol := TSymbol( sgDnw.Objects[CoinCol, i] );
//    if aSymbol <> nil then
//      UpdateSymbol( aSymbol, i );
  end;
end;
procedure TFrmDnwStates.refreshTimerTimer(Sender: TObject);
begin
  if FCount >= FTerm then
  begin
    RefreshClick( nil );
    FCount := 0;
  end;
  inc(FCount);
end;
procedure TFrmDnwStates.SaveEnv(aStorage: TStorage);
begin
  if aStorage = nil  then Exit;
  aStorage.FieldByName('Second' ).AsString := edtSec.Text;
  aStorage.FieldByName('Auto' ).AsBoolean  := cbAuto.Checked;
end;

end.
