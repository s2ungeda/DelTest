unit FPriceTable;
interface
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.Buttons,
  Vcl.Grids, Vcl.ValEdit, Vcl.CategoryButtons, Vcl.StdCtrls

  , UStorage, USymbols
  , UApiTypes, UDistributor
  , UTypes, Vcl.Menus
  ;
const
  up1 = 11;
  up2 = 14;
  bt1 = 18;
  bt2 = 21;
  
type
  TFrmPriceTable = class(TForm)
    StatusBar1: TStatusBar;
    plLeft: TPanel;
    plLeftClient: TPanel;
    sgKimp: TStringGrid;
    PopupMenu1: TPopupMenu;
    N1: TMenuItem;
    plLeftTop: TPanel;
    Refresh: TButton;
    cbAuto: TCheckBox;
    edtSec: TLabeledEdit;
    refreshTimer: TTimer;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure sgKimpDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect;
      State: TGridDrawState);
    procedure sgKimpMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure sgKimpKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure N1Click(Sender: TObject);
    procedure refreshTimerTimer(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure cbAutoClick(Sender: TObject);
    procedure RefreshClick(Sender: TObject);

  private
    FWinParam: TWinParam;
    FSaveRow, FSaveCol,  FRow   : integer;
    FPrecision : integer;
    FFontSize: integer;
    FFontName: string;
    FTerm    : integer;
    { Private declarations }
    procedure initControls;
    procedure InitObject;
    procedure UpdateSymbol(aSymbol: TSymbol; iRow : integer);


    procedure EnableEdit( bAble : boolean ) ;

    procedure QuoteEvnet(Sender, Receiver: TObject; DataID: Integer;  DataObj: TObject; EventID: TDistributorID);
    procedure SetSymbolToGrid(sCode: string; bLoad : boolean = false );
    procedure ClearGrid;
    procedure SetData(iCol, iRow: integer; aSymbol: TSymbol);
    procedure UpdateParam(bRefresh: boolean = true);
    procedure DefaultParam;
    procedure SetTotAmtSymbol(aKind: TExchangeKind; i: integer);

  public
    { Public declarations }
    procedure SaveEnv( aStorage : TStorage );
    procedure LoadEnv( aStorage : TStorage );

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

  FRow      := -1;
  FSaveRow  := -1;
  FSaveCol  := -1;
  FPrecision:= App.GetPrecision;

  DefaultParam;
  UpdateParam( false );
end;

procedure TFrmPriceTable.DefaultParam;
begin
  FWinParam.FontName  := 'Arial';
  FWinParam.FontSize  := 11;
  FWinParam.FTerm     := StrToInt( edtSec.Text );
end;

procedure TFrmPriceTable.UpdateParam( bRefresh : boolean );
begin
  with sgKimp do
  begin
    Canvas.Font.Name := FWinParam.FontName;
    Canvas.Font.Size := FwinParam.FontSize;
    FWinParam.FTerm  := StrToInt( edtSec.Text );
    refreshTimer.Interval := FWinParam.FTerm;

    if bRefresh then
      Invalidate;
  end;
end;

procedure TFrmPriceTable.InitObject;
var
  i : TMajorSymbolKind;
  j, iRow : integer;
//  iRow : integer;
  e : TExchangeKind;
//  aSymbol : TSymbol;
begin

  for I := msBTC to High(TMajorSymbolKind) do
  begin
    FSaveRow := GetMajorRow( integer(i) );
    SetSymbolToGrid(TMajorSymbolCode[i], true );
  end;

  iRow := integer(i);
  for e := ekUpbit to High(TExchangeKind) do
    with App.Engine.ApiManager.ExManagers[e] do
      for j := 0 to High(TopAmtSymbols) do
      begin
        FSaveRow := GetMajorRow( iRow ); inc(iRow);
        if TopAmtSymbols[j] = nil then continue;
        SetSymbolToGrid(TopAmtSymbols[j].Spec.BaseCode, true );
        sgKimp.Cells[ 0, FSaveRow +1] := Format('%s%d', [ TExchangeKindShortDesc[ e ], j+1]);
      end;

  FSaveRow := -1;
end;

procedure TFrmPriceTable.cbAutoClick(Sender: TObject);
begin
  refreshTimer.Enabled  := cbAuto.Checked;
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

//    aSymbol := App.Engine.SymbolCore.FindQuoteSymbol( j, sCode );
//    if aSymbol = nil then Continue;

    aSymbol := App.Engine.SymbolCore.BaseSymbols.FindSymbolEx( sCode, j);
    if aSymbol = nil then Continue;

    iRow := FSaveRow + integer(j) ;
    sgKimp.Objects[CoinCol, iRow ] := aSymbol;

    sgKimp.Cells[ExCol,   iRow ] := TExchangeKindShortDesc[j];
    UpdateSymbol( aSymbol, iRow );

    if j = ekBinance then
      sgKimp.Objects[ExCol, iRow] := Pointer(100);

    App.Engine.QuoteBroker.Brokers[j].Subscribe(Self, aSymbol,
      App.Engine.QuoteBroker.Brokers[j].DummyEventHandler
     );
  end;

  if bLoad then
    sgKimp.Cells[CoinCol, FSaveRow] := sCode;
end;

procedure TFrmPriceTable.SetData( iCol, iRow : integer; aSymbol : TSymbol );
begin
  with sgKimp do
    case iCol of
      2 : Cells[ iCol, iRow] := Format('%.*n %%', [ FPrecision, aSymbol.KimpPrice ]);
      3 : Cells[ iCol, iRow] := Format('%.*n', [ 1, aSymbol.WDCPrice ]);
      4 : Cells[ iCol, iRow] := Format('%.0n ', [ (aSymbol.Asks[0].Price * aSymbol.Asks[0].Volume) / 1000 ]);
      7 : Cells[ iCol, iRow] := Format('%.0n ', [ (aSymbol.Bids[0].Price * aSymbol.Bids[0].Volume) / 1000 ]);
    end;
end;


procedure TFrmPriceTable.UpdateSymbol( aSymbol : TSymbol; iRow : integer );
var
  iBRow, iPre : integer;
  aUnder, aSymbol2 : TSymbol;
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
        SetData( 2, iRow+1 , aSymbol2 );
        SetData( 3, iRow+1 , aSymbol2 );
      end;

      if Objects[CoinCol, iRow+2] <> nil then
      begin
        aSymbol2 := TSymbol( Objects[CoinCol, iRow+2] );   // Upbit
        SetData( 2, iRow+2 , aSymbol2 );
        SetData( 3, iRow+2 , aSymbol2 );
      end;

      // ����, ���� �� �����Ƿ� ǥ�� ����..
//      if aSymbol is TFuture then
//      begin
//        aUnder := ( aSymbol as TFuture).Underlying;
//        if aUnder <> nil  then
//        begin
//          Cells[ CurCol - 3, iRow] := ifThenStr( aUnder.IsFuture, '��', 'X');
//          Cells[ CurCol - 2, iRow] := ifThenStr( aUnder.IsMargin, '��', 'X');
//        end;
//      end;

//      Cells[ CurCol - 3, iRow] := ifThenStr( aSymbol.IsFuture, '��', 'X');
//      Cells[ CurCol - 2, iRow] := ifThenStr( aSymbol.IsMargin, '��', 'X');

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
      //  aSymbol2 := TSymbol( Objects[CoinCol, iBRow] );   // ���̳���.
        SetData( 2, iRow , aSymbol );
        SetData( 3, iRow , aSymbol );
      end;

//      if iRow = 32 then
//        iRow := 32;

      SetData( CurCol - 4, iRow, aSymbol );
      SetData( CurCol - 1, iRow, aSymbol );
//      Cells[ CurCol - 4, iRow] := aSymbol.QtyToStr( aSymbol.Asks[0].Volume );// Format('%*.n', [ aSymbol.Spec.Precision, aSymbol.Asks[0].Volume ]);
      Cells[ CurCol - 3, iRow] := aSymbol.PriceToStr( aSymbol.Asks[0].Price ); // Format('%*.n', [ aSymbol.Spec.Precision, aSymbol.Asks[0].Price ]);
      Cells[ CurCol - 2, iRow] := aSymbol.PriceToStr( aSymbol.Bids[0].Price ); //Format('%*.n', [ aSymbol.Spec.Precision, aSymbol.Bids[0].Price ]);
//      Cells[ CurCol - 1, iRow] := aSymbol.QtyToStr( aSymbol.Bids[0].Volume );//  Format('%*.n', [ aSymbol.Spec.Precision, aSymbol.Bids[0].Volume ]);
    end;

    Cells[ CurCol + 2, iRow] := ifThenStr( aSymbol.DepositState, '��', 'X');
    Cells[ CurCol + 3, iRow] := ifThenStr( aSymbol.WithDrawlState, '��', 'X');

    Cells[ CurCol , iRow]   := aSymbol.PriceToStr( aSymbol.Last ); // Format('%*.n', [ aSymbol.Spec.Precision, aSymbol.Last ]);
    Cells[ DAyAmtCol, iRow] := Format('%.*n', [ 0, aSymbol.DayAmount ]);



    if Cells[ CurCol , iRow] = Cells[ CurCol - 3, iRow] then
      Objects[ CurCol-3, iRow] := Pointer( 100 )
    else
      Objects[ CurCol-3, iRow] := Pointer( -100 );

    if Cells[ CurCol , iRow] = Cells[ CurCol - 2, iRow] then
      Objects[ CurCol-2, iRow] := Pointer( 100 )
    else
      Objects[ CurCol-2, iRow] := Pointer( -100 );


  end;
end;

procedure TFrmPriceTable.LoadEnv(aStorage: TStorage);
var
  i : integer;
  sCode : string;
begin
  if aStorage = nil  then Exit;


  for I := 22 to sgKimp.RowCount-1 do
  begin
    sCode := aStorage.FieldByName('Coin_'+inttostr(i) ).AsString;
    if sCode <> '' then
    begin
      FSaveRow := i;
      SetSymbolToGrid( sCode, true );
    end;
  end;

  FWinParam.FontName := aStorage.FieldByName('FontName').AsStringDef( FWinParam.FontName );
  FWinParam.FontSize := aStorage.FieldByName('FontSize').AsIntegerDef( FWinParam.FontSize );

  edtSec.Text        := aStorage.FieldByName('Second' ).AsStringDef('1');
  cbAuto.Checked     := aStorage.FieldByName('Auto' ).AsBooleanDef(true);

  UpdateParam;

  FSaveRow := -1;
end;


procedure TFrmPriceTable.N1Click(Sender: TObject);
begin
  if gWinCfg = nil then
    App.CreateWinConfig;

  try

    if gWinCfg.Open(FWinParam) then
    begin
      FWinParam := gWinCfg.GetParam;
      UpdateParam ;
    end;

  finally
    if gWinCfg <> nil then
      gWinCfg.Hide;
  end;
end;

procedure TFrmPriceTable.SaveEnv(aStorage: TStorage);
var
  i : integer;
  aSymbol : TSymbol;
begin
  if aStorage = nil  then Exit;

  for I := 22 to sgKimp.RowCount-1 do
  begin
    if (i Mod 3) <> 1 then continue;
    aSymbol := TSymbol( sgKimp.Objects[CoinCol, i ]);
    if aSymbol <> nil then
      aStorage.FieldByName('Coin_'+inttostr(i) ).AsString := aSymbol.Spec.BaseCode;
  end;

  aStorage.FieldByName('FontName').AsString := FWinParam.FontName;
  aStorage.FieldByName('FontSize').AsInteger:= FWinParam.FontSize;

  aStorage.FieldByName('Second' ).AsString := edtSec.Text;
  aStorage.FieldByName('Auto' ).AsBoolean  := cbAuto.Checked;

end;


procedure TFrmPriceTable.sgKimpDrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
  var
    stTxt : string;
    aRect : TRect;
    aFont, aBack : TColor;
    dFormat : Word;
    iRow2, iVal, iMode : integer;

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
      aBack := clSkyBlue;
    end else
    begin
      //  0,1,2 ->0    3,4,5 ->1   6,7,8 -> 2
      iMode := (ARow-1) div 3;
      if iMode mod 2 <> 0 then
        aBack := GRID_MOD_COLOR;
        
			//  CurCol = 8;
      if ( ACol in [ CurCol-6..CurCol] ) then
      begin
        dFormat := DT_RIGHT  ;
        if Objects[ExCol, ARow] <> nil then
          if ACol <> CurCol then
            dFormat := DT_CENTER;
        // �ŵ�/�ż� ���̶���Ʈ
        if ACol in [CurCol-3 .. CurCol -2] then
          if Objects[ ACol, ARow] <> nil then
          begin
            if ARow = 32 then
              iVal := integer( Objects[ACol, ARow] );
            iVal := integer( Objects[ACol, ARow] );
            if iVal > 0 then begin
              if ACol = CurCol-3 then
                aBack := Short_COLOR
              else
                aBack := LONG_COLOR;
            end;
          end;


      end else
      begin

       if ACol = CurCol+1 then begin
          iRow2 := ARow-2;
          if Objects[ExCol, ARow] <> nil then
            dFormat := DT_LEFT
          else begin
            if ( ARow mod 3 ) = 0  then 
            	dFormat := DT_RIGHT;
//            else if (ARow mod 3 ) = 1 then dFomat := DT_RIGHT;

//          	if ( iRow2 > 0 ) and (( Objects[ExCol, iRow2]) <> nil ) then
//            dFormat := DT_RIGHT;
          end;
        end
        else if ACol = DayAmtCol then
          dFormat := DT_RIGHT  ;
      end;

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
    end else
    if ARow mod 3 = 0 then
    begin
      Canvas.Pen.Color := clSilver;
      Canvas.Pen.Width := 1;
      Canvas.Pen.Style := psSolid;
      Canvas.MoveTo( rect.left-1, rect.bottom-1 );
      Canvas.Lineto( rect.right, rect.bottom -1);
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

    refreshTimer.Enabled := false;

    sText := sgKimp.Cells[FSaveCol, FSaveRow];
    if sText <> '' then
      SetSymbolToGrid( sText, true )
    else if sText = '' then
      ClearGrid;

    EnableEdit(false );

    refreshTimer.Enabled := true;
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

    if (FRow > 21) and  ((( FRow-1) mod 3 ) = 0 ) and  ( aCol = CoinCol ) then
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


procedure TFrmPriceTable.SetTotAmtSymbol( aKind : TExchangeKind; i : integer );
var
  aSymbol, bSymbol : TSymbol;
    im, idx : integer;
begin

  bSymbol := App.Engine.ApiManager.ExManagers[aKind].TopAmtSymbols[i];
  if bSymbol = nil then Exit;
  idx := -1;
  case aKind of
    ekUpbit : begin
      case i of
        0 : idx := up1;
        1 : idx := up2
      end;
      im := 1;
    end;
    ekBithumb : begin
      case i of
        0 : idx := bt1;
        1 : idx := bt2;
      end;
      im := 2;
    end;
  end;

  if idx < 0 then Exit;

  aSymbol := TSymbol( sgKimp.Objects[ CoinCol, idx] );
  if aSymbol = nil then Exit;
  if aSymbol = bSymbol then Exit;

  FSaveRow := idx - im;
  SetSymbolToGrid( bSymbol.Spec.BaseCode, true );
  sgKimp.Cells[ 0, FSaveRow +1] := Format('%s%d', [ TExchangeKindShortDesc[ aKind ], i+1]);

  App.DebugLog('%s change : %s, %.0n -> %s , %.0n', [
    TExchangeKindShortDesc[aKind], aSymbol.Code, aSymbol.DayAmount,
    bSymbol.Code, bSymbol.DayAmount
    ] );

end;


procedure TFrmPriceTable.refreshTimerTimer(Sender: TObject);
var
  i, j : integer;
  aSymbol : TSymbol;
  e : TExchangeKind;
begin

  for e := ekUpbit to High(TExchangeKind) do
    with App.Engine.ApiManager.ExManagers[e] do
      for j := 0 to High(TopAmtSymbols) do
        SetTotAmtSymbol( e, j );

  for I := 1 to sgKimp.RowCount-1 do
  begin
    aSymbol := TSymbol( sgKimp.Objects[ CoinCol, i] );
    if aSymbol <> nil then
       UpdateSymbol( asymbol, i );
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

procedure TFrmPriceTable.FormActivate(Sender: TObject);
begin
  refreshTimer.Enabled := true;
end;

procedure TFrmPriceTable.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  refreshTimer.Enabled := false;
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
  // Ÿ�̸� ó��..
  if (Receiver <> Self) or (DataObj = nil) then Exit;

  aSymbol := (DataObj as TQuote).Symbol;
  iRow := sgKimp.Cols[CoinCol].IndexOfObject( aSymbol );

  if iRow <= 0 then Exit;

  UpdateSymbol( asymbol,  iRow );

end;


procedure TFrmPriceTable.RefreshClick(Sender: TObject);
var
  iTmp : integer;
begin

  iTmp :=  StrToInt( edtSec.Text );

  if iTmp < 100 then
  begin
    ShowMessage('100 ���� ������ �ȵ�');
    Exit;
  end;

  FWinParam.FTerm := iTmp;
  refreshTimer.Interval := FWinParam.FTerm;
end;

end.
