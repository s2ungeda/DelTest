unit UNormalOrder;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Grids, Vcl.ExtCtrls, Vcl.StdCtrls

  , UApiTypes, UApiConsts

  , USymbols, UOrders, UAccounts, UPositions

  , UDistributor, UStorage

  ;

type
  TFrmNormalOrder = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    edtQty: TLabeledEdit;
    edtPrice: TLabeledEdit;
    cbExKind: TComboBox;
    edtCode: TEdit;
    rbBuy: TRadioButton;
    rbSell: TRadioButton;
    cbReduce: TCheckBox;
    btnOrder: TButton;
    sgHoga: TStringGrid;
    sgBal: TStringGrid;
    Button1: TButton;
    Button2: TButton;
    Timer1: TTimer;
    procedure rbSellClick(Sender: TObject);
    procedure rbBuyClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnOrderClick(Sender: TObject);
    procedure cbExKindChange(Sender: TObject);
    procedure edtCodeKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure sgHogaDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect;
      State: TGridDrawState);
    procedure sgHogaMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Button1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    FExKind : TExchangeKind;
    FSymbol : TSymbol;
    FAccount: TAccount;
    FPosition : TPosition;
    FCol, FRow : integer;
    procedure initControls;
    procedure DoPosition(aPos: TPosition);
    procedure UpdatePosition;
    { Private declarations }
  public
    { Public declarations }
    procedure QuoteProc(Sender, Receiver: TObject; DataID: Integer;
      DataObj: TObject; EventID: TDistributorID);
    procedure TradeProc(Sender, Receiver: TObject; DataID: Integer;
      DataObj: TObject; EventID: TDistributorID);

    procedure SaveEnv( aStorage : TStorage );
    procedure LoadEnv( aStorage : TStorage );
  end;

var
  FrmNormalOrder: TFrmNormalOrder;

implementation

uses
  GApp, GLibs
  , UConsts
  , UQuoteBroker
  , USymbolCore
  , Math
  ;

{$R *.dfm}

procedure TFrmNormalOrder.btnOrderClick(Sender: TObject);
var
  iSide : integer;
  dOrderQty, dPrice : double;
  aOrder  : TOrder;
begin

  if (edtQty.Text = '') or ( edtPrice.Text = '') then
  begin
    ShowMessage('필수항목 입력');
    Exit;
  end;

  if rbBuy.Checked then iSide := 1;
  if rbSell.Checked then iSide := -1;

  aOrder  := App.Engine.TradeCore.Orders[FExKind].NewOrder( FAccount, FSymbol,
    iSide, edtQty.Text , pcLimit, edtPrice.Text, tmGTC );

  if aOrder <> nil then
  begin
    aOrder.ReduceOnly := cbReduce.Checked;
    App.Engine.TradeBroker.Send( aOrder );
  end;
end;

procedure TFrmNormalOrder.Button1Click(Sender: TObject);
begin
//	App.Engine.SharedManager.RequestData(
//  	FExKind , mtSpot, rtBalance, 
//  )
	if FSymbol <> nil then
  	App.Engine.ApiManager.ExManagers[FExKind].RequestBalance( FSymbol)	;
end;

procedure TFrmNormalOrder.Button2Click(Sender: TObject);
begin
	//
  if FSymbol <> nil then  
		App.Engine.ApiManager.ExManagers[FExKind].RequestOrderList( FSymbol );
end;

procedure TFrmNormalOrder.cbExKindChange(Sender: TObject);
var
  aKind : TExchangeKind;
begin
  aKind     := TExchangeKind( cbExKind.ItemIndex );

  if aKind <> FExKind then
    FSymbol := nil;

  FExKind   := aKind;
  FAccount  := App.Engine.TradeCore.FindAccount( FExKind );
  FPosition := App.Engine.TradeCore.FindPosition( FAccount, FSymbol );

  if FPosition <> nil then
    UpdatePosition;
end;

procedure TFrmNormalOrder.edtCodeKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
  var
    sText : string;
    aSymbol : TSymbol;
begin
  if Key = VK_RETURN then
  begin
    sText := edtCode.Text;
    //if (FSymbol <> nil ) and ( FSymbol.Spec.BaseCode = sText) then Exit;
    if sText <> '' then
    begin
      aSymbol := App.Engine.SymbolCore.BaseSymbols.FindSymbol( sText, FExKind);
      if aSymbol = FSymbol then Exit;

      if FSymbol <> nil then
        App.Engine.QuoteBroker.Brokers[FSymbol.Spec.ExchangeType].Cancel( Self, aSymbol);
      if aSymbol <> nil then
        App.Engine.QuoteBroker.Brokers[FExKind].Subscribe( Self, aSymbol, QuoteProc);

      initGrid( sgHoga, false );
      FSymbol   := aSymbol;
      FPosition := App.Engine.TradeCore.FindPosition( FAccount, FSymbol );

      if FPosition <> nil then
        UpdatePosition;
    end;
  end;
end;

procedure TFrmNormalOrder.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TFrmNormalOrder.FormCreate(Sender: TObject);
begin
  initControls;

  FSymbol 	:= nil;
  FPosition	:= nil;

  App.Engine.TradeBroker.Subscribe( Self, TradeProc );
end;

procedure TFrmNormalOrder.FormDestroy(Sender: TObject);
begin
  //
  App.Engine.QuoteBroker.Cancel(Self );
  App.Engine.TradeBroker.Unsubscribe( Self );

end;

procedure TFrmNormalOrder.rbBuyClick(Sender: TObject);
begin
  btnOrder.Caption  := '매수 주문';
end;

procedure TFrmNormalOrder.rbSellClick(Sender: TObject);
begin
  btnOrder.Caption  := '매도 주문';
end;


procedure TFrmNormalOrder.SaveEnv(aStorage: TStorage);
begin

end;

procedure TFrmNormalOrder.sgHogaDrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
  var
    aFont, aBack : TColor;
    dFormat : word;
    aRect : TRect;
    stTxt : string;
begin

  aFont := clBlack;
  aBack := clWhite;
  aRect := Rect;
  dFormat := DT_RIGHT ;

  with sgHoga do
  begin
    stTxt := Cells[ACol, ARow];

    Canvas.Font.Color   := aFont;
    Canvas.Brush.Color  := aBack;

    if ACol = 1 then
      dFormat := DT_CENTER;

    aRect.Top := Rect.Top + 2;
    if ( ARow > 0 ) and ( dFormat = DT_RIGHT ) then
      aRect.Right := aRect.Right - 2;
    dFormat := dFormat or DT_VCENTER;

    Canvas.FillRect( Rect);
    DrawText( Canvas.Handle, PChar( stTxt ), Length( stTxt ), aRect, dFormat );
  end;
end;

procedure TFrmNormalOrder.sgHogaMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
  var
    aCol, aRow : integer;
    sTxt : string;
    dPrice: double;
begin
  sgHoga.MouseToCell( X, Y, aCol, aRow );

  if FSymbol = nil then Exit;

  dPrice := 0.0;
  if (aCol = 0) and ( aRow <=4) then begin
    //sTxt := sgHoga.Cells[aCol, aRow];
    dPrice  := FSymbol.Asks[aRow].Price;
  end
  else if ( aCol = 2 ) and ( aRow > 4 ) then begin
    //sTxt := sgHoga.Cells[aCol, aRow];
    dPrice  := FSymbol.Bids[aRow-5].Price;
  end;                                                 

	sTxt	:= FmtString( GetPrecision( FSymbol, dPrice ), dPrice , 1 );

  if sTxt <> '' then
    edtPrice.Text := sTxt;

end;

procedure TFrmNormalOrder.LoadEnv(aStorage: TStorage);
begin

end;       


procedure TFrmNormalOrder.initControls;
begin
  cbExKindChange( nil );

  with sgBal do
  begin
    Cells[0,0] 	:= '보유수량';
    Cells[0,1]	:= '평가금액';
    Cells[0,2]	:= '주문가능';

    Cells[2,0] 	:= '총옆가액';
    Cells[2,1]	:= '보유현금';
    Cells[2,2]	:= '코인합산';        
  end;
end;


procedure TFrmNormalOrder.Timer1Timer(Sender: TObject);
begin
	if FExKind = ekBinance then Exit;

  //
  
end;

procedure TFrmNormalOrder.TradeProc(Sender, Receiver: TObject; DataID: Integer;
  DataObj: TObject; EventID: TDistributorID);
begin
  if ( Receiver <> Self ) or ( DataObj = nil  ) then Exit;

  case integer(EventID) of
    ORDER_NEW    ,
    ORDER_ACCEPTED,
    ORDER_REJECTED, 
    ORDER_CANCELED ,
    ORDER_FILLED  :   ;


      // position Event;
    POSITION_NEW    ,
    POSITION_UPDATE , 
    POSITION_ABLEQTY : DoPosition( DataObj as TPosition )    ; 
    
  end;
end;

procedure TFrmNormalOrder.DoPosition( aPos : TPosition );

begin  
	if FPosition <> aPos then
    FPosition := aPos;

  UpdatePosition;
end;

procedure TFrmNormalOrder.UpdatePosition;
var
	dTotCoin : double;
begin

	if FPosition = nil then Exit;

  with sgBal do
  begin
  	Cells[1,0]	:= FPosition.Symbol.QtyToStr( FPosition.Volume );
    Cells[1,1]	:= Format('%.0n', [ FPosition.EntryOTE  ])  ;
    Cells[1,2]	:= Format('%.0n', [ Floor( FPosition.Account.AvailableAmt[scKRW] ) + 0.001  ]  );

    dTotCoin		:= App.Engine.TradeCore.Positions[ekBithumb].GetOpenPL( FPosition.Account );

    Cells[3,0]	:= Format('%.0n', [ Floor( dTotCoin + FPosition.Account.TradeAmt[scKRW] ) + 0.001 ] );
		Cells[3,1]	:= Format('%.0n', [ Floor( FPosition.Account.TradeAmt[scKRW] ) + 0.001  ]  );
    Cells[3,2]	:= Format('%.0n', [ Floor( dTotcoin ) + 0.001 ] );// FPosition.Symbol.QtyToStr( dTotCoin );
  end;
end;

procedure TFrmNormalOrder.QuoteProc(Sender, Receiver: TObject; DataID: Integer;
  DataObj: TObject; EventID: TDistributorID);
var
  I: Integer;
  aSymbol : TSymbol;
begin
  if ( Receiver <> Self ) or ( DataObj = nil ) then Exit;

  if (DataObj as TQuote).Symbol <> FSymbol Then Exit;  
  
  with sgHoga do
  for I := 0 to Fsymbol.Asks.Count-1 do
  begin
    Cells[1, 4-i] := FSymbol.PriceToStr( FSymbol.Asks[i].Price );
    Cells[0, 4-i] := FSymbol.QtyToStr( FSymbol.Asks[i].Volume) ;
  end;

  with sgHoga do
  for I := 0 to Fsymbol.Bids.Count-1 do
  begin
    Cells[1, 5+i] := FSymbol.PriceToStr( FSymbol.Bids[i].Price );
    Cells[2, 5+i] := FSymbol.QtyToStr( FSymbol.Bids[i].Volume) ;
  end;

  UpdatePosition;
  
end;

end.
