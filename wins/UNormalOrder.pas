unit UNormalOrder;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Grids, Vcl.ExtCtrls, Vcl.StdCtrls

  , UApiTypes, UApiConsts

  , USymbols, UOrders, UAccounts, UPositions

  , UDistributor

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
    procedure rbSellClick(Sender: TObject);
    procedure rbBuyClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnOrderClick(Sender: TObject);
    procedure cbExKindChange(Sender: TObject);
    procedure edtCodeKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    FExKind : TExchangeKind;
    FSymbol : TSymbol;
    FAccount: TAccount;
    procedure initControls;
    { Private declarations }
  public
    { Public declarations }
    procedure QuoteProc(Sender, Receiver: TObject; DataID: Integer;
      DataObj: TObject; EventID: TDistributorID);
    procedure TradeProc(Sender, Receiver: TObject; DataID: Integer;
      DataObj: TObject; EventID: TDistributorID);
  end;

var
  FrmNormalOrder: TFrmNormalOrder;

implementation

uses
  GApp, GLibs
  , UConsts
  ;

{$R *.dfm}

procedure TFrmNormalOrder.btnOrderClick(Sender: TObject);
var
  iSide : integer;
  dOrderQty, dPrice : double;
  aOrder  : TOrder;
begin
  if rbBuy.Checked then iSide := 1;
  if rbSell.Checked then iSide := -1;

  dOrderQty := StrToFloat( edtQty.Text );
  dPrice    := StrToFloat( edtPrice.Text );

  aOrder  := App.Engine.TradeCore.Orders[FExKind].NewOrder( FAccount, FSymbol,
    iSide, dOrderQty, pcLimit, dPrice, tmGTC );

  if aOrder <> nil then
  begin
    aOrder.ReduceOnly := cbReduce.Checked;
    App.Engine.TradeBroker.Send( aOrder );
  end;
end;

procedure TFrmNormalOrder.cbExKindChange(Sender: TObject);
begin
  FExKind := TExchangeKind( cbExKind.ItemIndex );
  FAccount  := App.Engine.TradeCore.FindAccount( FExKind );
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
    if (FSymbol <> nil ) and ( FSymbol.Spec.BaseCode = sText) then Exit;
    if sText <> '' then
    begin
      aSymbol := App.Engine.SymbolCore.BaseSymbols.FindSymbol( sText, FExKind);
      if aSymbol = FSymbol then Exit;

      if FSymbol <> nil then
        App.Engine.QuoteBroker.Brokers[FSymbol.Spec.ExchangeType].Cancel( Self, aSymbol);
      if aSymbol <> nil then
        App.Engine.QuoteBroker.Brokers[FExKind].Subscribe( Self, aSymbol, QuoteProc);

      initGrid( sgHoga, false );
      FSymbol := aSymbol;
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

  FSymbol := nil;

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


procedure TFrmNormalOrder.TradeProc(Sender, Receiver: TObject; DataID: Integer;
  DataObj: TObject; EventID: TDistributorID);
begin

end;

procedure TFrmNormalOrder.initControls;
begin
  cbExKindChange( nil );
end;

procedure TFrmNormalOrder.QuoteProc(Sender, Receiver: TObject; DataID: Integer;
  DataObj: TObject; EventID: TDistributorID);
var
  I: Integer;
begin
  if ( Receiver = Self ) or ( DataObj = FSymbol  ) then Exit;

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
end;

end.
