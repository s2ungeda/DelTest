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
  private
    FExKind : TExchangeKind;
    FSymbol : TSymbol;
    FAccount: TAccount;
    FCol, FRow : integer;
    procedure initControls;
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

procedure TFrmNormalOrder.cbExKindChange(Sender: TObject);
var
  aKind : TExchangeKind;
begin
  aKind     := TExchangeKind( cbExKind.ItemIndex );

  if aKind <> FExKind then
    FSymbol := nil;

  FExKind   := aKind;
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

  sTxt := '';
  if (aCol = 0) and ( aRow <=4) then begin
    sTxt := sgHoga.Cells[aCol, aRow];
    dPrice  := FSymbol.Asks[aRow].Price;
  end
  else if ( aCol = 2 ) and ( aRow > 4 ) then begin
    sTxt := sgHoga.Cells[aCol, aRow];
    dPrice  := FSymbol.Bids[aRow-5].Price;
  end;

  if sTxt <> '' then
    edtPrice.Text := sTxt;

end;

procedure TFrmNormalOrder.LoadEnv(aStorage: TStorage);
begin

end;



procedure TFrmNormalOrder.initControls;
begin
  cbExKindChange( nil );
end;


procedure TFrmNormalOrder.TradeProc(Sender, Receiver: TObject; DataID: Integer;
  DataObj: TObject; EventID: TDistributorID);
begin

end;


procedure TFrmNormalOrder.QuoteProc(Sender, Receiver: TObject; DataID: Integer;
  DataObj: TObject; EventID: TDistributorID);
var
  I: Integer;
begin
  if ( Receiver <> Self ) or ( DataObj = FSymbol  ) then Exit;

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
