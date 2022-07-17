unit UTradeBroker;

interface
uses
  System.Classes, System.SysUtils,
  UDistributor,
  USymbols, UOrders, UAccounts, UPositions, UFills,
  UApiTypes
  ;
type
  TTradeBroker = class
  private
    FDistributor: TDistributor;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Subscribe(Sender: TObject; aHandler: TDistributorEvent); overload;
    procedure Subscribe(Sender: TObject; iDataID : integer; aHandler: TDistributorEvent); overload;

    procedure Unsubscribe(Sender: TObject);

    procedure Accept( aOrder : TOrder;  sRjtCode : string ); overload;
    procedure Accept( aOrder : TOrder;  iRjtCode : integer ); overload;
    procedure Accept( aOrder : TOrder;  dtTime : TDateTime;
      bAccepted: boolean; sRjtCode : string = '' ); overload;
    procedure Cancel( aOrder : TOrder; dQty : double ); overload;
    procedure Cancel( aOrder : TOrder; bAccepted: boolean; sRjtCode : string = '' ); overload;
    procedure Fill( aOrder : TOrder; aFill : TFill );

    procedure Reject( aORder : TOrder; sCode : string );
    procedure Send( aOrder : TOrder );

    procedure PositionEvent( aPos : TPosition ;  iDataID : integer );

  end;

implementation
uses
  GApp , UTypes
  , UConsts
  ;
{ TSymbolBroker }
procedure TTradeBroker.Accept(aOrder: TOrder; iRjtCode: integer);
begin

end;

procedure TTradeBroker.Accept(aOrder: TOrder; sRjtCode: string);
begin

end;

// binance fut accept
procedure TTradeBroker.Accept(aOrder: TOrder; dtTime : TDateTime;
  bAccepted: boolean; sRjtCode : string);
var
  aPos : TPosition;

begin
  if bAccepted then
  begin
    if aOrder.IsAccept then Exit;

    aOrder.Accept( dtTime );

    aPos  := App.Engine.TradeCore.FindPosition( aOrder.Account, aOrder.Symbol );
    if aPos = nil then begin
      aPos := App.Engine.TradeCore.NewPosition( aOrder.Account, aOrder.Symbol );
      FDistributor.Distribute(Self, TRD_DATA, aPos, POSITION_NEW);
    end;

    FDistributor.Distribute(Self, TRD_DATA, aOrder, ORDER_ACCEPTED);
  end else
  begin
    aOrder.Reject(sRjtCode, dtTime);
    FDistributor.Distribute(Self, TRD_DATA, aOrder, ORDER_REJECTED);
  end;
end;

procedure TTradeBroker.Cancel(aOrder: TOrder; dQty : double);
begin
  aOrder.Cancel( aOrder.ActiveQty );
  FDistributor.Distribute(Self, TRD_DATA, aOrder, ORDER_CANCELED);
end;   

procedure TTradeBroker.Cancel(aOrder: TOrder; bAccepted: boolean;
  sRjtCode: string);
begin               
	if bAccepted then
  begin
    aOrder.Cancel( aOrder.ActiveQty);
    FDistributor.Distribute(Self, TRD_DATA, aOrder, ORDER_CANCELED);
  end else
  begin
  	aOrder.Reject( sRjtCode, now);
  end;
end;

procedure TTradeBroker.Reject(aORder: TOrder; sCode: string);
begin

end;

constructor TTradeBroker.Create;
begin
  FDistributor:= TDistributor.Create;
end;

destructor TTradeBroker.Destroy;
begin
  FDistributor.Free;
  inherited;
end;



procedure TTradeBroker.Fill(aOrder: TOrder; aFill: TFill);
var
  aPos : TPosition;
begin
  if not aOrder.IsAccept then
  begin
    Accept( aOrder, aFill.FillTime, true );
    App.Log( llInfo, 'Forward Accept Process : %s', [ aOrder.Represent ] );
  end;

  aPos := App.Engine.TradeCore.FindPosition( aOrder.Account, aOrder.Symbol);
  if aPos = nil then
  begin
    aPos  := App.Engine.TradeCore.NewPosition( aOrder.Account, aOrder.Symbol);
    FDistributor.Distribute(Self, TRD_DATA, aPos, POSITION_NEW);
  end;

  aPos.AddFill( aFill );
  aOrder.Fill( aFill );

  FDistributor.Distribute(Self, TRD_DATA, aOrder, ORDER_FILLED);
  FDistributor.Distribute(Self, TRD_DATA, aPos,   POSITION_UPDATE);
  FDistributor.Distribute(Self, TRD_DATA, aFill, FILL_NEW);

end;

procedure TTradeBroker.PositionEvent(aPos: TPosition;  iDataID: integer);
begin
	if aPos = nil then Exit;
 	FDistributor.Distribute(Self, TRD_DATA, aPos, POSITION_NEW);
end;



procedure TTradeBroker.Send(aOrder: TOrder);
begin
  App.Engine.ApiManager.ExManagers[aOrder.Account.ExchangeKind].SendOrder( aOrder );
end;


procedure TTradeBroker.Subscribe(Sender: TObject; iDataID: integer;
  aHandler: TDistributorEvent);
begin
  if Sender = nil then Exit;
  FDistributor.Subscribe(Sender, iDataID, ANY_OBJECT, ANY_EVENT, aHandler);
end;

procedure TTradeBroker.Subscribe(Sender: TObject; aHandler: TDistributorEvent);
begin
  if Sender = nil then Exit;
  FDistributor.Subscribe(Sender, TRD_DATA, ANY_OBJECT, ANY_EVENT, aHandler);
end;


procedure TTradeBroker.Unsubscribe(Sender: TObject);
begin
  FDistributor.Cancel(Sender);
end;
end.
