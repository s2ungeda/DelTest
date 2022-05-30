unit UOrders;

interface

uses
	System.Classes, System.SysUtils,

  UAccounts, Usymbols,  UFills, 

  UApiTypes, UTypes

  ;

type

  //------------------------------ order
  TOrderType 		= (otNormal, otChange, otCancel);  
  TPriceControl = (pcLimit, pcMarket );  
  TTimeInForce 	= (tmGTC, tmFOK, tmIOC, tmPOST);  

  TOrderState = (osReady,
                   // with server
                 osSent, osSrvAcpt, osSrvRjt,
                   // with exchange
                 osActive, osRejected, osFilled, osCanceled,
                 osConfirmed, osFailed);

  TOrderStates  = set of TOrderState;

  TOrder	= class;

  TOrderEvent = procedure(Order: TOrder) of object;

  TOrder = class(TCollectionItem)
  private
    FSymbol: TSymbol;
    FAccount: TAccount;
    
    FExchangeKind : TExchangeKind;    
    FTimeInForce: TTimeInForce;
    FState: TOrderState;
    FPriceControl: TPriceControl;
    
    FLocalNo: string;
    FGroupNo: string;
    FOrderNo: string;
    FPrice: Double;    
    
    FFills: TFillList;

		FOrderQty: double;    
    FCanceledQty: double;
    FFilledQty: double;
    FActiveQty: double;
    
    FSide: Integer;     
    FAvgPrice: double;
    FFilledPrice: Double;
    
    FRejectCode: string;

    FAcptTime: TDateTime;    
    FSentTime: TDateTime;
    FEventTime: TDateTime;
    FTradeTime: TDateTime;
        
    FOnStateChanged: TOrderEvent;
    FTarget: TOrder;
    FOrderType: TOrderType;

  public
    constructor Create(Coll: TCollection); override;
    destructor Destroy; override;

    property Account: TAccount read FAccount;
    property Symbol: TSymbol read FSymbol;    
    property Target: TOrder read FTarget;
    property ExchangeKind : TExchangeKind read FExchangeKind;

    property OrderType: TOrderType read FOrderType write FOrderType;
    property TimeInForce: TTimeInForce read FTimeInForce;
    property PriceControl: TPriceControl read FPriceControl;

    property Side: Integer read FSide;
    property OrderQty: double read FOrderQty Write FOrderQty;
    property Price: Double read FPrice;
    property FilledPrice: Double read FFilledPrice;                 
    property AvgPrice : double read FAvgPrice write FAvgPrice;   

		property ActiveQty: double read FActiveQty write FActiveQty;
    property FilledQty: double read FFilledQty write FFilledQty;
    property CanceledQty: double read FCanceledQty  write FCanceledQty;     

    property OrderNo: string read FOrderNo write FOrderNo;
    property LocalNo: string read FLocalNo write FLocalNo;
    property GroupNo: string read FGroupNo write FGroupNo;             

    property State: TOrderState read FState write FState;          
    property RejectCode: string read FRejectCode write FRejectCode;     
    property Fills  : TFillList  read FFills;

    property SentTime	: TDateTime read FSentTime;
    property AcptTime	: TDateTime read FAcptTime write FAcptTime;    
    property TradeTime: TDateTime read FTradeTime write FTradeTime;
    property EventTime: TDateTime read FEventTime write FEventTime;      

    property OnStateChanged: TOrderEvent read FOnStateChanged write FOnStateChanged;   
  end;
  
  TOrderList = class(TList)
  private
    function GetOrder(i: Integer): TOrder;
  public

    function FindOrder(sOrderNo: string): TOrder; overload;
    procedure SortByAcptTime;
    
    property Orders[i:Integer]: TOrder read GetOrder; default;
  end;


  TOrders = class(TCollection)
  private
      // list
    FNewOrders: TOrderList;
    FActiveOrders: TOrderList;
    FClosedOrders: TOrderList;
    FRjtOrders: TOrderList;
      // events
    FOnNew: TOrderEvent;

    function GetOrder(i: Integer): TOrder;
    procedure OrderStateChanged(aOrder: TOrder);
    function GetGroupID: integer;
    function New: TOrder;
  public

    constructor Create;
    destructor Destroy; override;

    function NewOrder( aAccount: TAccount; aSymbol: TSymbol; iSide : integer; dOrderQty: double;
      pcValue: TPriceControl; dPrice: Double; tmValue: TTimeInForce  ): TOrder; overload;
    
    function NewCancelOrder(aTarget: TOrder; iCancelQty: Integer ): TOrder;  
    function Represent: String;    
    
      // search
    function Find(aAccount: TAccount; aSymbol: TSymbol; OrderNo: string): TOrder; overload;
    function Find(OrderNo: string): TOrder; overload;

      //
    property Orders[i: Integer]: TOrder read GetOrder; default;
    property NewOrders: TOrderList read FNewOrders;
    property ActiveOrders: TOrderList read FActiveOrders;
    property ClosedOrders: TOrderList read FClosedOrders;
    property RjtOrders: TOrderList read FRjtOrders;         
      //
    property OnNew: TOrderEvent read FOnNew write FOnNew;

  end;  

  
  
implementation

uses
	GLibs
  , GApp
  ;

{ TOrder }

constructor TOrder.Create(Coll: TCollection);
begin
  inherited Create(Coll);

  FSymbol:= nil;
  FAccount:= nil; 
  FTarget	:= nil;
  FOnStateChanged := nil;

  FState:= osReady;

  FFills:= TFillList.Create;    

  FOrderQty:= 0.0;
  FCanceledQty:= 0.0;
  FActiveQty:= 0.0;
  FFilledQty:= 0.0;

  FSide:= 0;
  
  FSentTime:= 0;
  FTradeTime:= 0.0;
  FEventTime:= 0.0;
  FAcptTime:= 0;
    
  FPrice:= 0.0;
  FAvgPrice:= 0.0;
  FFilledPrice:= 0.0;
end;

destructor TOrder.Destroy;
begin
  FFills.Free;
  inherited;
end;



{ TOrderList }

function TOrderList.FindOrder(sOrderNo: string): TOrder;
var
  i: Integer;
  aOrder: TOrder;
begin
  Result := nil;

  for i := 0 to Count - 1 do
  begin
    aOrder := TOrder(Items[i]);
    if aOrder.OrderNo = sOrderNo then
    begin
      Result := aOrder;
      Break;
    end;
  end;

end;

function TOrderList.GetOrder(i: Integer): TOrder;
begin
  if (i >= 0) and (i <= Count-1) then
    Result := TOrder(Items[i])
  else
    Result := nil;
end;

function CompareAcptTime(Data1, Data2: Pointer): Integer;
var
  Order1: TOrder absolute Data1;
  Order2: TOrder absolute Data2;
begin
  if Order1.AcptTime > Order2.AcptTime then
    Result := 1
  else if Order1.AcptTime < Order2.AcptTime then
    Result := -1
  else
    Result := 0;
end;

procedure TOrderList.SortByAcptTime;
begin
  Sort(CompareAcptTime);
end;

{ TOrders }

constructor TOrders.Create;
begin
  inherited Create(TOrder);

  FNewOrders := TOrderList.Create;
  FActiveOrders := TOrderList.Create;
  FClosedOrders := TOrderList.Create;
  FRjtOrders := TOrderList.Create;
end;

destructor TOrders.Destroy;
begin
  FNewOrders.Free;
  FActiveOrders.Free;
  FClosedOrders.Free;
  FRjtOrders.Free;
  
  inherited;
end;

function TOrders.Find(aAccount: TAccount; aSymbol: TSymbol;
  OrderNo: string): TOrder;
var
  i: Integer;
  aOrder: TOrder;
  stLog : string;
begin
  Result := nil;
    // the search should be backward (don't change this order)
  for i := Count-1 downto 0 do
  begin
    aOrder := GetOrder(i);

    if (aOrder.Account = aAccount)
       and (aOrder.Symbol = aSymbol)
       and (aOrder.OrderNo = OrderNo) then
    begin
      Result := Items[i] as TOrder;
      Break;
    end;
  end;

end;

function TOrders.Find(OrderNo: string): TOrder;
var
  i: Integer;
  aOrder: TOrder;
begin
  Result := nil;

  for i := 0 to Count - 1 do
  begin
    aOrder := TOrder(Items[i]);
    if  (aOrder.OrderNo = OrderNo) then
    begin
      Result := aOrder;
      Break;
    end;
  end;

end;

function TOrders.GetGroupID: integer;
begin

end;

function TOrders.GetOrder(i: Integer): TOrder;
begin
  if (i >= 0) and (i <= Count-1) then
    Result := Items[i] as TOrder
  else
    Result := nil;
end;

function TOrders.NewCancelOrder(aTarget: TOrder; iCancelQty: Integer): TOrder;
begin

end;

function TOrders.New: TOrder;
begin
  Result := Add as TOrder;
  Result.FOnStateChanged := OrderStateChanged;
end;

function TOrders.NewOrder(aAccount: TAccount; aSymbol: TSymbol;  iSide : integer;
  dOrderQty: double; pcValue: TPriceControl; dPrice: Double;
  tmValue: TTimeInForce): TOrder;
begin
  Result := nil;

  if (aAccount = nil) or (aSymbol = nil) or ( CheckZero(dOrderQty)) then Exit;

//  if not App.Engine.TradeCore.Positions.CheckOrderLimit( aAccount, aSymbol, iOrderQty, dPrice ) then
//    Exit;

  Result := New;           

  Result.FAccount := aAccount;
  Result.FSymbol := aSymbol;

  Result.FTarget := nil;

  Result.FOrderType := otNormal;
  Result.FPriceControl := pcValue;
  Result.FTimeInForce := tmValue;

  Result.FOrderQty := Abs(dOrderQty);
  Result.FSide :=  iSide;
  Result.FPrice := dPrice;    

  Result.FOrderNo := '';

  Result.FActiveQty := 0;
  Result.FFilledQty := 0;
  Result.FCanceledQty := 0;     

  Result.FRejectCode := '';

  Result.FAcptTime := 0;   
    // add to the queue
  FNewOrders.Add(Result);    
   
    // notify 화면에서 필요..
  if Assigned(FOnNew) then
    FOnNew(Result);

end;

procedure TOrders.OrderStateChanged(aOrder: TOrder);
var index : integer;
begin
  if aOrder = nil then Exit;

    // remove from the old queue
  case aOrder.State of
    osReady: ; // irrelevant
    osSrvAcpt: ; // keep in the same queue
    osSrvRjt, osActive: FNewOrders.Remove(aOrder);
    osRejected:
      begin
        FNewOrders.Remove(aOrder);
        FActiveOrders.Remove(aOrder);        
      end;
    osFilled, osCanceled, osConfirmed, osFailed:
      begin
        FNewOrders.Remove(aOrder);
        FActiveOrders.Remove(aOrder);       
      end;
  end;

    // add to the new queue
  case aOrder.State of
    osActive:
      begin
         FActiveOrders.Add(aOrder);

      end;
    {osFilled,} osCanceled, osConfirmed: FClosedOrders.Add(aOrder);
    osSrvRjt, osRejected, osFailed: begin
      FRjtOrders.Add(aOrder);
      FClosedOrders.Add(aOrder);   
    end;
  end;
end;

function TOrders.Represent: String;
begin

end;

end.
