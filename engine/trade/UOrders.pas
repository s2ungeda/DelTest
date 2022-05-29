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

  TOrder = class(TCollectionItem)
  private
    FSymbol: TSymbol;
    FActiveQty: double;
    FGroupNo: string;
    FTimeToMarket: TTimeInForce;
    FLocalNo: string;
    FPrice: Double;
    FEventTime: TDateTime;
    FState: TOrderState;
    FTradeTime: TDateTime;
    FFills: TFillList;
    FOrderNo: string;
    FCanceledQty: double;
    FPriceControl: TPriceControl;
    FSide: Integer;
    FOrderQty: double;
    FSentTime: TDateTime;
    FAvgPrice: double;
    FFilledPrice: Double;
    FAcptTime: TDateTime;
    FFilledQty: double;
    FAccount: TAccount;
    FRejectCode: string;

  public
    constructor Create(Coll: TCollection); override;
    destructor Destroy; override;

    property Account: TAccount read FAccount;
    property Symbol: TSymbol read FSymbol;    

    property TimeToMarket: TTimeInForce read FTimeToMarket;
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
  end;
  
  TOrderList = class(TList)
  private
    function GetOrder(i: Integer): TOrder;
  public

    function FindOrder(sOrderNo: string): TOrder; overload;
    procedure SortByAcptTime;
    
    property Orders[i:Integer]: TOrder read GetOrder; default;
  end;
  
implementation

{ TOrder }

constructor TOrder.Create(Coll: TCollection);
begin
  inherited;

end;

destructor TOrder.Destroy;
begin

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

end.
