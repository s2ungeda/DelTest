unit UExchangeManager;

interface

uses
  system.Classes, system.SysUtils,

  UExchange     ,

  UApiTypes
  ;

type

  TMarketArray = array [ TMarketType ] of TExchange;

  TExchangeManager = class
  private

    FExchanges: TMarketArray;
    FExchangeType: TExchangeKind;
    FExchangeIdx: integer;
    function GetMarketCount: integer;
    function CreateMarket( aMarket : TMarketType ) :  TExchange;
  public
    Constructor Create( aExType : TExchangeKind ); overload;
    Destructor  Destroy; override;

//    function RequestData: boolean; virtual; abstract;
    function PrepareMaster : boolean; virtual; abstract;

    //  TExchangeMarketType
    property Exchanges   : TMarketArray read FExchanges write FExchanges;
    property MarketCount : integer read GetMarketCount;
    property ExchangeType: TExchangeKind read FExchangeType;
    property ExchangeIdx : integer read FExchangeIdx;
  end;


implementation

uses
  UBinanceSpotNMargin, UBinanceFutures ,
  UBithSpot, UUpbitSpot
  ;


{ TExchangeManaager }

constructor TExchangeManager.Create(aExType: TExchangeKind);
var
  i : TMarketType;
begin
  FExchangeType := aExType;
  FExchangeIdx  := integer( FExchangeIdx );
  
  for I := emSpot to High(TMarketType) do
  begin
    FExchanges[i] :=  CreateMarket( i);
  end;
end;

function TExchangeManager.CreateMarket(aMarket: TMarketType): TExchange;
begin
  Result := nil;  
  case FExchangeType of
    ekBinance: 
      begin
        case aMarket of
          emSpot  : Result := TBinanceSpotNMargin.Create( self, aMarket) ;
          emFuture: Result := TBinanceFutures.Create( self, aMarket) ;
        end;
      end;                 
    ekUpbit:
      begin
        case aMarket of
          emSpot  : Result := TUpbitSpot.Create( self, aMarket) ;
        end;

      end;                 
    etBitthumb: 
      begin
        case aMarket of
          emSpot  : Result := TBithSpot.Create( self, aMarket) ;
        end;     
      end;                       
  end;
end;

destructor TExchangeManager.Destroy;
var
  i : TMarketType;
begin

  for I := emSpot to High(TMarketType) do
  begin
    if FExchanges[i] <> nil then
      FExchanges[i].Free;
  end;

  inherited;
end;


function TExchangeManager.GetMarketCount: integer;
begin
  Result := Integer(High(TMarketType));
end;

end.
