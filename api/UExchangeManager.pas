unit UExchangeManager;

interface

uses
  system.Classes, system.SysUtils,

  UExchange     ,

  UApiTypes
  ;

type

  TMarketArray = array [ TMarketType ] of TExchange;

  TExchangeManaager = class
  private

    FMarkets: TMarketArray;
    function GetMarketCount: integer;
  public
    Constructor Create;
    Destructor  Destroy; override;

    //  TExchangeMarketType
    property Markets     : TMarketArray read FMarkets;
    property MarketCount : integer read GetMarketCount;
  end;


implementation

{ TExchangeManaager }

constructor TExchangeManaager.Create;
var
  i : TMarketType;
begin

  for I := emSpot to High(TMarketType) do
  begin
    FMarkets[i] :=  TExchange.Create(nil);
  end;

end;

destructor TExchangeManaager.Destroy;
var
  i : TMarketType;
begin

  for I := emSpot to High(TMarketType) do
  begin
    FMarkets[i].Free;
  end;

  inherited;
end;


function TExchangeManaager.GetMarketCount: integer;
begin
  Result := Integer(High(TMarketType));
end;

end.
