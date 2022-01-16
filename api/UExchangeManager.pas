unit UExchangeManager;

interface

uses
  system.Classes, system.SysUtils,

  UExchange     ,

  UApiTypes
  ;

type

  TMarketArray = array [ TExchangeMarketType ] of TExchange;

  TExchangeManaager = class
  private

    FExchanges: TMarketArray;
    function GetMarketCount: integer;
  public
    Constructor Create;
    Destructor  Destroy; override;

    //  TExchangeMarketType
    property Exchanges   : TMarketArray read FExchanges;
    property MarketCount : integer read GetMarketCount;
  end;


implementation

{ TExchangeManaager }

constructor TExchangeManaager.Create;
var
  i : TExchangeMarketType;
begin

  for I := emtSpot to High(TExchangeMarketType) do
  begin
    FExchanges[i] :=  TExchange.Create(nil);
  end;

end;

destructor TExchangeManaager.Destroy;
var
  i : TExchangeMarketType;
begin

  for I := emtSpot to High(TExchangeMarketType) do
  begin
    FExchanges[i].Free;
  end;

  inherited;
end;


function TExchangeManaager.GetMarketCount: integer;
begin
  Result := Integer(High(TExchangeMarketType));
end;

end.
