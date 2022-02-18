unit UBinanceFutures;

interface

uses
  System.Classes, System.SysUtils, System.DateUtils,

  UExchange,

  UApiTypes

  ;

type
  TBinanceFutures = class( TExchange )
  private
    function RequestFuttMaster : boolean;
    function RequestFutTicker  : boolean;
  public
    Constructor Create( aObj : TObject; aMarketType : TMarketType );
    Destructor  Destroy; override;

    function RequestMaster : boolean ; override;
  end;

implementation

uses
  GApp
  , UBinanceParse
  , REST.Types
  ;

{ TBinanceSpotNMargin }

constructor TBinanceFutures.Create(aObj: TObject; aMarketType: TMarketType);
begin
  inherited Create( aObj, aMarketType );

end;

destructor TBinanceFutures.Destroy;
begin

  inherited;
end;

function TBinanceFutures.RequestFutTicker: boolean;
var
  sOut, sJson : string;
begin
//  sTmp  := App.Engine.ApiConfig.GetBaseUrl( GetExKind , mtFutures );
//  SetBaseUrl( sTmp );

  if Request( rmGET, '/fapi/v1/ticker/24hr' , '', sJson, sOut ) then
  begin
    gBinReceiver.ParseFuttTicker(sJson);
  end else
  begin
    App.Log( llError, '', 'Failed %s ParseFuttTicker (%s, %s)',
      [ TExchangeKindDesc[GetExKind], sOut, sJson] );
    Exit( false );
  end;

  Result := App.Engine.SymbolCore.Futures[ GetExKind].Count > 0;
end;

function TBinanceFutures.RequestFuttMaster: boolean;
var
  sTmp, sOut, sJson : string;
begin
  sTmp  := App.Engine.ApiConfig.GetBaseUrl( GetExKind , mtFutures );
  SetBaseUrl( sTmp );

  if Request( rmGET, '/fapi/v1/exchangeInfo' , '', sJson, sOut ) then
  begin
    gBinReceiver.ParseFuttMaster(sJson);
  end else
  begin
    App.Log( llError, '', 'Failed %s Fut PreparMaster (%s, %s)',
      [ TExchangeKindDesc[GetExKind], sOut, sJson] );
    Exit( false );
  end;

  Result := App.Engine.SymbolCore.Futures[ GetExKind].Count > 0;

end;

function TBinanceFutures.RequestMaster: boolean;
begin
  Result := RequestFuttMaster
         and RequestFutTicker;
end;

end.
