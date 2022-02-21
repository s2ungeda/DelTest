unit UBithSpot;

interface

uses
  System.Classes, System.SysUtils, System.DateUtils,

  System.JSON,  Rest.Json , Rest.Types ,

  UExchange, USymbols ,  UMarketSpecs,

  UApiTypes

  ;

type
  TBithSpot = class( TExchange )
  private
    function RequestTicker : boolean;
    function RequestOrderBook : boolean;
  public
    Constructor Create( aObj : TObject; aMarketType : TMarketType );
    Destructor  Destroy; override;

    function ParsePrepareMaster : integer  ; override;
    function RequestMaster : boolean ; override;
  end;

implementation

uses
  GApp   , UApiConsts
  , UBithParse
  ;

{ TBinanceSpotNMargin }

constructor TBithSpot.Create(aObj: TObject; aMarketType: TMarketType);
begin
  inherited Create( aObj, aMarketType );

end;

destructor TBithSpot.Destroy;
begin

  inherited;
end;



function TBithSpot.ParsePrepareMaster : integer;
var
  aObj, master : TJsonObject;
  aVal : TJsonValue;
  aPair : TJsonPair;
  i : Integer;

begin
  master := TJsonObject.ParseJSONValue( MasterData ) as TJsonObject;
  if master = nil then Exit;
  aObj := master.GetValue('data') as TJsonObject;

  for I := 0 to aObj.Size-1 do
  begin
    aPair := aObj.Get(i);
    if aPair.JsonValue.ClassType = TJSONObject then
    begin
      aVal  := aPair.JsonValue;
//      DoLog( Format('%d. %s : %s ', [ i, aPair.JsonString.Value, aPair.JsonValue.Value]));
      Codes.Add( aPair.JsonString.Value );
    end;
  end;
end;


function TBithSpot.RequestMaster: boolean;
begin
  Result := RequestTicker
    and RequestOrderBook
    ;
end;

function TBithSpot.RequestTicker: boolean;
var
  aObj, master : TJsonObject;
  aVal : TJsonValue;
  aPair : TJsonPair;
  i : Integer;
  sBase, sCode, sTmp : string;
  aSymbol : TSymbol;
  bNew : boolean;
begin
  master := TJsonObject.ParseJSONValue( MasterData ) as TJsonObject;
  aObj := master.GetValue('data') as TJsonObject;

  for I := 0 to aObj.Size-1 do
  begin
    aPair := aObj.Get(i);
    if aPair.JsonValue.ClassType <> TJSONObject then continue;
    sCode := aPair.JsonString.Value;
    if ( GetCodeIndex( sCode ) < 0 ) then Continue;

    aSymbol := App.Engine.SymbolCore.Symbols[GetExKind].FindCode(sCode);
    if aSymbol = nil then
    begin
      bNew := true;
      aSymbol := App.Engine.SymbolCore.RegisterSymbol(GetExKind, mtSpot, sCode );
      if aSymbol = nil then Exit (false);
    end else
      bNew := false;

    with aSymbol do
    begin
      OrgCode     := sCode+'_KRW';
      Spec.BaseCode    := sCode;
      Spec.QuoteCode   := 'KRW';
      Spec.SettleCode  := 'KRW';
    end;

    aVal  := aPair.JsonValue;

    aSymbol.DayOpen := StrToFloatDef( aVal.GetValue<string>( 'opening_price' ), 0.0 );
    aSymbol.DayHigh := StrToFloatDef( aVal.GetValue<string>( 'max_price' ), 0.0 );
    aSymbol.DayLow  := StrToFloatDef( aVal.GetValue<string>( 'min_price' ), 0.0 );
    aSymbol.Last    := StrToFloatDef( aVal.GetValue<string>( 'closing_price' ), 0.0 );
    aSymbol.DayAmount   := StrToFloatDef( aVal.GetValue<string>( 'acc_trade_value_24H' ), 0.0 );
    aSymbol.DayVolume   := StrToFloatDef( aVal.GetValue<string>( 'units_traded_24H' ), 0.0 );

//    aSymbol.Time  := UnixToDateTime(  aVal.GetValue<int64>( 'date' ) );
    aSymbol.LocalTime := now;

    if bNew then
      App.Engine.SymbolCore.RegisterSymbol( GetExKind, aSymbol );

  end;

  Result := App.Engine.SymbolCore.Spots[GetExKind].Count > 0 ;
end;

function TBithSpot.RequestOrderBook: boolean;
var
  sOut, sJson : string;
begin
  SetBaseUrl( App.Engine.ApiConfig.GetBaseUrl( GetExKind , mtSpot ) );

  if Request( rmGET, '/public/orderbook/ALL_KRW', '', sJson, sOut ) then
  begin
    gBithReceiver.ParseSpotOrderBook( sJson );
  end else
  begin
    App.Log( llError, '', 'Failed %s RequestTicker (%s, %s)',
      [ TExchangeKindDesc[GetExKind], sOut, sJson] );
    Exit( false );
  end;

  Result := true;
end;

end.
