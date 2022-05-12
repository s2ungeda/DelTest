unit UBinanceSpotNMargin;

interface

uses
  System.Classes, System.SysUtils, System.DateUtils,

  System.JSON,  Rest.Json ,   Rest.Types,

  UExchange, USymbols ,  UMarketSpecs,

  UApiTypes

  ;

type
  TBinanceSpotNMargin = class( TExchange )
  private

    function RequestSpotMaster : boolean ;
    function RequestSpotTicker : boolean ;
    function RequestMarginMaster : boolean;
    function RequestMarginTier : boolean;
    procedure ReceiveDNWState;
    function RequestDNWStateSync : boolean;
  public
    Constructor Create( aObj : TObject; aMarketType : TMarketType );
    Destructor  Destroy; override;

    function ParsePrepareMaster : integer; override;
    function RequestMaster : boolean ; override;
    function RequestDNWState : boolean; override;

  end;

implementation

uses
  GApp, GLibs  , UApiConsts
  , UBinanceParse
  , UFQN
  , UEncrypts
  ;

{ TBinanceSpotNMargin }

constructor TBinanceSpotNMargin.Create(aObj: TObject; aMarketType: TMarketType);
begin
  inherited Create( aObj, aMarketType );


end;

destructor TBinanceSpotNMargin.Destroy;
begin

  inherited;
end;


function TBinanceSpotNMargin.ParsePrepareMaster : integer;
var
  master : TJsonObject;
  aObj  : TJsonObject;
  aArr  : TJsonArray;
  aPair : TJSONPair;
  I: Integer;
begin

  master := TJsonObject.ParseJSONValue( MasterData ) as TJsonObject;
  if master = nil then Exit;
  aPair := master.Get('symbols');
  if aPair = nil then Exit;

  aArr  := aPair.JsonValue as TJsonArray;

  var sTmp : string;

  for I := 0 to aArr.Size-1 do
  begin
    aObj  := aArr.Get(i) as TJsonObject;
    sTmp  := aObj.GetValue('quoteAsset').Value;

    if sTmp = 'USDT' then
      Codes.Add( aObj.GetValue('baseAsset').Value );
  end;

end;

function TBinanceSpotNMargin.RequestMaster: boolean;
begin
  Result := RequestSpotMaster          
         and RequestMarginMaster
         and RequestSpotTicker
         and RequestDNWStateSync
         ;
end;

// 저장해놓은 마스터 string 를 다시 파싱하면 됨.
function TBinanceSpotNMargin.RequestSpotMaster: boolean;
var
  master : TJsonObject;
  aObj, aFil  : TJsonObject;
  aArr, aArr2  : TJsonArray;
  aPair : TJSONPair;
  I, j: Integer;
  aSymbol : TSymbol;
  aSpec   : TMarketSpec;
begin

  master := TJsonObject.ParseJSONValue( MasterData ) as TJsonObject;
  aArr   := master.Get('symbols').JsonValue as TJsonArray;

  var sTmp : string;

  for I := 0 to aArr.Size-1 do
  begin
    aObj  := aArr.Get(i) as TJsonObject;
    sTmp  := aObj.GetValue('quoteAsset').Value;

    if sTmp = 'USDT' then
    begin

      var sBase, sCode : string;
      var bNew : boolean;

      sBase := aObj.GetValue('baseAsset').Value;
      if ( GetCodeIndex( sBase ) >= 0 ) then
      begin
        // BTCUSDT 형식.
        sCode := aObj.GetValue('symbol').Value;
        aSymbol := App.Engine.SymbolCore.Symbols[GetExKind].FindCode(sCode);

        if aSymbol = nil then
        begin
          bNew := true;
          aSymbol := App.Engine.SymbolCore.RegisterSymbol(GetExKind, mtSpot, sCode );
          if aSymbol = nil then Exit (false);

        end else
          bNew :=false;

        with aSymbol do
        begin
          OrgCode     := lowercase( sCode );
          Spec.BaseCode    := sBase;
          Spec.QuoteCode   := sTmp;
          Spec.SettleCode  := sTmp;
        end;

        sTmp := aObj.GetValue('status').Value;
        if sTmp = 'TRADING' then aSymbol.TradeAble := true  else aSymbol.TradeAble := false;

        aArr2 := aObj.Get('filters').JsonValue as TJsonArray;

        var aFiType : string;
        var dSize, dSize2 : double;
        var iPre : integer;

        for j := 0 to aArr2.Size-1 do
        begin
          aFil  := aArr2.Get(j) as TJsonObject;
          aFiType := aFil.GetValue('filterType').Value;
          if aFiType = 'PRICE_FILTER' then
          begin
            sTmp := aFil.GetValue('tickSize').Value;
            //sTmp := aFil.GetValue('minPrice').Value;
            dSize := StrToFloatDef( sTmp, 1.0 );
            iPre  := GetPrecision( sTmp );
          end else
          if aFiType = 'LOT_SIZE' then
          begin
            //sTmp := aFil.GetValue('minQty').Value;
            sTmp := aFil.GetValue('stepSize').Value;
            dSize2  := StrToFloatDef( sTmp, 1.0);
          end;
        end;

        aSymbol.Spec.SetSpec( iPre, dSize, dSize2 );

        if bNew then
          App.Engine.SymbolCore.RegisterSymbol( GetExKind, aSymbol );   
      end;
    end;
  end;

  Result := true;

end;             


function TBinanceSpotNMargin.RequestSpotTicker: boolean;
var
  data, sig   : string;
  sOut, sJson : string;
begin

  if Request( rmGET, '/api/v3/ticker/24hr', '', sJson, sOut ) then
  begin
//    App.Log( llDebug, '', '%s (%s, %s)', [ TExchangeKindDesc[GetExKind], sOut, sJson] );
    gBinReceiver.ParseSpotTicker( sJson );
  end else
  begin
    App.Log( llError, '', 'Failed %s RequestMarginMaster (%s, %s)',
      [ TExchangeKindDesc[GetExKind], sOut, sJson] );
    Exit( false );
  end;

  Result := true;
end;


procedure TBinanceSpotNMargin.ReceiveDNWState;
begin
  gBinReceiver.ParseDNWState( RestReq.Response.Content );
end;

function TBinanceSpotNMargin.RequestDNWState : boolean;
var
  data, sig, sTime: string;
  sOut, sJson : string;
begin
  sTime:= GetTimestamp;
  data := Format('timestamp=%s', [sTime]);
  sig  := CalculateHMACSHA256( data, App.Engine.ApiConfig.GetSceretKey( GetExKind , mtSpot ) );

  SetParam('timestamp', sTime );
  SetParam('signature', sig );
  SetParam('X-MBX-APIKEY', App.Engine.ApiConfig.GetApiKey( GetExKind , mtSpot ), pkHTTPHEADER );

  if not RequestAsync( ReceiveDNWState , rmGET, '/sapi/v1/asset/assetDetail') then
     App.Log( llError, 'Failed %s RequestDNWState ', [ TExchangeKindDesc[GetExKind]] );

  Result := true;

end;

function TBinanceSpotNMargin.RequestDNWStateSync: boolean;
var
  data, sig, sTime: string;
  sOut, sJson : string;
begin
  sTime:= GetTimestamp;
  data := Format('timestamp=%s', [sTime]);
  sig  := CalculateHMACSHA256( data, App.Engine.ApiConfig.GetSceretKey( GetExKind , mtSpot ) );

  SetParam('timestamp', sTime );
  SetParam('signature', sig );
  SetParam('X-MBX-APIKEY', App.Engine.ApiConfig.GetApiKey( GetExKind , mtSpot ), pkHTTPHEADER );

//  if not RequestAsync( ReceiveDNWState , rmGET, '/sapi/v1/asset/assetDetail') then
//     App.Log( llError, 'Failed %s RequestDNWState ', [ TExchangeKindDesc[GetExKind]] );

  if Request( rmGET, '/sapi/v1/asset/assetDetail', '', sJson, sOut ) then
  begin
//    App.Log( llDebug, '', '%s (%s, %s)', [ TExchangeKindDesc[GetExKind], sOut, sJson] );
    gBinReceiver.ParseDNWState( sJson );
  end else
  begin
    App.Log( llError, '', 'Failed %s RequestDNWState (%s, %s)',
      [ TExchangeKindDesc[GetExKind], sOut, sJson] );
    Exit(false);
  end;

  Result := true;

end;


function TBinanceSpotNMargin.RequestMarginMaster: boolean;
var
  data, sig, sTime: string;
  sOut, sJson : string;

begin

  SetBaseUrl( App.Engine.ApiConfig.GetBaseUrl( GetExKind , mtSpot ) );
  sTime:= GetTimestamp;
  data := Format('timestamp=%s', [sTime]);
  sig  := CalculateHMACSHA256( data, App.Engine.ApiConfig.GetSceretKey( GetExKind , mtSpot ) );

  App.Log( llDebug, '', '%s : %s',  [ App.Engine.ApiConfig.GetSceretKey( GetExKind , mtSpot )
                                    , App.Engine.ApiConfig.GetApiKey( GetExKind , mtSpot )] );

  SetParam('timestamp', sTime );
  SetParam('signature', sig );
  SetParam('X-MBX-APIKEY', App.Engine.ApiConfig.GetApiKey( GetExKind , mtSpot ), pkHTTPHEADER );

  if Request( rmGET, '/sapi/v1/margin/isolated/allPairs', '', sJson, sOut ) then
  begin
    //App.Log( llDebug, '', '%s (%s, %s)', [ TExchangeKindDesc[GetExKind], sOut, sJson] );
    gBinReceiver.ParseMarginPair( sJson );
  end else
  begin
    App.Log( llError, '', 'Failed %s RequestMarginMaster (%s, %s)',
      [ TExchangeKindDesc[GetExKind], sOut, sJson] );
    Exit( false );
  end;

  Result := true;

  
end;

function TBinanceSpotNMargin.RequestMarginTier: boolean;
var
  data, sig, sTime: string;
  sOut, sJson : string;
begin

  SetBaseUrl( App.Engine.ApiConfig.GetBaseUrl( GetExKind , mtSpot ) );
  sTime:= GetTimestamp;
  data := Format('timestamp=%s', [sTime]);
  sig  := CalculateHMACSHA256( data, App.Engine.ApiConfig.GetSceretKey( GetExKind , mtSpot ) );

  App.Log( llDebug, '', '%s : %s',  [ App.Engine.ApiConfig.GetSceretKey( GetExKind , mtSpot )
                                    , App.Engine.ApiConfig.GetApiKey( GetExKind , mtSpot )] );

  SetParam('timestamp', sTime );
  SetParam('signature', sig );
  SetParam('X-MBX-APIKEY', App.Engine.ApiConfig.GetApiKey( GetExKind , mtSpot ), pkHTTPHEADER );

  if Request( rmGET, '/sapi/v1/margin/isolated/allPairs', '', sJson, sOut ) then
  begin
    //App.Log( llDebug, '', '%s (%s, %s)', [ TExchangeKindDesc[GetExKind], sOut, sJson] );
    gBinReceiver.ParseMarginPair( sJson );
  end else
  begin
    App.Log( llError, '', 'Failed %s RequestMarginMaster (%s, %s)',
      [ TExchangeKindDesc[GetExKind], sOut, sJson] );
    Exit( false );
  end;

  Result := true;

end;

end.
