unit UBinanceSpotNMargin;

interface

uses
  System.Classes, System.SysUtils, System.DateUtils,

  System.JSON,  Rest.Json ,   Rest.Types,

  UExchange, USymbols , UOrders,  UMarketSpecs,

  UApiTypes

  ;

const
  REQ_END_POINT = '/sapi/v1/capital/config/getall';
//  REQ_END_POINT = '/sapi/v1/asset/assetDetail';

type
  TBinanceSpotNMargin = class( TExchange )
  private

    function RequestSpotMaster : boolean ;
    function RequestSpotTicker : boolean ;
    function RequestMarginMaster : boolean;
    function RequestMarginTier : boolean;
    procedure ReceiveDNWState;
    function RequestDNWStateSync : boolean;
    procedure MakeRest;


  public
    Constructor Create( aObj : TObject; aMarketType : TMarketType );
    Destructor  Destroy; override;

    procedure CyclicNotify(Sender: TObject);    override;    
    procedure ParseRequestData(iCode: integer; sName, sData: string);    override;    
    procedure RestNotify(Sender: TObject);  override;        

    function ParsePrepareMaster : integer; override;
    function RequestMaster : boolean ; override;
    function RequestDNWState : boolean; override;

    function RequestListenKey(bFirst: boolean): string;

	  function RequestBalance : boolean; override;
    function RequestOrders: boolean; override;

    	// to shared memory
    function SenderOrder( aOrder : TOrder ): boolean ; override;
    procedure ReceivedData( aReqType : TRequestType;  aData, aRef : string );override;

    procedure RequestBalance( aSymbol : TSymbol ) ; overload; override;
		procedure RequestOrderList( aSymbol : TSymbol ); override;
    procedure RequestPosition( aSymbol : TSymbol ); override;
    procedure RequestTradeAmt(aSymbol: TSymbol); override;

    procedure RequestOrderBook(aSymbol: TSymbol);override;

  end;

implementation

uses
  GApp, GLibs  , UApiConsts
  , UTypes
  , UBinanceParse
  , UCyclicItems, URestRequests
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
	if Result then
		MakeRest;
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
        var iPre, iQtyPre : integer;

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
            iQtyPre := GetPrecision( sTmp );
          end;
        end;

        aSymbol.Spec.SetSpec( iPre, dSize, dSize2, iQtyPre );

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

function TBinanceSpotNMargin.RequestListenKey(bFirst: boolean) : string;
var
  data, sig, sRrsc: string;
  sOut, sJson : string;
  aMethod : TRESTRequestMethod;
  aObj  : TJsonObject;
begin

  REsult := '';
  if bFirst then
  begin
    aMethod := rmPOST   ;
    sRrsc   := '/api/v3/userDataStream';
  end
  else begin
    aMethod := rmPUT;
    sRrsc   := '/api/v3/userDataStream?listenKey='+Field1;
  end;

  SetParam('X-MBX-APIKEY', App.Engine.ApiConfig.GetApiKey( GetExKind , mtSpot ), pkHTTPHEADER );

  if Request( aMethod, sRrsc, '', sJson, sOut ) then
  begin
    if bFirst then
      if RestRes.JSONValue <> nil then
      begin
        aObj  := RestRes.JSONValue as TJsonObject;
        Result := aObj.GetValue('listenKey').Value;
        Field1 := Result;
      end;
  end else
  begin
    App.Log( llError, '', 'Failed %s RequestListenKey (%s, %s)',
      [ TExchangeKindDesc[GetExKind], sOut, sJson] );
    Exit;
  end;
end;

function TBinanceSpotNMargin.RequestBalance: boolean;
var
  data, sig, sTime: string;
  sOut, sJson : string;
begin
  sTime:= GetTimestamp;
  data := Format('type=SPOT&timestamp=%s', [sTime]);
  sig  := CalculateHMACSHA256( data, App.Engine.ApiConfig.GetSceretKey( GetExKind , mtSpot ) );

  SetParam('type', 'SPOT');
  SetParam('timestamp', sTime );
  SetParam('signature', sig );
  SetParam('X-MBX-APIKEY', App.Engine.ApiConfig.GetApiKey( GetExKind , mtSpot ), pkHTTPHEADER );

  if Request( rmGET, '/sapi/v1/accountSnapshot', '', sJson, sOut ) then
  begin
//    App.Log( llDebug, '', '%s (%s, %s)', [ TExchangeKindDesc[GetExKind], sOut, sJson] );
    gBinReceiver.ParseSpotBalance( sJson );
  end else
  begin
    App.Log( llError, '', 'Failed %s RequestBalance (%s, %s)',
      [ TExchangeKindDesc[GetExKind], sOut, sJson] );
    Exit(false);
  end;

  Result := true;

end;

procedure TBinanceSpotNMargin.RequestBalance(aSymbol: TSymbol);
begin

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

  if not RequestAsync( ReceiveDNWState , rmGET, REQ_END_POINT) then
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


  if Request( rmGET, REQ_END_POINT, '', sJson, sOut ) then
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


procedure TBinanceSpotNMargin.MakeRest;
var
  aItem : TCyclicItem;  
begin
  aItem := CyclicItems.New('status');
  aItem.Interval  := 3000;
  aItem.Index     := 0;
  aItem.Resource	:= REQ_END_POINT;//'/sapi/v1/asset/assetDetail';
  aITem.Method		:= rmGET;

  aItem := CyclicItems.New('listenkey');
  aItem.Interval  := 60000 * 20;
  aItem.Index     := 1;

	MakeRestItems( 0 );     
  MakeCyclicThread;  
end;

procedure TBinanceSpotNMargin.CyclicNotify(Sender: TObject);
var
  aReq  : TRequest;
  sTime, data, sig : string;
begin
  if Sender = nil then Exit;       
  aReq := Sender as TRequest;

  if aReq.Seq = 0 then
  begin
    sTime:= GetTimestamp;
    data := Format('timestamp=%s', [sTime]);
    sig  := CalculateHMACSHA256( data, App.Engine.ApiConfig.GetSceretKey( GetExKind , mtSpot ) );

    aReq.Req.AddParameter('timestamp', sTime, pkGETorPOST);
    aReq.Req.AddParameter('signature', sig, pkGETorPOST);
    aReq.Req.AddParameter('X-MBX-APIKEY', App.Engine.ApiConfig.GetApiKey( GetExKind , mtSpot ), pkHTTPHEADER);
  end else
  begin
    RequestListenKey(false);
    Exit;
  end;

  if aReq.RequestAsync then
  	aReq.State := 1;
end;

procedure TBinanceSpotNMargin.RestNotify(Sender: TObject);
begin
  if Sender = nil then Exit;
  inherited  RestNotify( Sender )         
end;





procedure TBinanceSpotNMargin.ParseRequestData(iCode: integer; sName,
  sData: string);
begin
	if gBinReceiver = nil then Exit;
  
  if sData = '' then
  begin
  	App.Log(llError, '%s %s Data is Empty', [ TExchangeKindShortDesc[ GetExKind ], sName ]  );
    Exit;
  end;

  if iCode <> 200 then begin
  	App.Log(llError, '%s %s Request is Failed : %d,  %s', [ TExchangeKindShortDesc[ GetExKind ], sName, iCode, sData ]  );
    Exit;  	
  end else
		if sName = 'status' then
	   	gBinReceiver.ParseDNWState( sData );
end;

{$REGION '......... shared memoroy function ..............' }

function TBinanceSpotNMargin.SenderOrder(aOrder: TOrder): boolean;
begin

end;

procedure TBinanceSpotNMargin.RequestOrderBook(aSymbol: TSymbol);
begin
	App.Engine.SharedManager.RequestData( ekBinance, aSymbol.Spec.Market,
      rtOrderBook,  aSymbol.OrgCode , aSymbol.Code
      )  ;
end;

procedure TBinanceSpotNMargin.RequestOrderList(aSymbol: TSymbol);
begin
  inherited;

end;

function TBinanceSpotNMargin.RequestOrders: boolean;
begin

end;

procedure TBinanceSpotNMargin.RequestPosition(aSymbol: TSymbol);
begin
  inherited;

end;

procedure TBinanceSpotNMargin.ReceivedData(aReqType: TRequestType; aData,
  aRef: string);
begin
  inherited;

end;

procedure TBinanceSpotNMargin.RequestTradeAmt(aSymbol: TSymbol);
begin
  inherited;

end;

{$ENDREGION}

end.
