unit UBinanceFutures;

interface

uses
  System.Classes, System.SysUtils, System.DateUtils,

  UExchange,

  UOrders,

  UApiTypes

  ;

type
  TBinanceFutures = class( TExchange )
  private
    FIndex  : integer;
    FLastIndex : integer;

    function RequestFuttMaster : boolean;
    function RequestFutTicker  : boolean;

    // async proc
    procedure AsyncFutAllTicker;
    procedure AsyncFutAllOrderBook;
    procedure MakeRest;

  public
    Constructor Create( aObj : TObject; aMarketType : TMarketType );
    Destructor  Destroy; override;

    function ParsePrepareMaster : integer; override;
    function RequestMaster : boolean ; override;
    function RequestCandleData( sUnit : string; sCode : string ) : boolean;override;
    function RequestOrder( aOrder : TOrder ) : boolean;


    procedure ParseRequestData( iCode : integer; sName : string; sData : string ); override;
    procedure CyclicNotify( Sender : TObject ); override; 
    procedure RestNotify( Sender : TObject ); override;    

    procedure RequestData( iMod : integer );

    function RequestListenKey( bFirst : boolean ) : string;

	  function RequestBalance : boolean; override;
    function RequestPositons : boolean; override;
    function RequestOrders: boolean; override;
  end;

implementation

uses
  GApp, GLibs, UApiConsts
  , UBinanceParse
  , UCyclicItems, URestRequests
  , UEncrypts
  , REST.Types
  , System.JSON
  ;

{ TBinanceSpotNMargin }



constructor TBinanceFutures.Create(aObj: TObject; aMarketType: TMarketType);
begin
  inherited Create( aObj, aMarketType );

  FIndex  := 0;
  FLastIndex := -1;  

end;


destructor TBinanceFutures.Destroy;
begin

  inherited;
end;

function TBinanceFutures.ParsePrepareMaster: integer;
begin
  gBinReceiver.ParsePrepareFuttMaster(MasterData);
end;



function TBinanceFutures.RequestBalance: boolean;
var
  data, sig, sTime: string;
  sOut, sJson : string;
begin
  sTime:= GetTimestamp;
  data := Format('timestamp=%s', [sTime]);
  sig  := CalculateHMACSHA256( data, App.Engine.ApiConfig.GetSceretKey( GetExKind , mtFutures ) );


  SetParam('timestamp', sTime );
  SetParam('signature', sig );
  SetParam('X-MBX-APIKEY', App.Engine.ApiConfig.GetApiKey( GetExKind , mtFutures ), pkHTTPHEADER );

  if Request( rmGET, '/fapi/v2/account', '', sJson, sOut ) then
  begin
//    App.Log( llDebug, '', '%s (%s, %s)', [ TExchangeKindDesc[GetExKind], sOut, sJson] );
    gBinReceiver.ParseFutBalance( sJson );
  end else
  begin
    App.Log( llError, '', 'Failed %s RequestBalance (%s, %s)',
      [ TExchangeKindDesc[GetExKind], sOut, sJson] );
    Exit(false);
  end;

  Result := true;

end;

function TBinanceFutures.RequestCandleData(sUnit, sCode: string): boolean;
var
  sJson, sOut, sTmp : string;
  bRes : boolean;
  aKind : TMajorSymbolKind;
begin

  bRes := false;
  try
    SetParam('symbol', sCode );
    SetParam('limit', '50' );
    SetParam('interval', sUnit );

    if Request( rmGET, '/fapi/v1/klines' , '', sJson, sOut ) then
    begin
      sTmp := UpperCase(copy( sCode, 1, 3 ));
      if sTmp = 'BTC' then
        aKind := msBTC
      else aKind := msETH;
      gBinReceiver.ParseCandleData(aKind, sUnit, sJson);
    end else
    begin
      App.Log( llError, '', 'Failed %s RequestCandleData (%s, %s)',
        [ TExchangeKindDesc[GetExKind], sOut, sJson] );
      Exit( false );
    end;
    bRes := true;
  except
  end;

  Result := bRes;
end;

procedure TBinanceFutures.RequestData( iMod : integer );
begin

//	case iMod of
//      0 : begin
//        RestResult := Req[iMod].RequestAsync( AsyncFutAllOrderBook, rmGET, '/fapi/v1/ticker/bookTicker');
//      end;
//      1 : begin
//        RestResult := Req[iMod].RequestAsync( AsyncFutAllTicker, rmGET, '/fapi/v1/ticker/24hr');
//      end;    
//  end;

//  if ( FIndex <> FLastIndex )
//    or ( RestResult = nil )
//    or (( RestResult <> nil ) and ( RestResult.Finished )) then
//  begin
//    FLastIndex := FIndex;
//
//    case FIndex of
//      0 : begin
//        RestResult := Req[0].RequestAsync( AsyncFutAllOrderBook, rmGET, '/fapi/v1/ticker/bookTicker');
//      end;
//      1 : begin
//        RestResult := Req[1].RequestAsync( AsyncFutAllTicker, rmGET, '/fapi/v1/ticker/24hr');
//      end;
////      3 :begin  
////        RestResult := Req[2].RequestAsync( parseTicker, rmGET, '/public/ticker/ALL_KRW', true);
////      end;
//      else exit;
//    end;
//
//    if RestResult = nil then
//      App.Log( llError,  ' !! %s, %d Request %d Error ', [ TExchangeKindDesc[GetExKind], FIndex ] )
//    else begin
//      inc( FIndex );
//      if FIndex >= 2 then
//        FIndex := 0;
//    end;
//
//  end else
//  begin
//    var s : string;
//    if RestResult.Finished then s := 'fin' else s := 'not fin';
//    App.DebugLog( '!! %s, %d waiting req -> %d %s ', [ TExchangeKindDesc[GetExKind], FIndex, RestResult.ThreadID, s ]  );
//  end;
end;

procedure TBinanceFutures.AsyncFutAllOrderBook;
var
  sJson : string;
begin
//  sJson :=  Req[0].GetResponse;
//  if sJson = '' then Exit;
  gBinReceiver.ParseFutAllOrderBook( sJson );

end;

procedure TBinanceFutures.AsyncFutAllTicker;
var
  sJson : string;
begin
//  sJson :=  Req[1].GetResponse;
//  if sJson = '' then Exit;
  gBinReceiver.ParseFutAllTicker( sJson );

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
begin
  gBinReceiver.ParseFuttMaster(MasterData);
  Result := App.Engine.SymbolCore.Futures[ GetExKind].Count > 0;
end;

function TBinanceFutures.RequestListenKey(bFirst: boolean) : string;
var
  data, sig, sTime: string;
  sOut, sJson : string;
  aMethod : TRESTRequestMethod;
  aObj  : TJsonObject;
begin

  REsult := '';
  if bFirst then aMethod := rmPOST
  else begin
    aMethod := rmPUT;
    App.DebugLog('------------  listenkey update --------------' );
  end;

  SetBaseUrl(    App.Engine.ApiConfig.GetBaseUrl( GetExKind , mtFutures ) );
  SetParam('X-MBX-APIKEY', App.Engine.ApiConfig.GetApiKey( GetExKind , mtFutures ), pkHTTPHEADER );

  if Request( aMethod, '/fapi/v1/listenKey', '', sJson, sOut ) then
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

//function TBinanceFutures.RequestFuttMaster: boolean;
//var
//  sTmp, sOut, sJson : string;
//begin
//  sTmp  := App.Engine.ApiConfig.GetBaseUrl( GetExKind , mtFutures );
//  SetBaseUrl( sTmp );
//
//  if Request( rmGET, '/fapi/v1/exchangeInfo' , '', sJson, sOut ) then
//  begin
//    gBinReceiver.ParseFuttMaster(sJson);
//  end else
//  begin
//    App.Log( llError, '', 'Failed %s Fut PreparMaster (%s, %s)',
//      [ TExchangeKindDesc[GetExKind], sOut, sJson] );
//    Exit( false );
//  end;
//
//  Result := App.Engine.SymbolCore.Futures[ GetExKind].Count > 0;
//
//end;

function TBinanceFutures.RequestMaster: boolean;
begin  
  
  Result := RequestFuttMaster
         and RequestFutTicker;
         
	if Result then
		MakeRest;
end;


function TBinanceFutures.RequestOrder(aOrder: TOrder): boolean;
var
  sTime, sData , sSig, sJson, sOut : string;
begin

  sTime:= GetTimestamp;

  sData := format('symbol=%s&side=%s&type=LIMIT&timeInForce=GTC&quantity=%s&price=%s'+
  '&newClientOrderId=%s&timestamp=%s', [
    aOrder.Symbol.OrgCode,
    ifThenStr( aOrder.Side > 0 ,'BUY', 'SELL'),
    format('%.*f', [ aOrder.Symbol.Spec.QtyPrecision, aOrder.OrderQty ] ),
    format('%.*f', [ aOrder.Symbol.Spec.Precision, aOrder.Price ] ),
    aOrder.LocalNo,
    sTime
  ]);

  sSig  := CalculateHMACSHA256( sData, App.Engine.ApiConfig.GetSceretKey( GetExKind , mtFutures ) );

  SetParam('timestamp', sTime );
  SetParam('signature', sSig );
  SetParam('X-MBX-APIKEY', App.Engine.ApiConfig.GetApiKey( GetExKind , mtFutures ), pkHTTPHEADER );

  if Request( rmPOST, '/fapi/v1/order', '', sJson, sOut ) then
  begin
//    App.Log( llDebug, '', '%s (%s, %s)', [ TExchangeKindDesc[GetExKind], sOut, sJson] );
    gBinReceiver.ParseFutPosition( sJson );
  end else
  begin
    App.Log( llError, '', 'Failed %s RequestPositons (%s, %s)',
      [ TExchangeKindDesc[GetExKind], sOut, sJson] );
    Exit(false);
  end;

  Result := true;



end;

function TBinanceFutures.RequestOrders: boolean;
begin

end;

function TBinanceFutures.RequestPositons: boolean;
var
  data, sig, sTime: string;
  sOut, sJson : string;
begin
  sTime:= GetTimestamp;
  data := Format('timestamp=%s', [sTime]);
  sig  := CalculateHMACSHA256( data, App.Engine.ApiConfig.GetSceretKey( GetExKind , mtFutures ) );


  SetParam('timestamp', sTime );
  SetParam('signature', sig );
  SetParam('X-MBX-APIKEY', App.Engine.ApiConfig.GetApiKey( GetExKind , mtFutures ), pkHTTPHEADER );

  if Request( rmGET, '/fapi/v2/positionRisk ', '', sJson, sOut ) then
  begin
//    App.Log( llDebug, '', '%s (%s, %s)', [ TExchangeKindDesc[GetExKind], sOut, sJson] );
    gBinReceiver.ParseFutPosition( sJson );
  end else
  begin
    App.Log( llError, '', 'Failed %s RequestPositons (%s, %s)',
      [ TExchangeKindDesc[GetExKind], sOut, sJson] );
    Exit(false);
  end;

  Result := true;


end;

procedure TBinanceFutures.MakeRest;
var
  aItem : TCyclicItem;  
begin
  aItem := CyclicItems.New('orderbook');
  aItem.Interval  := 1000;
  aItem.Index     := 0;
  aItem.Resource	:= '/fapi/v1/ticker/bookTicker';  
  aITem.Method		:= rmGET;

  aItem := CyclicItems.New('listenkey');
  aItem.Interval  := 60000 * 20;    // 20Ка..
  aItem.Index     := 1;

	MakeRestItems( 0 );
  MakeCyclicThread;
end;

procedure TBinanceFutures.CyclicNotify(Sender: TObject);
var
  aReq  : TRequest;
begin
  if Sender = nil then Exit;
  aReq := Sender as TRequest;

  if aReq.Seq = 1 then
    RequestListenKey( false )
  else begin
    if aReq.RequestAsync then
    	aReq.State := 1;
  end;
end;

procedure TBinanceFutures.RestNotify(Sender: TObject);
begin
  if Sender = nil then Exit;
  inherited  RestNotify( Sender )
end;



procedure TBinanceFutures.ParseRequestData(iCode: integer; sName,
  sData: string);
begin
  if sData = '' then
  begin
  	App.Log(llError, '%s %s Data is Empty', [ TExchangeKindShortDesc[ GetExKind ], sName ]  );
    Exit;
  end;

  if iCode <> 200 then begin
  	App.Log(llError, '%s %s Request is Failed : %d,  %s', [ TExchangeKindShortDesc[ GetExKind ], sName, iCode, sData ]  );
    Exit;  	
  end else
		if sName = 'orderbook' then
		  gBinReceiver.ParseFutAllOrderBook( sData );

end;

end.
