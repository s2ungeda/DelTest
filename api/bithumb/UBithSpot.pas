unit UBithSpot;

interface

uses
  System.Classes, System.SysUtils, System.DateUtils, //Windows,

  System.JSON,  Rest.Json , Rest.Types ,  REST.Client,

  UExchange, USymbols , UOrders, UMarketSpecs,

  UApiTypes

  ;

type
  TBithSpot = class( TExchange )
  private

    FIndex  : integer;
    FLastIndex : integer;

    function RequestTicker : boolean;
    function RequestOrderBook : boolean;  overload;

    procedure OnHTTPProtocolError(Sender: TCustomRESTRequest);
    procedure parseAssetsstatus;
    procedure parseOrderBook;
    procedure parseTicker;
    procedure MakeRest;

    function EncodePath( sRrsc, sPoint, sTime  : string ) : string;
    
  public
    Constructor Create( aObj : TObject; aMarketType : TMarketType );
    Destructor  Destroy; override;

    procedure RequestData;

    procedure CyclicNotify( Sender : TObject ); override; 
    procedure RestNotify( Sender : TObject ); override;    

    procedure RequestOrderBook( c : char ) ; overload;
    function RequestDNWState : boolean; override;
    procedure ParseRequestData( iCode : integer; sName : string; sData : string ); override;
    procedure ParseRequestData( aReqType : TRequestType; sData : string ); override;

    function ParsePrepareMaster : integer  ; override;
    function RequestMaster : boolean ; override;
    function RequestCandleData( sUnit : string; sCode : string ) : boolean;override;

	  function RequestBalance : boolean; override;
    function RequestPositons : boolean; override;
    function RequestOrders: boolean;overload; override;

		// to shared memory
    procedure RequestBalance( aSymbol : TSymbol ) ; overload; override;
    procedure RequestOrderDetail( aOrder : TOrder ); override;
  	procedure RequestOrderList( aSymbol : TSymbol ); override;
    function SenderOrder( aOrder : TOrder ): boolean ; override;
    procedure ReceivedData( aReqType : TRequestType;  aData, aRef : string );override;

  end;

//var
//  CriticalSection: TRTLCriticalSection;

implementation

uses
  GApp , GLibs, UApiConsts ,  UEncrypts
  , UTypes
  , UBithParse, URestItems ,UCyclicItems, URestRequests
  , USymbolUtils
  , Math
  , Web.HTTPApp, idcodermime, IdGlobal
  ;

{ TBinanceSpotNMargin }

constructor TBithSpot.Create(aObj: TObject; aMarketType: TMarketType);
begin
  inherited Create( aObj, aMarketType );

  FIndex  := 0;
  FLastIndex := -1;

end;



destructor TBithSpot.Destroy;
begin

  inherited;
end;          

function TBithSpot.EncodePath(sRrsc, sPoint, sTime : string): string;
var
	sValue : string;
begin
	sValue := HTTPEncode(UTF8Encode(sPoint));
  sValue := StringReplace(sValue, '+', '%20', [rfReplaceAll]);
  sValue := StringReplace(sValue, '%21', '!', [rfReplaceAll]);
  sValue := StringReplace(sValue, '%27', '''', [rfReplaceAll]);
  sValue := StringReplace(sValue, '%28', '(', [rfReplaceAll]);
  sValue := StringReplace(sValue, '%29', ')', [rfReplaceAll]);
  sValue := StringReplace(sValue, '%26', '&', [rfReplaceAll]);
  sValue := StringReplace(sValue, '%3D', '=', [rfReplaceAll]);
  sValue := StringReplace(sValue, '%7E', '~', [rfReplaceAll]);

  Result := sRrsc + chr(0) + sValue +  chr(0 ) + sTime;
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

  try
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
  finally
    if aObj <> nil then aObj.Free
  end;
end;                                      





function TBithSpot.RequestMaster: boolean;
begin

  Result := RequestTicker
    and RequestOrderBook
    and RequestDNWState
    ;

	if Result then
		MakeRest;
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
  aList: TList;
begin
  aList := nil;
  master := TJsonObject.ParseJSONValue( MasterData ) as TJsonObject;
  aObj := master.GetValue('data') as TJsonObject;

  aList := TList.Create;
  try

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
        //Spec.SetSpec(1,1,4);
      end;

      aVal  := aPair.JsonValue;

      aSymbol.DayOpen := StrToFloatDef( aVal.GetValue<string>( 'opening_price' ), 0.0 );
      aSymbol.DayHigh := StrToFloatDef( aVal.GetValue<string>( 'max_price' ), 0.0 );
      aSymbol.DayLow  := StrToFloatDef( aVal.GetValue<string>( 'min_price' ), 0.0 );
      aSymbol.Last    := StrToFloatDef( aVal.GetValue<string>( 'closing_price' ), 0.0 );
      aSymbol.PrevClose   := StrToFloatDef( aVal.GetValue<string>( 'prev_closing_price' ), 0.0 );
      aSymbol.DayAmount   := StrToFloatDef( aVal.GetValue<string>( 'acc_trade_value_24H' ), 0.0 ) / 100000000;
      aSymbol.DayVolume   := StrToFloatDef( aVal.GetValue<string>( 'units_traded_24H' ), 0.0 );

  //    aSymbol.Time  := UnixToDateTime(  aVal.GetValue<int64>( 'date' ) );
      aSymbol.LocalTime := now;

      if bNew then
        App.Engine.SymbolCore.RegisterSymbol( GetExKind, aSymbol );

      if ( sCode <> 'BTC' ) and ( sCode <> 'ETH') and ( sCode <> 'XRP') then
        aList.Add( aSymbol );

    end;
    Result := App.Engine.SymbolCore.Spots[GetExKind].Count > 0 ;

    aList.Sort(CompareDailyAmount);
    if aList.Count > 2 then
    begin
      App.Engine.ApiManager.ExManagers[ekBithumb].TopAmtSymbols[0] := TSymbol( aList.Items[0] );
      App.Engine.ApiManager.ExManagers[ekBithumb].TopAmtSymbols[1] := TSymbol( aList.Items[1] );
    end;
  finally
    if aObj <> nil then
      aObj.Free;
    if aList <> nil then aList.Free;

  end;
end;





function TBithSpot.RequestOrderBook: boolean;
var
  sOut, sJson : string;
begin
  SetBaseUrl( App.Engine.ApiConfig.GetBaseUrl( GetExKind , mtSpot ) );

  // 406 에러 때문에 아래 와 같이 헤더 추가
  Set406;

//  if not RequestAsync(
//     ReceiveAsyncData
//   , rmGET, '/public/orderbook/ALL_KRW') then
//     App.Log( llError, 'Failed %s RequestOrderBook ', [ TExchangeKindDesc[GetExKind]] );

  if Request( rmGET, '/public/orderbook/ALL_KRW', '', sJson, sOut ) then
  begin
    gBithReceiver.ParseSpotOrderBook( sJson );
  end else
  begin
    App.Log( llError, '', 'Failed %s RequestOrderBook (%s, %s)',
      [ TExchangeKindDesc[GetExKind], sOut, sJson] );
    Exit( false );
  end;

  Result := true;
end;

procedure TBithSpot.RequestOrderBook( c : char );
var
  sOut, sJson : string;
begin

  //EnterCriticalSection(CriticalSection);
  try

    SetBaseUrl( App.Engine.ApiConfig.GetBaseUrl( GetExKind , mtSpot ) );
    // 406 에러 때문에 아래 와 같이 헤더 추가
    Set406;

    if Request( rmGET, '/public/orderbook/ALL_KRW', '', sJson, sOut ) then
    begin
      gBithReceiver.ParseSpotOrderBook( sJson );
    end else
    begin
      App.Log( llError, '', 'Failed %s RequestOrderBook (%s, %s)',
        [ TExchangeKindDesc[GetExKind], sOut, sJson] );
      Exit;
    end;

//    if not RequestAsync(
//       ReceiveAsyncData
//     , rmGET, '/public/orderbook/ALL_KRW') then
//       App.Log( llError, 'Failed %s RequestOrderBook ', [ TExchangeKindDesc[GetExKind]] );
  except
  //  LeaveCriticalSection(CriticalSection);
  end;
end;







function TBithSpot.RequestCandleData(sUnit, sCode: string): boolean;
var
  sOut, sJson, sRrs, sTmp : string;
  bRes : boolean;
  aKind : TMajorSymbolKind;
begin

  bRes := false;
  try

    SetBaseUrl( App.Engine.ApiConfig.GetBaseUrl( GetExKind , mtSpot ) );
    // 406 에러 때문에 아래 와 같이 헤더 추가
    Set406;

    sRrs := Format('public/candlestick/%s/%s', [ sCode, sUnit] );
    if Request( rmGET, sRrs, '', sJson, sOut ) then
    begin
      sTmp := copy( sCode, 1, 3 );
      if sTmp = 'BTC' then
        aKind := msBTC
      else aKind := msETH;
      gBithReceiver.ParseCandleData(aKind, sUnit, sJson );
    end else
    begin
      App.Log( llError, '', 'Failed %s RequestCandleData (%s, %s)',
        [ TExchangeKindDesc[GetExKind], sOut, sJson] );
      Exit ( false );
    end;
    bRes := true;
  except
  end;

  Result := bRes;

end;

procedure TBithSpot.ParseRequestData(iCode: integer; sName, sData: string);
begin

	if gBithReceiver = nil then exit;  

  if sData = '' then
  begin
  	App.Log(llError, '%s %s Data is Empty', [ TExchangeKindShortDesc[ GetExKind ], sName ]  );
    Exit;
  end;

  if iCode <> 200 then begin
  	App.Log(llError, '%s %s Request is Failed : %d,  %s', [ TExchangeKindShortDesc[ GetExKind ], sName, iCode, sData ]  );
    Exit;
  end else begin

		if sName = 'orderbook' then
			gBithReceiver.ParseSpotOrderBook( sData )
    else if sName = 'ticker' then
    	gBithReceiver.ParseTicker( sData )
    else if sName = 'status' then
    	gBithReceiver.ParseDnwState( sData );
  end;         
end;

procedure TBithSpot.ParseRequestData(aReqType: TRequestType; sData: string);
begin
	if gBithReceiver = nil then exit;  
	case aReqType of
    rtNewOrder: ;
    rtCnlOrder: ;
    rtOrderList: ;
    rtPosition: ;
    rtBalance: gBithReceiver.ParseBalance( sData ) ;
    rtAbleOrder: ;
  end;
end;

procedure TBithSpot.RequestData;
var
	aReq : TReqeustItem;
  i, idx : integer;
begin



	exit;

	for I := 0 to 2 do
  begin

//    if ( i = 2 ) and ( FIndex mod 3 = 0 ) then
//    	continue;
        
    aReq := TReqeustItem.Create;
    aReq.AMethod	:= rmGET;
    aReq.Req.init( App.Engine.ApiConfig.GetBaseUrl( GetExKind , mtSpot ), true );      


    case i of
      0 : begin aReq.AResource:= '/public/orderbook/ALL_KRW';   	idx	:= RestType(PUB_REQ);  
      					aReq.Name := 'orderbook'; end;
      1 : begin aReq.AResource:= '/public/assetsstatus/ALL';		 	idx := RestType(PUB_REQ); 
     						aReq.Name := 'assetsstatus'; 		end;
      2 : begin aReq.AResource:= 'public/ticker/ALL_KRW'; 				idx := RestType(PUB_REQ);
      					aReq.Name := 'ticker'; 		end;
    end;
    
//    if Rest[idx] <> nil then
//    	Rest[idx].PushQueue( aReq );  
  end;


  inc( FIndex );
  if FIndex > (High(int64) - 1000) then
  	FIndex := 0;
    
//  if ( FIndex <> FLastIndex )
//    or ( RestResult = nil )
//    or (( RestResult <> nil ) and ( RestResult.Finished )) then
//  begin
//    FLastIndex := FIndex;
//
//    case FIndex of
//      0 , 2: begin
//        RestResult := Req[0].RequestAsync( parseOrderBook, rmGET, '/public/orderbook/ALL_KRW', true);
//      end;
//      1 : begin
//        RestResult := Req[1].RequestAsync( parseAssetsstatus, rmGET, '/public/assetsstatus/ALL', true);
//      end;
//      3 :begin
//        RestResult := Req[2].RequestAsync( parseTicker, rmGET, '/public/ticker/ALL_KRW', true);
//      end;
//      else exit;
//    end;
//
//    if RestResult = nil then
//      App.Log( llError,  ' !! %s, %d Request %d Error ', [ TExchangeKindDesc[GetExKind], FIndex ] )
//    else begin
//      inc( FIndex );
//      if FIndex >= 4 then
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


procedure TBithSpot.parseOrderBook;
var
  sJson : string;
begin
 //sJson :=  Req[0].GetResponse;
  if sJson = '' then Exit;
  gBithReceiver.ParseSpotOrderBook( sJson );

end;
procedure TBithSpot.parseAssetsstatus;
var
  sJson : string;
begin
  //sJson :=  Req[1].GetResponse;
  if sJson = '' then Exit;
  gBithReceiver.ParseDnwState( sJson );

end;
procedure TBithSpot.parseTicker;
var
  sJson : string;
begin
  //sJson :=  Req[2].GetResponse;
  if sJson = '' then Exit;
  gBithReceiver.ParseTicker( sJson );
end;

function TBithSpot.RequestDNWState : boolean;
var
  sOut, sJson : string;
begin

 // EnterCriticalSection(CriticalSection);
  try
    SetBaseUrl( App.Engine.ApiConfig.GetBaseUrl( GetExKind , mtSpot ) );

    Set406;

    if Request( rmGET, '/public/assetsstatus/ALL', '', sJson, sOut ) then
    begin
      gBithReceiver.ParseDnwState( sJson );
    end else
    begin
      App.Log( llError, '', 'Failed %s RequestDNWState (%s, %s)',
        [ TExchangeKindDesc[GetExKind], sOut, sJson] );
      Exit (false);
    end;

    result := true;
//    if not RequestAsync(
//      ReceiveAsyncData
//     , rmGET, '/public/assetsstatus/ALL') then
//       App.Log( llError, 'Failed %s RequestDNWStte ', [ TExchangeKindDesc[GetExKind]] );
  except
   // LeaveCriticalSection(CriticalSection);
  end;

end;


	// received data from shared memroy
procedure TBithSpot.ReceivedData(aReqType: TRequestType; aData, aRef: string);
begin
	case aReqType of
    rtNewOrder: gBithReceiver.ParseSpotNewOrder( aData, aRef ) ;    // aRef is LocalHo
    rtCnlOrder: gBithReceiver.ParseSpotCnlOrder( aData, aRef ) ;	  // aRef is OrderNo
    rtOrderList: gBithReceiver.ParseSpotOrderList( aData, aRef );		// aRef is symbol code
    rtBalance: gBithReceiver.ParseSpotBalance( aData, aRef ) ;  // aref is symbol code
	  rtPosition: ;
    rtAbleOrder: ;
    rtOrdDetail: gBithReceiver.ParseSpotOrderDetail( aData, aRef ) ; // aref is OrderNo
    rtTradeAmt : ;
  end;
end;

function TBithSpot.RequestBalance: boolean;
var
	sRsrc, sJson, sOut, sTime, sData, sVal, sSig : string;
  sParam1 : string;
begin
  try

  	SetBaseUrl( App.Engine.ApiConfig.GetBaseUrl( GetExKind , mtSpot ) );
    
    sParam1	:= 'ALL';
    sRsrc := '/info/balance';
    sTime := GetTimestamp;
    sVal	:= EncodePath( sRsrc, Format('endPoint=%s&currency=%s', [ sRsrc, sParam1] ), sTime );

    sSig	:= CalculateHMACSHA512( sVal, App.Engine.ApiConfig.GetSceretKey( GetExKind, mtSpot) );
    sData	:= TIdEncoderMIME.EncodeString( sSig, IndyTextEncoding_UTF8 );

// 		Set406;

  	SetParam('Api-Key', App.Engine.ApiConfig.GetApiKey( GetExKind, mtSpot), TRESTRequestParameterKind.pkHTTPHEADER );//, [poDoNotEncode]);
		SetParam('Api-Sign', sData , TRESTRequestParameterKind.pkHTTPHEADER , [poDoNotEncode]);
		SetParam('Api-Nonce', sTime , TRESTRequestParameterKind.pkHTTPHEADER );

    SetParam('endPoint', sRsrc, TRESTRequestParameterKind.pkREQUESTBODY);
    SetParam('currency', sParam1, TRESTRequestParameterKind.pkREQUESTBODY);    

    if Request( rmPOST, sRsrc, '', sJson, sOut ) then
    begin
      gBithReceiver.ParseBalance( sJson );
    end else
    begin
      App.Log( llError, '', 'Failed %s RequestBalance (%s, %s)',
        [ TExchangeKindDesc[GetExKind], sOut, sJson] );
      Exit( false );
    end;
    
	except
  end;

  Result := true;
end;

function TBithSpot.RequestPositons: boolean;
begin
  try
    
	except
  end;

  Result := true;
end;

function TBithSpot.RequestOrders: boolean;
var
	sRsrc, sJson, sOut, sTime, sData, sVal, sSig : string;
  sParam1 : string;
  I: Integer;
  aSymbol : TSymbol;
begin
  try

  	SetBaseUrl( App.Engine.ApiConfig.GetBaseUrl( GetExKind , mtSpot ) );
    
    sParam1	:= 'ALL';
    sRsrc := '/info/orders';


    for I := 0 to App.Engine.SymbolCore.Symbols[ekBithumb].Count-1 do
		begin

    	sParam1	:= App.Engine.SymbolCore.Symbols[ekBithumb].Symbols[i].Code;
    
      sTime := GetTimestamp;
      sVal	:= EncodePath( sRsrc, Format('endPoint=%s&order_currency=%s', [ sRsrc, sParam1] ), sTime );

      sSig	:= CalculateHMACSHA512( sVal, App.Engine.ApiConfig.GetSceretKey( GetExKind, mtSpot) );
      sData	:= TIdEncoderMIME.EncodeString( sSig, IndyTextEncoding_UTF8 );

  // 		Set406;

      SetParam('Api-Key', App.Engine.ApiConfig.GetApiKey( GetExKind, mtSpot), TRESTRequestParameterKind.pkHTTPHEADER );//, [poDoNotEncode]);
      SetParam('Api-Sign', sData , TRESTRequestParameterKind.pkHTTPHEADER , [poDoNotEncode]);
      SetParam('Api-Nonce', sTime , TRESTRequestParameterKind.pkHTTPHEADER );

      SetParam('endPoint', sRsrc, TRESTRequestParameterKind.pkREQUESTBODY);
      SetParam('order_currency', sParam1, TRESTRequestParameterKind.pkREQUESTBODY);    

      if Request( rmPOST, sRsrc, '', sJson, sOut ) then
      begin
        gBithReceiver.ParseOrders( sJson );
      end else
      begin
        App.Log( llError, '', 'Failed %s RequestOrders (%s, %s)',
          [ TExchangeKindDesc[GetExKind], sOut, sJson] );
        Exit( false );
      end;  
      
	    sleep(30);
    end;
    
	except
  end;

  Result := true;
end;

procedure TBithSpot.OnHTTPProtocolError(Sender: TCustomRESTRequest);
begin
  if Sender <> nil  then
  begin
    App.Log( llError,  '%s Async Request Error : %s ( status : %d, %s)' , [ TExchangeKindDesc[GetExKind]
    ,  Sender.Response.Content ,  Sender.Response.StatusCode, Sender.Response.StatusText ]  );
  end;
end;

procedure TBithSpot.MakeRest;
var
  aItem : TCyclicItem;
  info : TDivInfo;
begin

  aItem := CyclicItems.New('orderbook');
  aItem.Interval  := 1000;
  aItem.Index     := 1;
  aItem.Resource	:= '/public/orderbook/ALL_KRW';
  aITem.Method		:= rmGET;

  aItem := CyclicItems.New('ticker');
  aItem.Interval  := 1000;
  aItem.Index     := 2;
  aItem.Resource	:= '/public/ticker/ALL_KRW';
  aITem.Method		:= rmGET;

  aItem := CyclicItems.New('status');
  aItem.Interval  := 3000;
  aItem.Index     := 3;
  aItem.Resource	:= '/public/assetsstatus/ALL';
  aITem.Method		:= rmGET;

  MakeRestItems( 3 );
	// cyclic thread .. 리커버리 끝나면  resume..
	MakeCyclicThread;
end;

procedure TBithSpot.CyclicNotify(Sender: TObject);
var
	aItem : TCyclicItem;
  aReq  : TRequest;
  sNm, sRsrc  : string;
begin

  if Sender = nil then Exit;
  aReq := Sender as TRequest;

  if aReq.RequestAsync then
  	aReq.State := 1;

end;

procedure TBithSpot.RestNotify(Sender: TObject);
var
   aReq : TRequest;
   sTmp : string;
   avg  : integer;
   gap  : integer;
begin
  if Sender = nil then Exit;
  inherited  RestNotify( Sender );

end;
        

procedure TBithSpot.RequestBalance(aSymbol: TSymbol);
begin

	App.Engine.SharedManager.RequestData( ekBithumb, aSymbol.Spec.Market,
      rtBalance,  aSymbol.Code , aSymbol.Code
      )  ;   
end;


procedure TBithSpot.RequestOrderDetail(aOrder: TOrder);
var
	sData : string;
begin
	sData := Format('%s|%s', [ aOrder.Symbol.Code, aOrder.OrderNo ] );

	App.Engine.SharedManager.RequestData( ekBithumb, aOrder.Symbol.Spec.Market,
      rtOrdDetail,  sData , aOrder.OrderNo
      )  ;       
end;

procedure TBithSpot.RequestOrderList(aSymbol: TSymbol);
var
	sData : string;
begin     
	sData := Format('%s', [ aSymbol.Code ] );

	App.Engine.SharedManager.RequestData( ekBithumb, aSymbol.Spec.Market,
      rtOrderList,  sData , aSymbol.Code
      )  ;
end;

function TBithSpot.SenderOrder(aOrder: TOrder): boolean;
var
	sData : string;
begin
	// '|' 를 구분자 사용	  

  if aOrder.OrderType = otNormal then
  begin

    sData := Format('%s|%s|%s|%s|%s', [
      aOrder.Symbol.Code,
      ifThenStr( aOrder.Side > 0 , 'bid', 'ask' ),
      aOrder.PriceBI.OrgVal,
      aOrder.OrderQtyBI.OrgVal,
      aOrder.Symbol.Spec.SettleCode
    ]);

    App.Engine.SharedManager.RequestData( ekBithumb, aOrder.Symbol.Spec.Market,
      rtNewOrder,  sData , aOrder.LocalNo
      )
  end else
  if aOrder.OrderType = otCancel then
  begin
    sData := Format('%s|%s|%s|%s', [
      aOrder.OrderNo,
      aOrder.Symbol.Code,
      ifThenStr( aOrder.Side > 0 , 'bid', 'ask' ),
      aOrder.Symbol.Spec.SettleCode
    ]);

    App.Engine.SharedManager.RequestData( ekBithumb, aOrder.Symbol.Spec.Market,
      rtCnlOrder,  sData , aOrder.OrderNo
      )    
  end;
  
end;

end.
