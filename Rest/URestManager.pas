unit URestManager;

interface

uses
  System.Classes, System.SysUtils,

  System.JSON,  Rest.Json , REST.Client,  Rest.Types,

  UApiTypes ,

  USharedData   , USharedConsts
  ;

type

  TRestManager = class
  private
    FRestReq : array [TExchangeKind] of TRESTRequest ;
    FOnPushData: TSharedPushData;

    function CheckShareddData( var sArr : TArray<string>; sData: string;
               iCount : integer; aPrcName : string ) : boolean;

    procedure PushData( aExKind : TExchangeKind; aMarket : TMarketType; c3 : char;  sData, sRef : string );

    procedure RequestBinFutNewOrder( sData, sRef : string );
    procedure RequestBinFutCnlOrder( sData, sRef : string );

    //  bithumb
    procedure RequestBitOrderList( sData, sRef : string );
    procedure RequestBitBalance( sData, sRef : string );
    procedure RequestBitNewOrder( sData, sRef : string );
    procedure RequestBitCnlOrder( sData, sRef : string );
    procedure RequestBitOrderDetail( sData, sRef : string );
    procedure RequestBitTradeAmt( sData, sRef : string );

    //  upbit
    procedure RequestUptOrderList( sData, sRef : string; cDiv : char = TR_REQ_ORD );
    procedure RequestUptBalance( sData, sRef : string );
    procedure RequestUptAvailableOrder( sData, sRef : string );
    procedure RequestUptNewOrder( sData, sRef : string );
    procedure RequestUptCnlOrder( sData, sRef : string );
    procedure RequestUptOrderDetail( sData, sRef : string );
    function  RequestUptFailMessage( sReq : string ) : string;
    

    function Request( aExKind : TExchangeKind; AMethod : TRESTRequestMethod;  AResource : string;
       var outJson, outRes : string ) : boolean;
  public
    Constructor Create;
    Destructor  Destroy; override;
    procedure init( bn, up, bt : TRESTRequest );
    procedure OnSharedDataNotify( aData : TDataItem );


    property OnPushData : TSharedPushData read FOnPushData write FOnPushData;
  end;

implementation

uses
  GApp, GLibs
  , UApiConsts
  , UEncrypts
  , IdCoderMIME, IdGlobal
  , system.Hash

  , JOSE.Core.JWT   ,JOSE.Core.Builder, JOSE.Core.JWA
  ;

{ TRestManager }

function TRestManager.CheckShareddData(var sArr : TArray<string>; sData: string;
   iCount : integer; aPrcName : string): boolean;
var
  iLen : integer;
begin

  sArr  := sData.Split(['|']);
  iLen  := high( sArr );

  if ( iLen < 0 ) or ( iLen + 1 <> iCount ) then
  begin
    App.Log(llError, '%s data is empty (%s) ', [ aPrcName, sData ] );
    Result := false;
  end else
    Result := true;
end;

constructor TRestManager.Create;
begin
  FRestReq[ekBinance] := nil;
  FRestReq[ekUpbit]   := nil;
  FrestReq[ekBithumb] := nil;
end;

destructor TRestManager.Destroy;
begin

  inherited;
end;

procedure TRestManager.init(bn, up, bt: TRESTRequest);
begin
  FRestReq[ekBinance] := bn;
  FRestReq[ekUpbit]   := up;
  FrestReq[ekBithumb] := bt;
end;


procedure TRestManager.OnSharedDataNotify(aData: TDataItem);
begin
  App.DebugLog('%d, %s, %s', [ aData.size, AnsiString( aData.data ), aData.ref ] );
  case aData.exKind of
    EX_BN:  // binance
      case aData.trDiv of
        TR_NEW_ORD : RequestBinFutNewOrder( AnsiString( aData.data ), aData.ref ) ;
        TR_CNL_ORD : RequestBinFutCnlOrder( AnsiString( aData.data ), aData.ref ) ;
        TR_REQ_POS : ;//

      end;
    EX_UP:  // upbit
      case aData.trDiv of
        TR_NEW_ORD : RequestUptNewOrder( AnsiString( aData.data ), aData.ref)  ;
        TR_CNL_ORD : RequestUptCnlOrder( AnsiString( aData.data ), aData.ref) ;
        TR_REQ_ORD : RequestUptOrderList( AnsiString( aData.data ), aData.ref) ;
        TR_REQ_BAL : RequestUptBalance( AnsiString( aData.data ), aData.ref)  ;
        TR_ORD_DETAIL : RequestUptOrderDetail( AnsiString( aData.data ), aData.ref) ;
        TR_ABLE_ORD : RequestUptAvailableOrder( AnsiString( aData.data ), aData.ref)  ;
        TR_TRD_AMT  : RequestUptOrderList( AnsiString( aData.data ), aData.ref, TR_TRD_AMT) ;  // 계좌 거래 대금
      end;
    EX_BI:  // bithumb
      case aData.trDiv of
        TR_NEW_ORD : RequestBitNewOrder( AnsiString( aData.data ), aData.ref );
        TR_CNL_ORD : RequestBitCnlOrder( AnsiString( aData.data ), aData.ref ) ;
        TR_REQ_ORD : RequestBitOrderList( AnsiString( aData.data ), aData.ref ); 		 // 주문 조회..
        TR_REQ_BAL : RequestBitBalance( AnsiString( aData.data ), aData.ref );       // 잔고 조회...
        TR_ORD_DETAIL : RequestBitOrderDetail( AnsiString( aData.data ), aData.ref );    // 주문 상세조회..  )
        TR_TRD_AMT    : RequestBitTradeAmt( AnsiString( aData.data ), aData.ref );    // 종목별 거래대금..  )
      end;
  end;

end;

procedure TRestManager.PushData( aExKind : TExchangeKind; aMarket : TMarketType;
   c3 : char; sData, sRef: string);
   var
    c1, c2 : char;
begin
  if Assigned( FOnPushData ) then
  begin
    case aExKind of
      ekBinance:c1 := EX_BN;
      ekUpbit:  c1 := EX_UP;
      ekBithumb:c1 := EX_BI;
    end;

    case aMarket of
      mtSpot:   c2 := 'S';
      mtFutures:c2 := 'F' ;
    end;
    
    FOnPushData( c1, c2, c3, sData, sRef );
  end;
end;

function TRestManager.Request(aExKind: TExchangeKind;
  AMethod: TRESTRequestMethod; AResource: string; var outJson,
  outRes: string): boolean;
begin

  Result := false;

  with FRestReq[aExKind] do
  begin
    Method   := AMethod;
    Resource := AResource;
  end;

  try
    try

      with FRestReq[aExKind] do
      begin
        Execute;

        OutJson:= Response.Content;

        if Response.StatusCode <> 200 then
        begin
          OutRes := Format( 'status : %d, %s', [ Response.StatusCode, Response.StatusText ] );
          Exit;
        end;

        Result  := true;
      end;

    except
      on E: Exception do
      begin
        OutRes := E.Message;
        Exit(false);
      end
    end;
  finally
    FRestReq[aExKind].Params.Clear;
    FRestReq[aExKind].Body.ClearBody;
  end;

end;

// bithumb api --------------------------------------------------------------------------------------

procedure TRestManager.RequestBinFutCnlOrder(sData, sRef: string);
var
  sArr  : TArray<string>;
  sTime, sBody, sSig, outJson, outRes : string;
begin
  if not CheckShareddData( sArr, sData, BC_CNT, 'BinFutCnlOrder') then Exit;

  sTime := GetTimestamp;
  sBody := Format('symbol=%s&orderId=%s&timestamp=%s',
    [ sArr[BC_CODE], sArr[BC_OID], sTime ]);

  sSig  := CalculateHMACSHA256(sBody,App.ApiConfig.GetSceretKey( ekBinance, mtFutures) );
  sBody := sBody + Format('&signature=%s', [ sSig ]);
  FRestReq[ekBinance].AddParameter('X-MBX-APIKEY',
      App.ApiConfig.GetApiKey( ekBinance, mtFutures) , pkHTTPHEADER );

  if not Request( ekBinance,rmDELETE, '/fapi/v1/order?'+sBody, outJson, outRes ) then
    App.Log( llError, '', 'Failed %s RequestBinFutCnlOrder (%s, %s)',
    [ TExchangeKindDesc[ekBinance], outRes, outJson] );



end;

procedure TRestManager.RequestBinFutNewOrder(sData, sRef: string);
var
  sArr  : TArray<string>;
  sRsrc, outJson, outRes : string;
  sTime, sBody, sSig : string;
begin
  if not CheckShareddData( sArr, sData, BO_CNT, 'BinFutNewOrder') then Exit;

  sBody := Format('symbol=%s&side=%s&type=%s&timeInForce=GTC&quantity=%s&price=%s&timestamp=%s',
    [ sArr[BO_CODE],sArr[BO_LS], sArr[BO_TYPE], sArr[BO_QTY], sArr[BO_PRC] , sTime ]);

  sSig  := CalculateHMACSHA256(sBody,App.ApiConfig.GetSceretKey( ekBinance, mtFutures) );
  sBody := sBody + Format('&signature=%s', [ sSig ]);
  FRestReq[ekBinance].AddParameter('X-MBX-APIKEY',
      App.ApiConfig.GetApiKey( ekBinance, mtFutures) , pkHTTPHEADER );

//  if Request( ekBinance, rmPOST, '/fapi/v1/order?'+sBody, outJson , outRes ) then
//  begin
//
//  end else
//  begin
//    App.Log( llError, '', 'Failed %s RequestBinFutNewOrder (%s, %s)',
//      [ TExchangeKindDesc[ekBinance], outRes, outJson] );
//  end;


  if not Request( ekBinance ,rmPOST, '/fapi/v1/order?'+sBody, outJson, outRes ) then
    App.Log( llError, '', 'Failed %s RequestBinFutNewOrder (%s, %s)',
    [ TExchangeKindDesc[ekBinance], outRes, outJson] );

  PushData( ekBinance, mtFutures, TR_NEW_ORD, outJson, sRef );

end;

procedure TRestManager.RequestBitBalance(sData, sRef: string);
var
  sArr  : TArray<string>;

	sRsrc, outJson, outRes, sTime, sBody, sVal, sSig : string;
  sParam1 : string;
  I: Integer;
  aExKind : TExchangeKind;
    
begin
  if not CheckShareddData( sArr, sData, TB_CNT, 'BitBalance') then Exit;

  try      
  	aExKind	:= ekBithumb;
    
    sParam1	:= sArr[TB_CODE];     
    sRsrc 	:= '/info/balance';     
    sTime 	:= GetTimestamp;    
    sVal		:= EncodePath( sRsrc, Format('endPoint=%s&currency=%s', [ sRsrc, sParam1 ] ), sTime );     

    sSig	:= CalculateHMACSHA512( sVal, App.ApiConfig.GetSceretKey( aExKind, mtSpot) );
    sBody	:= TIdEncoderMIME.EncodeString( sSig, IndyTextEncoding_UTF8 );  
		
    FRestReq[aExKind].AddParameter('Api-Key', App.ApiConfig.GetApiKey( aExKind, mtSpot), TRESTRequestParameterKind.pkHTTPHEADER );//, [poDoNotEncode]);
    FRestReq[aExKind].AddParameter('Api-Sign', sBody , TRESTRequestParameterKind.pkHTTPHEADER , [poDoNotEncode]);
    FRestReq[aExKind].AddParameter('Api-Nonce', sTime , TRESTRequestParameterKind.pkHTTPHEADER );

    FRestReq[aExKind].AddParameter('endPoint', sRsrc, TRESTRequestParameterKind.pkREQUESTBODY);
    FRestReq[aExKind].AddParameter('currency', sParam1, TRESTRequestParameterKind.pkREQUESTBODY);     

    if not Request( aExKind ,rmPOST, sRsrc, outJson, outRes ) then
      App.Log( llError, '', 'Failed %s RequestBitBalance (%s, %s)',
      [ TExchangeKindDesc[aExKind], outRes, outJson] );

   	PushData( aExKind, mtSpot, TR_REQ_BAL, outJson, sRef );
	except
  end;  
end;

procedure TRestManager.RequestBitCnlOrder(sData, sRef: string);
var
  sArr  : TArray<string>;

	sRsrc, outJson, outRes, sTime, sBody, sVal, sSig : string;   
  aExKind : TExchangeKind;
    
begin
  if not CheckShareddData( sArr, sData, TC_CNT, 'BitCnlOrder') then Exit;

  try      
  	aExKind	:= ekBithumb;      

    sRsrc 	:= '/trade/cancel';     
    sTime 	:= GetTimestamp;    
    sBody		:= Format('endPoint=%s&order_currency=%s&payment_currency=%s&order_id=%s&type=%s', [
    		sRsrc, sArr[TC_CODE], sArr[TC_STT], sArr[TC_OID], sArr[TC_LS]    	
    	]);              
    sVal		:= EncodePath( sRsrc, sBody, sTime );     

    sSig	:= CalculateHMACSHA512( sVal, App.ApiConfig.GetSceretKey( aExKind, mtSpot) );
    sBody	:= TIdEncoderMIME.EncodeString( sSig, IndyTextEncoding_UTF8 );  
		
    with  FRestReq[aExKind] do
    begin
                              
    	AddParameter('Api-Key', App.ApiConfig.GetApiKey( aExKind, mtSpot), TRESTRequestParameterKind.pkHTTPHEADER );//, [poDoNotEncode]);
    	AddParameter('Api-Sign', sBody , TRESTRequestParameterKind.pkHTTPHEADER , [poDoNotEncode]);
    	AddParameter('Api-Nonce', sTime , TRESTRequestParameterKind.pkHTTPHEADER );

    	AddParameter('endPoint', sRsrc, TRESTRequestParameterKind.pkREQUESTBODY);

  		AddParameter('order_currency', 	sArr[TC_CODE], 	TRESTRequestParameterKind.pkREQUESTBODY);
  		AddParameter('payment_currency',sArr[TC_STT], 	TRESTRequestParameterKind.pkREQUESTBODY);
  		AddParameter('order_id', 				sArr[TC_OID], 	TRESTRequestParameterKind.pkREQUESTBODY);
  		AddParameter('type', 						sArr[TC_LS], 		TRESTRequestParameterKind.pkREQUESTBODY);    
    end;

    if not Request( aExKind ,rmPOST, sRsrc, outJson, outRes ) then
      App.Log( llError, '', 'Failed %s RequestBitCnlOrder (%s, %s)',
      [ TExchangeKindDesc[aExKind], outRes, outJson] );       

   	PushData( aExKind, mtSpot, TR_CNL_ORD, outJson, sRef );              
	except
  end;  

end;

procedure TRestManager.RequestBitNewOrder(sData, sRef: string);
var
  sArr  : TArray<string>;

	sRsrc, outJson, outRes, sTime, sBody, sVal, sSig : string;
  sParam1 : string;
  I: Integer;
  aExKind : TExchangeKind;
    
begin
  if not CheckShareddData( sArr, sData, TO_CNT, 'BitNewOrder') then Exit;

  try      
  	aExKind	:= ekBithumb;    
   
    sRsrc 	:= '/trade/place';
    sTime 	:= GetTimestamp;    
    sBody		:= Format('endPoint=%s&order_currency=%s&payment_currency=%s&units=%s&price=%s&type=%s', [
    		sRsrc, sArr[TO_CODE], sArr[TO_STT], sArr[TO_QTY], sArr[TO_PRC], sArr[TO_LS]    	
    	]);
    	
    sVal		:= EncodePath( sRsrc, sBody,  sTime );     

    sSig		:= CalculateHMACSHA512( sVal, App.ApiConfig.GetSceretKey( aExKind, mtSpot) );
    sBody		:= TIdEncoderMIME.EncodeString( sSig, IndyTextEncoding_UTF8 );  
		
    FRestReq[aExKind].AddParameter('Api-Key', App.ApiConfig.GetApiKey( aExKind, mtSpot), TRESTRequestParameterKind.pkHTTPHEADER );//, [poDoNotEncode]);
    FRestReq[aExKind].AddParameter('Api-Sign', sBody , TRESTRequestParameterKind.pkHTTPHEADER , [poDoNotEncode]);
    FRestReq[aExKind].AddParameter('Api-Nonce', sTime , TRESTRequestParameterKind.pkHTTPHEADER );

    FRestReq[aExKind].AddParameter('endPoint', sRsrc, TRESTRequestParameterKind.pkREQUESTBODY);
    
    with FRestReq[aExKind] do
    begin
      AddParameter('order_currency', 		sArr[TO_CODE], TRESTRequestParameterKind.pkREQUESTBODY);
      AddParameter('payment_currency', 	sArr[TO_STT], TRESTRequestParameterKind.pkREQUESTBODY);
      AddParameter('units', sArr[TO_QTY], TRESTRequestParameterKind.pkREQUESTBODY);
      AddParameter('price', sArr[TO_PRC], TRESTRequestParameterKind.pkREQUESTBODY);
      AddParameter('type',  sArr[TO_LS], TRESTRequestParameterKind.pkREQUESTBODY);
    end;

    if not Request( aExKind ,rmPOST, sRsrc, outJson, outRes ) then
      App.Log( llError, '', 'Failed %s RequestBitNewOrder (%s, %s)',
      [ TExchangeKindDesc[aExKind], outRes, outJson] );       

   	PushData( aExKind, mtSpot, TR_NEW_ORD, outJson, sRef );              
	except
  end;  
end;



procedure TRestManager.RequestBitOrderDetail(sData, sRef: string);
var
  sArr  : TArray<string>;

	sRsrc, outJson, outRes, sTime, sBody, sVal, sSig : string;
  I: Integer;
  aExKind : TExchangeKind;
    
begin
  if not CheckShareddData( sArr, sData, TD_CNT, 'BitOrderDetail') then Exit;

  try      
  	aExKind	:= ekBithumb;
    sRsrc 	:= '/info/order_detail';     
    sTime 	:= GetTimestamp;        
    sVal		:= EncodePath( sRsrc, Format('endPoint=%s&order_id=%s&order_currency=%s', 
      [ sRsrc, sArr[TD_OID], sArr[TD_CODE]] ), sTime )  ;

    sSig	:= CalculateHMACSHA512( sVal, App.ApiConfig.GetSceretKey( aExKind, mtSpot) );
    sBody	:= TIdEncoderMIME.EncodeString( sSig, IndyTextEncoding_UTF8 );  
		
    with FRestReq[aExKind] do
    begin
    	AddParameter('Api-Key', App.ApiConfig.GetApiKey( aExKind, mtSpot), TRESTRequestParameterKind.pkHTTPHEADER );//, [poDoNotEncode]);
    	AddParameter('Api-Sign', sBody , TRESTRequestParameterKind.pkHTTPHEADER , [poDoNotEncode]);
    	AddParameter('Api-Nonce', sTime , TRESTRequestParameterKind.pkHTTPHEADER );

    	AddParameter('endPoint', sRsrc, TRESTRequestParameterKind.pkREQUESTBODY);
    	AddParameter('order_id', 			sArr[TD_OID] , TRESTRequestParameterKind.pkREQUESTBODY);     
    	AddParameter('order_currency',sArr[TD_CODE], TRESTRequestParameterKind.pkREQUESTBODY);     
    end;

    if not Request( aExKind ,rmPOST, sRsrc, outJson, outRes ) then
      App.Log( llError, '', 'Failed %s RequestBitOrderDetail (%s, %s)',
      [ TExchangeKindDesc[aExKind], outRes, outJson] );       

   	PushData( aExKind, mtSpot, TR_ORD_DETAIL, outJson, sRef );              
	except
  end;  

end;

procedure TRestManager.RequestBitOrderList(sData, sRef: string);
var
  sArr  : TArray<string>;

	sRsrc, outJson, outRes, sTime, sBody, sVal, sSig : string;
  sParam1 : string;
  I: Integer;
  aExKind : TExchangeKind;

begin
  if not CheckShareddData( sArr, sData, TL_CNT, 'BitOrderList') then Exit;

  try
  	aExKind	:= ekBithumb;

    sParam1	:= sArr[TL_CODE];
//    sParam2 := trim( sArr[TL_OID] );
    sRsrc 	:= '/info/orders';
    sTime 	:= GetTimestamp;

//    if sParam2 = '' then
	    sVal	:= EncodePath( sRsrc, Format('endPoint=%s&order_currency=%s', [ sRsrc, sParam1] ), sTime )  ;
//    else begin
//	    sVal	:= EncodePath( sRsrc, Format('endPoint=%s&order_id=%s&order_currency=%s', [ sRsrc, sParam2, sParam1 ] ), sTime );
//      FRestReq[aExKind].AddParameter('order_id', sParam2, TRESTRequestParameterKind.pkREQUESTBODY);
//    end;

    sSig	:= CalculateHMACSHA512( sVal, App.ApiConfig.GetSceretKey( aExKind, mtSpot) );
    sBody	:= TIdEncoderMIME.EncodeString( sSig, IndyTextEncoding_UTF8 );

    FRestReq[aExKind].AddParameter('Api-Key', App.ApiConfig.GetApiKey( aExKind, mtSpot), TRESTRequestParameterKind.pkHTTPHEADER );//, [poDoNotEncode]);
    FRestReq[aExKind].AddParameter('Api-Sign', sBody , TRESTRequestParameterKind.pkHTTPHEADER , [poDoNotEncode]);
    FRestReq[aExKind].AddParameter('Api-Nonce', sTime , TRESTRequestParameterKind.pkHTTPHEADER );

    FRestReq[aExKind].AddParameter('endPoint', sRsrc, TRESTRequestParameterKind.pkREQUESTBODY);
    FRestReq[aExKind].AddParameter('order_currency', sParam1, TRESTRequestParameterKind.pkREQUESTBODY);

    if not Request( aExKind ,rmPOST, sRsrc, outJson, outRes ) then
      App.Log( llError, '', 'Failed %s RequestBitOrderList (%s, %s)',
      [ TExchangeKindDesc[aExKind], outRes, outJson] );

   	PushData( aExKind, mtSpot, TR_REQ_ORD, outJson, sRef );
	except
  end;

end;

procedure TRestManager.RequestBitTradeAmt(sData, sRef: string);
var
  sArr  : TArray<string>;

	sRsrc, outJson, outRes, sTime, sBody, sVal, sSig : string;
  sParam1 : string;
  I: Integer;
  aExKind : TExchangeKind;

begin
  if not CheckShareddData( sArr, sData, TL_CNT, 'BitTradeAmt') then Exit;

  try
  	aExKind	:= ekBithumb;

    sParam1	:= sArr[TL_CODE];

    sRsrc 	:= '/info/ticker';
    sTime 	:= GetTimestamp;

    sVal	:= EncodePath( sRsrc, Format('endPoint=%s&order_currency=%s', [ sRsrc, sParam1] ), sTime )  ;
    sSig	:= CalculateHMACSHA512( sVal, App.ApiConfig.GetSceretKey( aExKind, mtSpot) );
    sBody	:= TIdEncoderMIME.EncodeString( sSig, IndyTextEncoding_UTF8 );

    FRestReq[aExKind].AddParameter('Api-Key', App.ApiConfig.GetApiKey( aExKind, mtSpot), TRESTRequestParameterKind.pkHTTPHEADER );//, [poDoNotEncode]);
    FRestReq[aExKind].AddParameter('Api-Sign', sBody , TRESTRequestParameterKind.pkHTTPHEADER , [poDoNotEncode]);
    FRestReq[aExKind].AddParameter('Api-Nonce', sTime , TRESTRequestParameterKind.pkHTTPHEADER );

    FRestReq[aExKind].AddParameter('endPoint', sRsrc, TRESTRequestParameterKind.pkREQUESTBODY);
    FRestReq[aExKind].AddParameter('order_currency', sParam1, TRESTRequestParameterKind.pkREQUESTBODY);

    if not Request( aExKind ,rmPOST, sRsrc, outJson, outRes ) then
      App.Log( llError, '', 'Failed %s RequestBitTradeAmt (%s, %s)',
      [ TExchangeKindDesc[aExKind], outRes, outJson] );

   	PushData( aExKind, mtSpot, TR_TRD_AMT, outJson, sRef );
	except
  end;
end;

// bithumb api --------------------------------------------------------------------------------------

// upbit api --------------------------------------------------------------------------------------

procedure TRestManager.RequestUptAvailableOrder(sData, sRef: string);
var
  sArr  : TArray<string>;
  aExKind : TExchangeKind;

  LToken: TJWT;
  guid : TGUID;     vHash : THashSHA2;
  sSig, sToken, sQuery, outRes, sRsrc, outJson : string;
begin

  if not CheckShareddData( sArr, sData, UA_CNT, 'UptAvailableOrder') then Exit;

  aExKind := ekUpbit;

  LToken:= TJWT.Create(TJWTClaims);
  try

    sQuery := 'market='+sArr[UA_CODE];
    sRsrc  := '/v1/orders/chance?'+sQuery;

    LToken.Claims.SetClaimOfType<string>('access_key', App.ApiConfig.GetApiKey( aExKind, mtSpot));
    LToken.Claims.SetClaimOfType<string>('nonce', GetUUID );
    LToken.Claims.SetClaimOfType<string>('query_hash', vHash.gethashstring( sQuery, SHA512 ) );
    LToken.Claims.SetClaimOfType<string>('query_hash_alg', 'SHA512' );

    sSig  := TJOSE.SerializeCompact(App.ApiConfig.GetSceretKey( aExKind, mtSpot),
             TJOSEAlgorithmId.HS256, LToken);
    sToken:= Format('Bearer %s', [sSig ]);

    with FRestReq[aExKind] do
    begin
      AddParameter('Authorization', sToken, TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode] );
    end;

    if not Request( aExKind ,rmGET, sRsrc, outJson, outRes ) then
      App.Log( llError, '', 'Failed %s RequestBitOrderList (%s, %s)',
      [ TExchangeKindDesc[aExKind], outRes, outJson] );

    PushData( aExKind, mtSpot, TR_ABLE_ORD, outJson, sRef );

  finally
    LToken.Free;
  end;
end;

procedure TRestManager.RequestUptBalance(sData, sRef: string);
var
  sArr  : TArray<string>;
  aExKind : TExchangeKind;

  LToken: TJWT;
  guid : TGUID;
  sSig, sID, sToken, outRes, sRsrc, outJson : string;
begin

  //if not CheckShareddData( sArr, sData, TL_CNT, 'UptBalance') then Exit;

  aExKind := ekUpbit;

  LToken:= TJWT.Create(TJWTClaims);
  try
    sID := GetUUID;
    sRsrc := '/v1/accounts';

    LToken.Claims.SetClaimOfType<string>('access_key', App.ApiConfig.GetApiKey( aExKind, mtSpot));
    LToken.Claims.SetClaimOfType<string>('nonce', sID );
    sSig  := TJOSE.SerializeCompact(App.ApiConfig.GetSceretKey( aExKind, mtSpot),
             TJOSEAlgorithmId.HS256, LToken);
    sToken:= Format('Bearer %s', [sSig ]);

    with FRestReq[aExKind] do
    begin
      AddParameter('Authorization', sToken, TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode] );
    end;

    if not Request( aExKind ,rmGET, sRsrc, outJson, outRes ) then
      App.Log( llError, '', 'Failed %s RequestBitOrderList (%s, %s)',
      [ TExchangeKindDesc[aExKind], outRes, outJson] );

    PushData( aExKind, mtSpot, TR_REQ_BAL, outJson, sRef );

  finally
    LToken.Free;
  end;

end;

procedure TRestManager.RequestUptCnlOrder(sData, sRef: string);
var
  sArr  : TArray<string>;
  aExKind : TExchangeKind;

  LToken: TJWT;
  guid : TGUID;     vHash : THashSHA2;
  sSig, sToken, sQuery, outRes, sRsrc, outJson : string;
begin

  if not CheckShareddData( sArr, sData, UC_CNT, 'UptCnlOrder') then Exit;
  aExKind := ekUpbit;
  LToken:= TJWT.Create(TJWTClaims);
  try

    sQuery := 'uuid='+sArr[UC_UID];
    sRsrc  := '/v1/order?'+sQuery;

    LToken.Claims.SetClaimOfType<string>('access_key', App.ApiConfig.GetApiKey( aExKind, mtSpot));
    LToken.Claims.SetClaimOfType<string>('nonce', GetUUID );
    LToken.Claims.SetClaimOfType<string>('query_hash', vHash.gethashstring( sQuery, SHA512 ) );
    LToken.Claims.SetClaimOfType<string>('query_hash_alg', 'SHA512' );

    sSig  := TJOSE.SerializeCompact(App.ApiConfig.GetSceretKey( aExKind, mtSpot),
             TJOSEAlgorithmId.HS256, LToken);
    sToken:= Format('Bearer %s', [sSig ]);

    with FRestReq[aExKind] do
    begin
      AddParameter('Authorization', sToken, TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode] );
    end;

    if not Request( aExKind ,rmDELETE, sRsrc, outJson, outRes ) then
      App.Log( llError, '', 'Failed %s RequestUptCnlOrder (%s, %s)',
      [ TExchangeKindDesc[aExKind], outRes, outJson] );

    PushData( aExKind, mtSpot, TR_CNL_ORD, outJson, sRef );

  finally
    LToken.Free;
  end;
end;

function TRestManager.RequestUptFailMessage( sReq : string ): string;
begin
  Result := '{"error":{"name": "request_failed", "message": "'+sReq+' request failed."}}';
end;

procedure TRestManager.RequestUptNewOrder(sData, sRef: string);
var
  sArr  : TArray<string>;
  aExKind : TExchangeKind;

  LToken: TJWT;
  guid : TGUID;     vHash : THashSHA2;
  sSig, sToken, sQuery, outRes, sRsrc, outJson : string;
  aObj : TJsonObject;
begin

  if not CheckShareddData( sArr, sData, UO_CNT, 'UptNewOrder') then Exit;
  aExKind := ekUpbit;

  aObj  := TJsonObject.Create;
  LToken:= TJWT.Create(TJWTClaims);
  try

    sQuery := format('market=%s&side=%s&price=%s&volume=%s&order_type=%s', [
      sArr[UO_CODE], sArr[UO_LS], sArr[UO_PRC], sArr[UO_QTY], sArr[UO_TYPE]
      ]);
    sRsrc  := '/v1/orders?'+sQuery;

    with aObj do
    begin
      AddPair('market', sArr[UO_CODE] );
      AddPair('side',   sArr[UO_LS]);
      AddPair('price',  sArr[UO_PRC]);
      AddPair('volume', sArr[UO_QTY]);
      AddPair('order_type',sArr[UO_TYPE]);
    end;

    LToken.Claims.SetClaimOfType<string>('access_key', App.ApiConfig.GetApiKey( aExKind, mtSpot));
    LToken.Claims.SetClaimOfType<string>('nonce', GetUUID );
    LToken.Claims.SetClaimOfType<string>('query_hash', vHash.gethashstring( sQuery, SHA512 ) );
    LToken.Claims.SetClaimOfType<string>('query_hash_alg', 'SHA512' );

    sSig  := TJOSE.SerializeCompact(App.ApiConfig.GetSceretKey( aExKind, mtSpot),
             TJOSEAlgorithmId.HS256, LToken);
    sToken:= Format('Bearer %s', [sSig ]);

    with FRestReq[aExKind] do
    begin
      AddParameter('Authorization', sToken, TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode] );
      Body.Add(aObj);
    end;

    if not Request( aExKind ,rmPOST, sRsrc, outJson, outRes ) then
      App.Log( llError, '', 'Failed %s RequestUptNewOrder (%s, %s)',
      [ TExchangeKindDesc[aExKind], outRes, outJson] );

    PushData( aExKind, mtSpot, TR_NEW_ORD, outJson, sRef );

  finally
    LToken.Free;
    aObj.Free;
  end;
end;

procedure TRestManager.RequestUptOrderDetail(sData, sRef: string);
var
  sArr{, sUids}  : TArray<string>;
  aExKind : TExchangeKind;

  LToken: TJWT;
  guid : TGUID;     vHash : THashSHA2;
  sSig, sToken, sQuery, outRes, sRsrc, outJson : string;
  //I: Integer;
begin

  if not CheckShareddData( sArr, sData, UD_CNT, 'UptOrderDetail') then Exit;

  aExKind := ekUpbit;
  LToken:= TJWT.Create(TJWTClaims);

  try

    try
  //    sUids  := sArr[UD_UID].Split([',']);
      sQuery := 'uuid='+ sArr[UD_UID];
  //    for I := 0 to High(sUids) do
  //    begin
  //      if i > 0 then
  //        sQuery := sQuery + '&';
  //      sQuery := sQuery + 'uuids[]=' + sUids[i]
  //    end;
      sRsrc  := '/v1/order?'+sQuery;

      LToken.Claims.SetClaimOfType<string>('access_key', App.ApiConfig.GetApiKey( aExKind, mtSpot));
      LToken.Claims.SetClaimOfType<string>('nonce', GetUUID );
      LToken.Claims.SetClaimOfType<string>('query_hash', vHash.gethashstring( sQuery, SHA512 ) );
      LToken.Claims.SetClaimOfType<string>('query_hash_alg', 'SHA512' );

      sSig  := TJOSE.SerializeCompact(App.ApiConfig.GetSceretKey( aExKind, mtSpot),
               TJOSEAlgorithmId.HS256, LToken);
      sToken:= Format('Bearer %s', [sSig ]);

      with FRestReq[aExKind] do
      begin
        AddParameter('Authorization', sToken, TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode] );
      end;

      if not Request( aExKind ,rmGET, sRsrc, outJson, outRes ) then
        App.Log( llError, '', 'Failed %s RequestUptOrderDetail (%s, %s)',
        [ TExchangeKindDesc[aExKind], outRes, outJson] );

    except
      outJson := RequestUptFailMessage('OrderDetail');
    end;

    PushData( aExKind, mtSpot, TR_ORD_DETAIL, outJson, sRef );

  finally
    LToken.Free;
  end;


end;

procedure TRestManager.RequestUptOrderList(sData, sRef: string; cDiv : char);
var
  sArr  : TArray<string>;
  aExKind : TExchangeKind;

  LToken: TJWT;
  guid : TGUID;     vHash : THashSHA2;
  sSig, sToken, sQuery, outRes, sRsrc, outJson : string;
begin

  if not CheckShareddData( sArr, sData, UL_CNT, 'UptOrderList') then Exit;

  aExKind := ekUpbit;

  LToken:= TJWT.Create(TJWTClaims);
  try

    try
      sQuery := format('state=%s&order_by=%s', [ sArr[UL_STATE], sArr[UL_ASC] ]);
      sRsrc  := '/v1/orders?'+sQuery;

      LToken.Claims.SetClaimOfType<string>('access_key', App.ApiConfig.GetApiKey( aExKind, mtSpot));
      LToken.Claims.SetClaimOfType<string>('nonce', GetUUID );
      LToken.Claims.SetClaimOfType<string>('query_hash', vHash.gethashstring( sQuery, SHA512 ) );
      LToken.Claims.SetClaimOfType<string>('query_hash_alg', 'SHA512' );

      sSig  := TJOSE.SerializeCompact(App.ApiConfig.GetSceretKey( aExKind, mtSpot),
               TJOSEAlgorithmId.HS256, LToken);
      sToken:= Format('Bearer %s', [sSig ]);

      with FRestReq[aExKind] do
      begin
        AddParameter('Authorization', sToken, TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode] );
      end;

      if not Request( aExKind ,rmGET, sRsrc, outJson, outRes ) then
        App.Log( llError, '', 'Failed %s RequestUptOrderList (%s, %s)',
        [ TExchangeKindDesc[aExKind], outRes, outJson] );

      PushData( aExKind, mtSpot, cDiv, outJson, sRef );
    except
    end;

  finally
    LToken.Free;
  end;

end;

// upbit api --------------------------------------------------------------------------------------


end.
