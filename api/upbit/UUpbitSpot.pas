unit UUpbitSpot;

interface

uses
  System.Classes, System.SysUtils, System.DateUtils,

  System.JSON,  Rest.Json , Rest.Types , Rest.Client,

  UExchange, USymbols,

  UApiTypes

  ;

type
  TUpbitSpot = class( TExchange )
  private

    FIndex  : int64;
    FLastIndex : integer;

    FLimitSec: integer;
    FLimitMin: integer;
    FInterval: integer;
    FEnable: boolean;
    FMarketParam: string;
    procedure ReceiveDNWState ;
    procedure ParseRateLimit(sTmp: string);
    function RequestSpotTicker : boolean;

    procedure OnHTTPProtocolError(Sender: TCustomRESTRequest);
    procedure MakeRest;

//    procedure parseAssetsstatus;
//    procedure parseOrderBook;
//    procedure parseTicker;    

  public
    Constructor Create( aObj : TObject; aMarketType : TMarketType );
    Destructor  Destroy; override;

    procedure RequestData;

    function ParsePrepareMaster : integer; override;
    function RequestMaster : boolean ; override;
    function RequestCandleData( sUnit : string; sCode : string ) : boolean; override;
		// Ÿ�̸Ӹ� ���� ��ȸ    
    procedure ParseRequestData( iCode : integer; sName : string; sData : string ); override;

    // private api
    function RequestAccounts : boolean;
    function RequestWaitOrders : boolean;
    procedure RequestAvailableAmt( aSymbol : TSymbol );
    
    
    procedure CyclicNotify( Sender : TObject ); override;
    procedure RestNotify( Sender : TObject ); override;

    function RequestDNWState : boolean; override;
    function GetSig( idx : Integer ): string; overload;
    function GetSig( sQuery : string ): string; overload;

    property  LimitSec : integer read FLimitSec;
    property  LimitMin : integer read FLimitMin;
    property  Interval : integer read FInterval;
    property  Enable   : boolean read FEnable;

      // ������Ʈ�� ���� ( ��� ���� )
    property  MarketParam : string read FMarketParam;
  end;

implementation

uses
  GApp  , UApiConsts
  , UEncrypts
  , UUpbitParse, URestItems 
  , UCyclicItems, URestRequests
  , JOSE.Core.JWT
  , JOSE.Core.JWA
  , JOSE.Core.Builder
  , Math
  , System.Hash
  ;

{ TBinanceSpotNMargin }

constructor TUpbitSpot.Create(aObj: TObject; aMarketType: TMarketType);
begin
  inherited Create( aObj, aMarketType );
  FLimitSec := 30;
  FLimitMin := 1;
  FEnable   := true;

  FIndex  := 0;
  FLastIndex := -1;
end;

destructor TUpbitSpot.Destroy;
begin

  inherited;
end;





function TUpbitSpot.ParsePrepareMaster: integer;
var
  master : TJsonArray;
  aObj : TJsonObject;
  i, iLen : Integer;
  stTmp : string;
  sts   : TArray<string>;
begin

  try

  master :=  TJsonObject.ParseJSONValue( MasterData) as TJsonArray;

    for I := 0 to master.Size-1 do
    begin
      aObj := master.Get(i) as TJsonObject;
      stTmp:= aObj.GetValue('market').Value;

      sts := stTmp.Split(['-']);
      iLen:= Length(sts);
      if iLen <= 1 then continue;

      if sts[0] = 'KRW' then begin
        Codes.Add( sts[1] );
      end;
    end;
  finally
    if master <> nil then master.Free;
  end;
end;


function TUpbitSpot.RequestMaster: boolean;
begin

  Result :=  RequestSpotTicker
      and  RequestDNWState       ;      

	if Result then
		MakeRest;
end;

	// pub query       �д� 600ȸ, �ʴ� 10ȸ (����, ĵ��, ü��, ƼĿ, ȣ����)
	// pri query       �ʴ� 30ȸ, �д� 900ȸ   -- �д� 30�ΰ� ����..
  // order           �ʴ� 8ȸ, �д� 200ȸ





function TUpbitSpot.RequestSpotTicker: boolean;
var
  aList : TStringList;
  sOut, sJson, sData  : string;
  I: Integer;
begin
  aList := TStringList.Create;
  try
    GetCodeList(aList);
    if aList.Count <= 0 then Exit;
    FMarketParam := '';
    for I := 0 to aList.Count-1 do
    begin
      FMarketParam := FMarketParam + 'KRW-'+aList[i];
      if i < aList.Count-1  then
        FMarketParam := FMarketParam + ','
    end;

    SetBaseUrl( App.Engine.ApiConfig.GetBaseUrl( GetExKind , mtSpot ) );
    SetParam('markets', FMarketParam );


    if Request( rmGET, 'v1/ticker', '', sJson, sOut ) then
    begin         //App.Log( llDebug, '', '%s (%s, %s)', [ TExchangeKindDesc[GetExKind], sOut, sJson] );

      gUpReceiver.ParseSpotTicker( sJson );
      sleep(100);
    end else
    begin
      App.Log( llError, '', 'Failed %s RequestMaster (%s, %s)',
        [ TExchangeKindDesc[GetExKind], sOut, sJson] );
      Exit( false );
    end;

//    sTmp := '';
//    for I := 0 to aList.Count-1 do
//    begin
//      sTmp := sTmp + Format('"KRW-%s"', [aList[i]]);
//      if i < aList.Count-1  then
//        sTmp := sTmp + ','
//    end;

    Result := App.Engine.SymbolCore.Symbols[ GetExKind].Count > 0 ;

  finally
    aList.Free;
  end;

end;





procedure TUpbitSpot.ReceiveDNWState;
var
  sTmp : string;
begin
  sTmp := RestReq.Response.Headers.Values['Remaining-Req'];
  App.DebugLog('remain-req', [ sTmp] );
//  gUpReceiver.ParseDNWSate( RestReq.Response.Content );
end;

procedure TUpbitSpot.ParseRateLimit( sTmp : string );
var
  sts : TArray<string>;
  min, sec : string;
begin
  if sTmp = '' then Exit;

  sts := sTmp.Split([';']);
  sec := trim( sts[2] );
  min := trim( sts[1] );

  sts := sec.Split(['=']);
  FLimitSec := StrToIntDef( trim( sts[1] ), 1 );
  sts := min.Split(['=']);
  FLimitMin := StrToIntDef( trim( sts[1] ), 1 );
end;


procedure TUpbitSpot.ParseRequestData(iCode: integer; sName, sData: string);
begin
  inherited;

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
			gUpReceiver.ParseSpotOrderBook( sData )
    else if sName = 'ticker' then
    	gUpReceiver.ParseSpotTicker2(sData)
    else if sName = 'status' then begin
//      App.Log(llInfo, 'test', '%d, %s : %s', [ iCode, sName, Copy(sData, 1, 100 ) ]  );
    	gUpReceiver.ParseDNWSate( sData );
    end;

end;

procedure TUpbitSpot.RequestData;
var
	aReq : TReqeustItem;
  i, idx : integer;
begin

	for I := 0 to 2 do
  begin

    if ( i = 2 ) and ( FIndex mod 4 = 0 ) then
    	continue;
        
    aReq := TReqeustItem.Create;
    aReq.AMethod	:= rmGET;
    aReq.Req.init( App.Engine.ApiConfig.GetBaseUrl( GetExKind , mtSpot ) );      
    	

    case i of
      0 : begin aReq.AResource:= '/v1/orderbook';   	idx	:= RestType(PUB_REQ);  
      					aReq.Name := 'orderbook'; end;
      1 : begin aReq.AResource:= '/v1/ticker';				idx := RestType(PUB_REQ); 
     						aReq.Name := 'ticker'; 		end;
      2 : begin aReq.AResource:= '/v1/status/wallet'; idx := RestType(PRI_REQ);
      					aReq.Name := 'status'; 		end;
    end;

    if i = 2 then
    begin
      var sToken : string;
      sToken := GetSig(i);
      aReq.Req.Req.AddParameter('Authorization', sToken, TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode] );
     // App.DebugLog('------');
    end else
    	aReq.Req.Req.AddParameter('markets', FMarketParam, pkGETorPOST);
    
//    if Rest[idx] <> nil then
//    	Rest[idx].PushQueue( aReq );  
  end;


  inc( FIndex );
  if FIndex > (High(int64) - 1000) then
  	FIndex := 0;
  
	
	
//  // ȣ�� , ƼĿ, �����.
//  if ( FIndex <> FLastIndex )
//    or ( RestResult = nil )
//    or (( RestResult <> nil ) and ( RestResult.Finished )) then
//  begin
//    FLastIndex := FIndex;
//
//    case FIndex of
//      0 , 2: begin
//        Req[0].Req.AddParameter('markets', FMarketParam, pkGETorPOST);
//        RestResult := Req[0].RequestAsync( parseOrderBook, rmGET, '/v1/orderbook', true);
//      end;
//      1 : begin
//        RestResult := RequestAsyncDnwState(1);
//      end;
//      3 :begin
//        Req[2].Req.AddParameter('markets', FMarketParam, pkGETorPOST);
//        RestResult := Req[2].RequestAsync( parseTicker, rmGET, 'v1/ticker', true);
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



//procedure TUpbitSpot.parseTicker;
//var
//  sJson : string;
//begin
//  sJson :=  Req[2].GetResponse;
//  if sJson = '' then Exit;
//  gUpReceiver.ParseSpotTicker2( sJson );
//end;
//
//procedure TUpbitSpot.parseAssetsstatus;
//var
//  sJson : string;
//begin
//  sJson :=  Req[1].GetResponse;
//  if sJson = '' then Exit;
//  gUpReceiver.ParseDNWSate( sJson );
//end;
//
//procedure TUpbitSpot.parseOrderBook;
//var
//  sJson : string;
//begin
//  sJson :=  Req[0].GetResponse;
//  if sJson = '' then Exit;
//  gUpReceiver.ParseSpotOrderBook( sJson );
//
//end;

function TUpbitSpot.GetSig( idx : Integer ): string;
var
  LToken: TJWT;
  guid : TGUID;
  sSig, sID : string;
begin

  LToken:= TJWT.Create(TJWTClaims);

  try

    sID := GetUUID;

    LToken.Claims.SetClaimOfType<string>('access_key', App.Engine.ApiConfig.GetApiKey( GetExKind , mtSpot ));
    LToken.Claims.SetClaimOfType<string>('nonce', sID );

    sSig := TJOSE.SerializeCompact(  App.Engine.ApiConfig.GetSceretKey( GetExKind , mtSpot )
      ,  TJOSEAlgorithmId.HS256, LToken);
    Result := Format('Bearer %s', [sSig ]);   
  finally
    LToken.Free;
  end;
end;

function TUpbitSpot.GetSig(sQuery: string): string;
var
  LToken: TJWT;
  guid : TGUID;
  sSig, sID, sToken : string;
  vHash : THashSHA2;
begin

  LToken:= TJWT.Create(TJWTClaims);

  try

    sID := GetUUID;

    Result := vHash.gethashstring( sQuery, SHA512 );      

    LToken.Claims.SetClaimOfType<string>('access_key', App.Engine.ApiConfig.GetApiKey( GetExKind , mtSpot ));
    LToken.Claims.SetClaimOfType<string>('nonce', sID );
    LToken.Claims.SetClaimOfType<string>('query_hash', Result );    
    LToken.Claims.SetClaimOfType<string>('query_hash_alg', 'SHA512' );        

    sSig := TJOSE.SerializeCompact(  App.Engine.ApiConfig.GetSceretKey( GetExKind , mtSpot )
      ,  TJOSEAlgorithmId.HS256, LToken);
    sToken := Format('Bearer %s', [sSig ]);  

    RestReq.AddParameter('Authorization', sToken, TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode] );      
    
  finally
    LToken.Free;
  end;

end;

function TUpbitSpot.RequestAccounts : boolean;
var
//	aReq : TRequest;
  sSig ,sJson, sOut: string;
  bRes : boolean;
begin 

//  aReq	:= GetReqItems;
//  if (aReq <> nil) and ( aReq.State <> 1 ) then
//  begin
//    aReq.Req.Params.Clear;
//    sSig	:= GetSig(0);
//    aReq.Req.AddParameter('Authorization', sSig, TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode] );
//
//    aReq.SetParam( rmGET, '/v1/accounts', 'account' );
//
//    if RestThread <> nil then
//      RestThread.PushQueue( aReq );        
//  end;
	bRes := false;

  try
    SetBaseUrl( App.Engine.ApiConfig.GetBaseUrl( GetExKind , mtSpot ) );  
    sSig	:= GetSig(0);
    RestReq.AddParameter('Authorization', sSig, TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode] );

    if Request( rmGET, '/v1/accounts', '', sJson, sOut ) then
    begin
      gUpReceiver.ParseAccounts( sJson );
    end else
    begin
      App.Log( llError, '', 'Failed %s RequestAccounts (%s, %s)',
        [ TExchangeKindDesc[GetExKind], sOut, sJson] );
      Exit (bRes);
    end;

    bRes := true;    
    
  except
  end;

  Result := bRes;
end;

function TUpbitSpot.RequestWaitOrders: boolean;
var
  sQryStr ,sJson, sQuery, sOut: string;
  bRes : boolean;
begin
	bRes := false;

  try
    SetBaseUrl( App.Engine.ApiConfig.GetBaseUrl( GetExKind , mtSpot ) );  
    sQuery  := 'state=wait';
    sQryStr	:= GetSig(sQuery);

    if Request( rmGET, '/v1/orders?'+sQuery, '', sJson, sOut ) then
    begin
      gUpReceiver.ParseAccounts( sJson );
    end else
    begin
      App.Log( llError, '', 'Failed %s RequestWaitOrders (%s, %s)',
        [ TExchangeKindDesc[GetExKind], sOut, sJson] );
      Exit (bRes);
    end;

    bRes := true;    
    
  except
  end;

  Result := bRes;
end;

procedure TUpbitSpot.RequestAvailableAmt(aSymbol: TSymbol);
begin

end;

function TUpbitSpot.RequestCandleData(sUnit, sCode: string): boolean;
var
  sJson, sOut : string;
  bRes : boolean;
begin

  bRes := false;

  try

    SetBaseUrl( App.Engine.ApiConfig.GetBaseUrl( GetExKind , mtSpot ) );
    SetParam('market', sCode );
    SetParam('count', '150' );

    if Request( rmGET, 'v1/candles/minutes/'+sUnit, '', sJson, sOut ) then
    begin
      gUpReceiver.ParseCandleData( sUnit, sJson );
    end else
    begin
      App.Log( llError, '', 'Failed %s RequestCandleData (%s, %s)',
        [ TExchangeKindDesc[GetExKind], sOut, sJson] );
      Exit( false );
    end;

    bRes := true;
  except
  end;

  result := bRes;
end;

function TUpbitSpot.RequestDNWState : boolean;
var
  LToken: TJWT;
  sSig, sID, sToken, sOut, sJson : string;
begin

  LToken:= TJWT.Create(TJWTClaims);

  try

    sID := GetUUID;

    LToken.Claims.SetClaimOfType<string>('access_key', App.Engine.ApiConfig.GetApiKey( GetExKind , mtSpot ));
    LToken.Claims.SetClaimOfType<string>('nonce', sID );

    sSig := TJOSE.SerializeCompact(  App.Engine.ApiConfig.GetSceretKey( GetExKind , mtSpot )
      ,  TJOSEAlgorithmId.HS256, LToken);
    sToken := Format('Bearer %s', [sSig ]);

    RestReq.AddParameter('Authorization', sToken, TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode] );

    if Request( rmGET, '/v1/status/wallet', '', sJson, sOut ) then
    begin         //App.Log( llDebug, '', '%s (%s, %s)', [ TExchangeKindDesc[GetExKind], sOut, sJson] );
      gUpReceiver.ParseDNWSate( sJson );
    end else
    begin
      App.Log( llError, '', 'Failed %s RequestDNWState (%s, %s)',
        [ TExchangeKindDesc[GetExKind], sOut, sJson] );
      Exit (false);
    end;

    Result := true;

  finally
    LToken.Free;
  end;

end;

procedure TUpbitSpot.OnHTTPProtocolError(Sender: TCustomRESTRequest);
begin
  if Sender <> nil  then
  begin
    App.Log( llError,  '%s Async Request Error : %s ( status : %d, %s)' , [ TExchangeKindDesc[GetExKind]
    ,  Sender.Response.Content ,  Sender.Response.StatusCode, Sender.Response.StatusText ]  );
  end;
end;


procedure TUpbitSpot.MakeRest;
var
  aItem : TCyclicItem;

begin

  aItem := CyclicItems.New('orderbook');
  aItem.Interval  := 1000;
  aItem.Index     := 0;
  aItem.Resource	:= '/v1/orderbook';
  aITem.Method		:= rmGET;

  aItem := CyclicItems.New('ticker');
  aItem.Interval  := 1000;
  aItem.Index     := 1;
  aItem.Resource	:= '/v1/ticker';
  aITem.Method		:= rmGET;

  aItem := CyclicItems.New('status');
  aItem.Interval  := 3000;
  aItem.Index     := 2;
  aItem.Resource	:= '/v1/status/wallet';
  aITem.Method		:= rmGET;
	// request item �� �޸𸮿� �̸� ������..����/���� ���̱� ����.
	MakeRestItems( 3 );
  MakeCyclicThread;

end;

procedure TUpbitSpot.CyclicNotify(Sender: TObject);
var
//	aItem : TCyclicItem;
  aReq  : TRequest;
  sNm, sRsrc  : string;
  iCnt : integer;
begin

//  if IsAsyncWaiting then Exit;

  if Sender = nil then Exit;
  aReq := Sender as TRequest;

  try
//  	aReq	:= GetReqItems;
//    if (aReq <> nil) and ( aReq.State <> 1 ) then
//    begin
//			aReq.Req.Params.Clear;
//
//      case aItem.index of
//      	1 : sRsrc	:=  '/v1/orderbook';
//        2 : sRsrc	:=  '/v1/ticker';
//        3 : sRsrc	:=  '/v1/status/wallet';
//      end;

      if aReq.Seq = 2 then
      begin
        var sToken : string;
        sToken := GetSig(0);
        aReq.Req.AddParameter('Authorization', sToken, TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode] );
      end else
      	aReq.Req.AddParameter('markets', FMarketParam, pkGETorPOST);

//      aReq.SetParam(rmGET, sRsrc, aITem.Name);

//      iCnt := 0;
//      while IsAsyncWaiting do begin
//        sleep(10);
//        App.DebugLog('waiting (%02d) : %s - %', [iCnt, aReq.Name] );
//        inc(iCnt);
//      end;

			if aReq.RequestAsync then begin
        RestExet := aReq.ReqThread;
      	aReq.State := 1;
      end;
//      if RestThread <> nil then
//        RestThread.PushQueue( aReq );
//    end else
//    if (aReq <> nil) and ( aReq.State = 1 ) then
//    begin
//      App.DebugLog('%s, %s State = 1 !!! ', [TExShortDesc[GetExKind] , aItem.Name] );
//    end;


  except
//  	on e : exception do
//	  	App.Log(llError, '%s %s CyclicNotify Error : %s', [ TExShortDesc[GetExKind]
//      , aItem.Name, e.Message ]  );
  end;
end;

procedure TUpbitSpot.RestNotify(Sender: TObject);
var
   aReq : TRequest;
   sTmp : string;
   gap, avg  : integer;

begin
  if Sender = nil then Exit;
  inherited  RestNotify( Sender );
end;

end.
