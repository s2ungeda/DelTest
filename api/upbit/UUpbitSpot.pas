unit UUpbitSpot;

interface

uses
  System.Classes, System.SysUtils, System.DateUtils,

  System.JSON,  Rest.Json , Rest.Types , Rest.Client,

  UExchange,

  UApiTypes

  ;

type
  TUpbitSpot = class( TExchange )
  private

    FIndex  : integer;
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

    procedure parseAssetsstatus;
    procedure parseOrderBook;
    procedure parseTicker;

  public
    Constructor Create( aObj : TObject; aMarketType : TMarketType );
    Destructor  Destroy; override;

    procedure RequestData;

    function ParsePrepareMaster : integer; override;
    function RequestMaster : boolean ; override;
    function RequestCandleData( sUnit : string; sCode : string ) : boolean; override;

    function RequestDNWState : boolean; override;
    function RequestAsyncDnwState( idx : Integer ) : TRESTExecutionThread;

    property  LimitSec : integer read FLimitSec;
    property  LimitMin : integer read FLimitMin;
    property  Interval : integer read FInterval;
    property  Enable   : boolean read FEnable;

      // 쿼리스트링 인자 ( 모든 종목 )
    property  MarketParam : string read FMarketParam;
  end;

implementation

uses
  GApp  , UApiConsts
  , UEncrypts
  , UUpbitParse
  , JOSE.Core.JWT

  , JOSE.Core.JWA
  , JOSE.Core.Builder
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
var
  i : integer;
begin

  for I := 0 to High(Req) do
  begin
    Req[i].init( App.Engine.ApiConfig.GetBaseUrl( GetExKind , mtSpot ));
    Req[i].Req.OnHTTPProtocolError := OnHTTPProtocolError;
  end;

  Result :=  RequestSpotTicker
      and  RequestDNWState
      ;
end;


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


procedure TUpbitSpot.RequestData;
begin
  // 호가 , 티커, 입출금.
  if ( FIndex <> FLastIndex )
    or ( RestResult = nil )
    or (( RestResult <> nil ) and ( RestResult.Finished )) then
  begin
    FLastIndex := FIndex;

    case FIndex of
      0 , 2: begin
        Req[0].Req.AddParameter('markets', FMarketParam, pkGETorPOST);
        RestResult := Req[0].RequestAsync( parseOrderBook, rmGET, '/v1/orderbook', true);
      end;
      1 : begin
        RestResult := RequestAsyncDnwState(1);
      end;
      3 :begin
        Req[2].Req.AddParameter('markets', FMarketParam, pkGETorPOST);
        RestResult := Req[2].RequestAsync( parseTicker, rmGET, 'v1/ticker', true);
      end;
      else exit;
    end;

    if RestResult = nil then
      App.Log( llError,  ' !! %s, %d Request %d Error ', [ TExchangeKindDesc[GetExKind], FIndex ] )
    else begin
      inc( FIndex );
      if FIndex >= 4 then
        FIndex := 0;
    end;

  end else
  begin
    var s : string;
    if RestResult.Finished then s := 'fin' else s := 'not fin';
    App.DebugLog( '!! %s, %d waiting req -> %d %s ', [ TExchangeKindDesc[GetExKind], FIndex, RestResult.ThreadID, s ]  );
  end;
end;



procedure TUpbitSpot.parseTicker;
var
  sJson : string;
begin
  sJson :=  Req[2].GetResponse;
  if sJson = '' then Exit;
  gUpReceiver.ParseSpotTicker2( sJson );
end;

procedure TUpbitSpot.parseAssetsstatus;
var
  sJson : string;
begin
  sJson :=  Req[1].GetResponse;
  if sJson = '' then Exit;
  gUpReceiver.ParseDNWSate( sJson );
end;

procedure TUpbitSpot.parseOrderBook;
var
  sJson : string;
begin
  sJson :=  Req[0].GetResponse;
  if sJson = '' then Exit;
  gUpReceiver.ParseSpotOrderBook( sJson );

end;

function TUpbitSpot.RequestAsyncDnwState( idx : Integer ): TRESTExecutionThread;
var
  LToken: TJWT;
  guid : TGUID;
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
    Req[idx].Req.AddParameter('Authorization', sToken, TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode] );

    Result := Req[idx].RequestAsync( parseAssetsstatus, rmGET, '/v1/status/wallet');

  finally
    LToken.Free;
  end;
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
//  if not FEnable then
//  begin
//    inc( FInterval ,2 );
//    if FInterval > 30 then begin
//      FEnable   := true;
//      FInterval := 0;
//      FLimitMin := 1;
//    end else Exit;
//  end;
//
//
//  if (FLimitSec <= 0) then
//  begin
//    App.Log(llInfo, 'TUpbitSpot RequestDNWState Rate Limit %d, %d',[ FLimitMin, FLimitSec] );
//
//    if FLimitMin <= 0 then begin
//      FEnable   := false;
//      FInterval := 0;
//      Exit;
//    end;
//
//    FLimitSec := 1;
//    Exit;
//  end;

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



end.
