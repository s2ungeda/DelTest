unit UBinFutRequests;

interface

uses
  System.Classes, System.SysUtils,
  REST.Client,  Rest.Types,
  UApiTypes  ,
  IRestRequests
  ;

type
  TBinFutReuqest = class(IRestRequest)
  private
    procedure sig(var sData: string);
  public
    procedure RequestNewOrder( sData, sRef : string );
    procedure RequestCnlOrder( sData, sRef : string );
    procedure RequestBalance( sData, sRef : string );
//    procedure RequestBinFutPosition( sData, sRef : string );
    procedure RequestOrderList( sData, sRef : string );
  end;

implementation

uses
  GApp, GLibs
  , UEncrypts
  , UApiConsts
  , USharedConsts
  ;

{ TBinFutReuqest }

procedure TBinFutReuqest.sig(var sData : string);
var
  sig : string;
begin
  sig   := CalculateHMACSHA256(sData,App.ApiConfig.GetSceretKey( ekBinance, mtFutures) );
  sData := sData + Format('&signature=%s', [ sig ]);

  RestReq.AddParameter('X-MBX-APIKEY',
      App.ApiConfig.GetApiKey( ekBinance, mtFutures) , pkHTTPHEADER );
end;



// 포지션과 잔고를 한번의 조회로 변경.
//procedure TBinFutReuqest.RequestBinFutPosition(sData, sRef: string);
//var
//  sBody, sTime, sSig, outJson, outRes : string;
//begin
//  //
//  sTime := GetTimestamp;
//  if sData = '' then
//    sBody := Format('timestamp=%s', [sTime])
//  else
//    sBody := Format('symbol=%s&timestamp=%s', [sData, sTime]);
//
//  sig(sBody);
//
//  if not Request( ekBinance,rmGET, '/fapi/v2/positionRisk?'+sBody, outJson, outRes ) then
//    App.Log( llError, '', 'Failed %s RequestBinFutPosition (%s, %s)',
//    [ TExchangeKindDesc[ekBinance], outRes, outJson] );
//
//  PushData( ekBinance, mtFutures, TR_REQ_POS, outJson, sRef );
//
//end;

// 선물 : 포지션과 잔고를 한번에 조회..
procedure TBinFutReuqest.RequestBalance(sData, sRef: string);
var
  sBody, sTime, outJson, outRes : string;
begin
  sTime := GetTimestamp;
  sBody := Format('timestamp=%s', [sTime]);

  sig(sBody);

//  sSig  := CalculateHMACSHA256(sBody,App.ApiConfig.GetSceretKey( ekBinance, mtFutures) );
//  sBody := sBody + Format('&signature=%s', [ sSig ]);
//
//  RestReq.AddParameter('X-MBX-APIKEY',
//      App.ApiConfig.GetApiKey( ekBinance, mtFutures) , pkHTTPHEADER );

  if not Request( ekBinance,rmGET, '/fapi/v2/account?'+sBody, outJson, outRes ) then
    App.Log( llError, '', 'Failed %s RequestBinFutBalance (%s, %s)',
    [ TExchangeKindDesc[ekBinance], outRes, outJson] );

  PushData( ekBinance, mtFutures, TR_REQ_BAL, outJson, sRef );

end;

procedure TBinFutReuqest.RequestOrderList(sData, sRef: string);
var
  sBody, sTime, outJson, outRes : string;
begin
  sTime := GetTimestamp;
  if sData = '' then
    sBody := Format('timestamp=%s', [sTime])
  else
    sBody := Format('symbol=%s&timestamp=%s', [sData, sTime]);

  sig(sBody);

  if not Request( ekBinance,rmGET, '/fapi/v1/openOrders?'+sBody, outJson, outRes ) then
    App.Log( llError, '', 'Failed %s RequestBinFutOrderList (%s, %s)',
    [ TExchangeKindDesc[ekBinance], outRes, outJson] );

  PushData( ekBinance, mtFutures, TR_REQ_ORD, outJson, sRef );

end;

procedure TBinFutReuqest.RequestCnlOrder(sData, sRef: string);
var
  sArr  : TArray<string>;
  sTime, sBody, sSig, outJson, outRes : string;
begin
  if not CheckShareddData( sArr, sData, BC_CNT, 'BinFutCnlOrder') then Exit;

  sTime := GetTimestamp;
  sBody := Format('symbol=%s&orderId=%s&timestamp=%s',
    [ sArr[BC_CODE], sArr[BC_OID], sTime ]);

  App.DebugLog('RequestBinFutCnlOrder : %s', [ sBody ] );

  sig(sBody);
//  sSig  := CalculateHMACSHA256(sBody,App.ApiConfig.GetSceretKey( ekBinance, mtFutures) );
//  sBody := sBody + Format('&signature=%s', [ sSig ]);
//
//  RestReq.AddParameter('X-MBX-APIKEY',
//      App.ApiConfig.GetApiKey( ekBinance, mtFutures) , pkHTTPHEADER );

  if not Request( ekBinance,rmDELETE, '/fapi/v1/order?'+sBody, outJson, outRes ) then
    App.Log( llError, '', 'Failed %s RequestBinFutCnlOrder (%s, %s)',
    [ TExchangeKindDesc[ekBinance], outRes, outJson] );

  PushData( ekBinance, mtFutures, TR_CNL_ORD, outJson, sRef );

end;

procedure TBinFutReuqest.RequestNewOrder(sData, sRef: string);
var
  sArr  : TArray<string>;
  sRsrc, outJson, outRes : string;
  sTime, sBody, sSig : string;
begin
  if not CheckShareddData( sArr, sData, BO_CNT, 'BinFutNewOrder') then Exit;

  sTime := GetTimestamp;
  sBody := Format('symbol=%s&side=%s&type=%s&timeInForce=GTC&quantity=%s&price=%s'+
                '&newClientOrderId=%s&reduceOnly=%s&timestamp=%s',
    [ sArr[BO_CODE],sArr[BO_LS], sArr[BO_TYPE], sArr[BO_QTY], sArr[BO_PRC]
      , sArr[BO_CID], sArr[BO_RDO], sTime ]);

  App.DebugLog('RequestBinFutNewOrder : %s', [ sBody ] );

  sig(sBody);
//  sSig  := CalculateHMACSHA256(sBody,App.ApiConfig.GetSceretKey( ekBinance, mtFutures) );
//  sBody := sBody + Format('&signature=%s', [ sSig ]);
//
//  RestReq.AddParameter('X-MBX-APIKEY',
//      App.ApiConfig.GetApiKey( ekBinance, mtFutures) , pkHTTPHEADER );

  if not Request( ekBinance ,rmPOST, '/fapi/v1/order?'+sBody, outJson, outRes ) then
    App.Log( llError, '', 'Failed %s RequestBinFutNewOrder (%s, %s)',
    [ TExchangeKindDesc[ekBinance], outRes, outJson] );

  PushData( ekBinance, mtFutures, TR_NEW_ORD, outJson, sRef );

end;


end.
