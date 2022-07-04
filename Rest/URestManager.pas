unit URestManager;

interface

uses
  System.Classes, System.SysUtils,

  System.JSON,  Rest.Json , REST.Client,  Rest.Types,

  UApiTypes ,

  USharedData
  ;

type

  TRestManager = class
  private
    FRestReq : array [TExchangeKind] of TRESTRequest ;

    function CheckShareddData( var sArr : TArray<string>; sData: string;
               iCount : integer; aPrcName : string ) : boolean;

    procedure RequestBinFutNewOrder( sData : string );
    procedure RequestBinFutCnlOrder( sData : string );

    function Request( aExKind : TExchangeKind; AMethod : TRESTRequestMethod;  AResource : string;
       var outJson, outRes : string ) : boolean;
  public
    Constructor Create;
    Destructor  Destroy; override;
    procedure init( bn, up, bt : TRESTRequest );
    procedure OnSharedDataNotify( aData : TDataItem );
  end;

implementation

uses
  GApp, GLibs
  , USharedConsts
  , UApiConsts
  , UEncrypts
  ;

{ TRestManager }

function TRestManager.CheckShareddData(var sArr : TArray<string>; sData: string;
   iCount : integer; aPrcName : string): boolean;
var
  iLen : integer;
begin

  sArr  := sData.Split(['|']);
  iLen  := high( sArr );

  if ( iLen <= 0 ) or ( iLen <> iCount ) then
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
  case aData.exKind of
    'b':  // binance
      case aData.trDiv of
        'n' : RequestBinFutNewOrder( AnsiString( aData.data )) ;
        'c' : RequestBinFutCnlOrder( AnsiString( aData.data )) ;
      end;
    'u':  // upbit
      case aData.trDiv of
        'n' : ;
        'c' : ;
      end;
    't':  // bithumb
      case aData.trDiv of
        'n' : ;
        'c' : ;
      end;
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
  end;

end;

procedure TRestManager.RequestBinFutCnlOrder(sData: string);
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

procedure TRestManager.RequestBinFutNewOrder(sData: string);
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

  if Request( ekBinance, rmPOST, '/fapi/v1/order?'+sBody, outJson , outRes ) then
  begin

  end else
  begin
    App.Log( llError, '', 'Failed %s RequestBinFutNewOrder (%s, %s)',
      [ TExchangeKindDesc[ekBinance], outRes, outJson] );
  end;

end;

end.
