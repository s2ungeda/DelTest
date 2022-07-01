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
  public
    Constructor Create( bn, up, bt : TRESTRequest );
    Destructor  Destroy; override;
    procedure OnSharedDataNotify( aData : TDataItem );
  end;

implementation

uses
  GApp, GLibs
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

constructor TRestManager.Create(bn, up, bt: TRESTRequest);
begin
  FRestReq[ekBinance] := bn;
  FRestReq[ekUpbit]   := up;
  FrestReq[ekBithumb] := bt;
end;

destructor TRestManager.Destroy;
begin

  inherited;
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

procedure TRestManager.RequestBinFutCnlOrder(sData: string);
var
  sArr  : TArray<string>;
  sTime, sBody, sSig : string;
begin
  if not CheckShareddData( sArr, sData, BC_CNT, 'BinFutCnlOrder') then Exit;

  try

    sTime := GetTimestamp;
    sBody := Format('symbol=%s&orderId=%s&timestamp=%s',
      [ sArr[BC_CODE], sArr[BC_OID], sTime ]);


    sSig  := CalculateHMACSHA256(sBody,App.ApiConfig.GetSceretKey( ekBinance, mtFutures) );
    sBody := sBody + Format('&signature=%s', [ sSig ]);
    FRestReq[ekBinance].AddParameter('X-MBX-APIKEY',
        App.ApiConfig.GetApiKey( ekBinance, mtFutures) , pkHTTPHEADER );

    FRestReq[ekBinance].Resource := '/fapi/v1/order?'+sBody;
    FRestReq[ekBinance].Method := rmDELETE;

    FRestReq[ekBinance].Execute;

    //memo1.Lines.Add( restRes.JSONValue.ToString );
  finally

  end;

end;

procedure TRestManager.RequestBinFutNewOrder(sData: string);
var
  sArr  : TArray<string>;
begin
  if not CheckShareddData( sArr, sData, BO_CNT, 'BinFutNewOrder') then Exit;


end;

end.
