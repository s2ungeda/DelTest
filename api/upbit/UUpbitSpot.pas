unit UUpbitSpot;

interface

uses
  System.Classes, System.SysUtils, System.DateUtils,

  System.JSON,  Rest.Json , Rest.Types ,

  UExchange,

  UApiTypes

  ;

type
  TUpbitSpot = class( TExchange )
  public
    Constructor Create( aObj : TObject; aMarketType : TMarketType );
    Destructor  Destroy; override;

    function ParsePrepareMaster : integer; override;
    function RequestMaster : boolean ; override;
    procedure RequestDNWState; override;
  end;

implementation

uses
  GApp  , UApiConsts
  , UUpbitParse
  ,JOSE.Core.JWT
//  ,JOSE.Core.JWK
//  ,JOSE.Core.JWS
  ,JOSE.Core.JWA
  ,JOSE.Core.Builder
 // ,JOSE.Types.JSON

  ;

{ TBinanceSpotNMargin }

constructor TUpbitSpot.Create(aObj: TObject; aMarketType: TMarketType);
begin
  inherited Create( aObj, aMarketType );

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
  aList : TStringList;
  sTmp, sOut, sJson, sData  : string;
  I: Integer;
begin
  aList := TStringList.Create;
  try
    GetCodeList(aList);
    if aList.Count <= 0 then Exit;
    sTmp := '';
    for I := 0 to aList.Count-1 do
    begin
      sTmp := sTmp + 'KRW-'+aList[i];
      if i < aList.Count-1  then
        sTmp := sTmp + ','
    end;

    SetBaseUrl( App.Engine.ApiConfig.GetBaseUrl( GetExKind , mtSpot ) );
    SetParam('markets', sTmp );


    if Request( rmGET, 'v1/ticker', '', sJson, sOut ) then
    begin         //App.Log( llDebug, '', '%s (%s, %s)', [ TExchangeKindDesc[GetExKind], sOut, sJson] );

      gUpReceiver.ParseSpotTicker( sJson );
    end else
    begin
      App.Log( llError, '', 'Failed %s RequestMaster (%s, %s)',
        [ TExchangeKindDesc[GetExKind], sOut, sJson] );
      Exit( false );
    end;

    sTmp := '';
    for I := 0 to aList.Count-1 do
    begin
      sTmp := sTmp + Format('"KRW-%s"', [aList[i]]);
      if i < aList.Count-1  then
        sTmp := sTmp + ','
    end;


    Result := App.Engine.SymbolCore.Symbols[ GetExKind].Count > 0 ;

  finally
    aList.Free;
  end;

end;

procedure TUpbitSpot.RequestDNWState;
var
  LToken: TJWT;
  guid : TGUID;
  sSig, sID, sToken, sData, sOut, sJson : string;
begin

  LToken:= TJWT.Create(TJWTClaims);

  try

    CreateGUID(guid);
    sData := GUIDToString(guid);
    sID := Copy( sData, 2, Length( sData) - 2);

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
      Exit;
    end;

  finally
    LToken.Free;
  end;

end;


end.
