unit UExchange;

interface

uses
  System.Classes,   System.SysUtils,
  REST.Types, REST.Client ,

  UApiTypes
  ;
type

  TExchange = class
  private
    FRESTClient: TRESTClient;

    FExCode: string;
    FLastTime: TDateTime;
    FInfo: TExchangeInfo;
    FRestRes: TRESTResponse;
    FRestReq: TRESTRequest;
    FParent: TObject;
    FMarketType: TMarketType;
    FMarketIdx: integer;
    FMasterData: string;
    FCodes: TStrings;

  public
    Constructor Create( aObj : TObject; aMarketType : TMarketType ); overload;
    Destructor  Destroy; override;

    function Request( AMethod : TRESTRequestMethod;  AResource : string;  ABody : string;
      var OutJson, OutRes  : string  ) : boolean;

    procedure SetBaseUrl(url : string); inline;
    procedure SetParam( const aName, aValue : string;  aKind : TRESTRequestParameterKind = pkGETorPOST) ; inline;
    procedure GetCodeList( var aList : TStringList ) ;
    function  GetExKind : TExchangeKind;
    function  GetCodeIndex( S : string ) : integer;


//----------------------------------------------------------- common request
    function ParsePrepareMaster : integer ; virtual; abstract;
    function RequestMaster : boolean ; virtual; abstract;

//--------------------------------------------------------------
    function PrepareMaster : boolean;
//--------------------------------------------------------------
    property Parent : TObject read FParent;

    property RESTClient: TRESTClient read FRESTClient;
    property RestReq: TRESTRequest read FRestReq;
    property RestRes: TRESTResponse read FRestRes;

    property Info : TExchangeInfo read FInfo;
    property LastTime : TDateTime read FLastTime write FLastTime;

    property MarketType : TMarketType read FMarketType;
    property MarketIdx  : integer read FMarketIdx;

    // 종목마스터 데이타.
    property MasterData : string read FMasterData;
    // 공통 Base Currency Code 가 담겨 있음.
    property Codes  : TStrings read FCodes;
  end;

implementation

uses
  GApp,
  System.JSON,
  UExchangeManager
  ;

{ TExchagne }

constructor TExchange.Create(  aObj : TObject; aMarketType : TMarketType );
begin

  FRESTClient   := TRESTClient.Create('');
  FRestReq := TRESTRequest.Create( FRESTClient );
  FRestRes := TRESTResponse.Create( nil );

  FRestReq.Response := FRestRes;

  FParent := aObj;
  FMarketType := aMarketType;
  FMarketIdx  := integer(FMarketType);

  FCodes := TStringList.Create;
end;

destructor TExchange.Destroy;
begin

  FCodes.Free;

  FRestRes.Free;
  FRestReq.Free;
  FRESTClient.Free;
  inherited;
end;



function TExchange.GetCodeIndex( S : string ) : integer;
begin
  Result := ( FParent as TExchangeManager ).Codes.IndexOf(S);
end;

procedure TExchange.GetCodeList(var aList: TStringList);
var
  i : integer;
begin
  for I := 0 to ( FParent as TExchangeManager ).Codes.Count-1 do
    aList.Add( ( FParent as TExchangeManager ).Codes[i]);
end;

function TExchange.GetExKind: TExchangeKind;
begin
  Result := ( FParent as TExchangeManager ).ExchangeType;
end;

function TExchange.PrepareMaster: boolean;
var
  sTmp, sOut, sJson : string;
begin
  sTmp  := App.Engine.ApiConfig.GetBaseUrl( GetExKind , mtSpot );
  SetBaseUrl( sTmp );
  sTmp  := App.Engine.ApiConfig.GetPrepare(GetExKind , mtSpot );
  if Request( rmGET, sTmp , '', sJson, sOut ) then
  begin
    FMasterData := sJson ;
    ParsePrepareMaster;
  end else
  begin
    App.Log( llError, '', 'Failed %s spot PreparMaster (%s, %s)',
      [ TExchangeKindDesc[GetExKind], sOut, sJson] );
    Exit( false );
  end;

  Result := FCodes.Count > 0 ;
end;


function TExchange.Request(AMethod : TRESTRequestMethod;  AResource : string;  ABody : string;
      var OutJson, OutRes : string ): boolean;
var
  aParam : TRESTRequestParameter;
begin

  with FRestReq do
  begin
    Method   := AMethod;
    Resource := AResource;
    if ABody <> '' then
      Body.Add( ABody);
//  Body.Add( ABody, ctAPPLICATION_JSON);
  end;


  try
    try
      FRestReq.Execute;

      if FRestRes.StatusCode <> 200 then
      begin
        OutRes := Format( 'status : %d, %s', [ FRestRes.StatusCode, FRestRes.StatusText ] );
        OutJson:= FRestRes.Content;
        Exit( false );
      end;

      OutJson := FRestRes.Content;
      Result := true;

    except
      on E: Exception do
      begin
        OutRes := E.Message;
        Exit(false);
      end
    end;
  finally
    FRestReq.Params.Clear;
  end;


end;

procedure TExchange.SetBaseUrl(url: string);
begin
  FRESTClient.BaseURL := url;
end;


procedure TExchange.SetParam(const aName, aValue: string;
  aKind: TRESTRequestParameterKind);
begin
  FRestReq.AddParameter(aName, aValue, aKind);
end;

end.
