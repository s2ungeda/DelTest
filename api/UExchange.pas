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
    function  GetExKind : TExchangeKind;

//----------------------------------------------------------- common request
    function PrepareMaster : boolean;
    function ParsePrepareMaster : integer ; virtual; abstract;

    property Parent : TObject read FParent;

    property RESTClient: TRESTClient read FRESTClient;
    property RestReq: TRESTRequest read FRestReq;
    property RestRes: TRESTResponse read FRestRes;

    property Info : TExchangeInfo read FInfo;
    property LastTime : TDateTime read FLastTime write FLastTime;
    property MasterData : string read FMasterData;
    property MarketType : TMarketType read FMarketType;
    property MarketIdx  : integer read FMarketIdx;

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



function TExchange.GetExKind: TExchangeKind;
begin
  Result := ( FParent as TExchangeManager ).ExchangeType;
end;

function TExchange.PrepareMaster: boolean;
var
  sTmp, sOut, sJson : string;
begin
  sTmp  := App.Engine.ApiConfig.GetBaseUrl( GetExKind , emSpot );
  SetBaseUrl( sTmp );
  sTmp  := App.Engine.ApiConfig.GetPrepare(GetExKind , emSpot );
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
    Body.Add( ABody);
//  Body.Add( ABody, ctAPPLICATION_JSON);
  end;

//  aParam := FRestReq.Params.AddItem;
//  aParam.Value := ABody;
//  aParam.Value := TJSONObject.ParseJSONValue( ABody );
//  aParam.ContentType := ctAPPLICATION_JSON;

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
end;

procedure TExchange.SetBaseUrl(url: string);
begin
  FRESTClient.BaseURL := url;
end;



end.
