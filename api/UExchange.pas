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

  public
    Constructor Create( aObj : TObject ); overload;
    Destructor  Destroy; override;

    function Request( AMethod : TRESTRequestMethod;  AResource : string;  ABody : string;
      var OutData : string  ) : boolean;

    property Parent : TObject read FParent;

    property RESTClient: TRESTClient read FRESTClient;
    property RestReq: TRESTRequest read FRestReq;
    property RestRes: TRESTResponse read FRestRes;

    property Info : TExchangeInfo read FInfo;
    property LastTime : TDateTime read FLastTime write FLastTime;
  end;

implementation

uses
  System.JSON,
  UExchangeManager
  ;

{ TExchagne }

constructor TExchange.Create(  aObj : TObject );
begin

  FRESTClient   := TRESTClient.Create('');
  FRestReq := TRESTRequest.Create( FRESTClient );
  FRestRes := TRESTResponse.Create( nil );

  FRestReq.Response := FRestRes;

  FParent := aObj;
end;

destructor TExchange.Destroy;
begin

  FRestRes.Free;
  FRestReq.Free;
  FRESTClient.Free;
  inherited;
end;



function TExchange.Request(AMethod : TRESTRequestMethod;  AResource : string;  ABody : string;
      var OutData : string ): boolean;
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
  except
    on E: Exception do
    begin
      OutData := E.Message;
      Exit(false);
    end
  end;
end;

end.
