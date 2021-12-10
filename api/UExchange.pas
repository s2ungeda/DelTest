unit UExchange;

interface

uses
  System.Classes,   System.DateUtils,
  REST.Types, REST.Client ,

  UApiTypes

  ;

type

  TExchagne = class
  private
    FRESTClient: TRESTClient;
    FRESTRequest: TRESTRequest;
    FRESTResponse: TRESTResponse;
    FExCode: string;
    FLastTime: TDateTime;
    FInfo: TExchangeInfo;

  public
    Constructor Create( code : string );
    Destructor  Destroy; override;

    function RequestMaster : boolean; virtual; abstract;
    function RequestLast : boolean; virtual; abstract;

    property RESTClient: TRESTClient read FRESTClient;
    property RESTRequest: TRESTRequest read FRESTRequest;
    property RESTResponse: TRESTResponse read FRESTResponse;

    property ExCode : string  read FExCode ;
    property Info : TExchangeInfo read FInfo;

    property LastTime : TDateTime read FLastTime write FLastTime;
  end;

implementation

{ TExchagne }

constructor TExchagne.Create(code : string);
begin
  FExCode := code;

  FRESTClient   := TRESTClient.Create( nil );
  FRESTRequest  := TRESTRequest.Create( nil );
  FRESTResponse := TRESTResponse.Create( nil );
end;

destructor TExchagne.Destroy;
begin

  FRESTResponse.Free;
  FRESTRequest.Free;
  FRESTClient.Free;
  inherited;
end;



end.
