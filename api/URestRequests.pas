unit URestRequests;

interface

uses
  System.Classes, System.SysUtils, System.DateUtils,

  REST.Client, REST.Types
  ;

type

  TRequest = class( TCollectionItem )
  private
    FReq: TRESTRequest;
    FRsp: TRESTResponse;
    FClient: TRESTClient;
    FReqResult: TRESTExecutionThread;
    function CanRequest: boolean;

  public
    Constructor Create;
    Destructor  Destroy; override;

    procedure init( url : string );
    function Request( AMethod : TRESTRequestMethod;  AResource : string;  ABody : string;
      var OutJson, OutRes  : string  ) : boolean;
    function RequestAsync( aHandler: TCompletionHandler; AMethod : TRESTRequestMethod;  AResource : string;
       bOption : boolean = false  ) : TRESTExecutionThread;
    function GetResponse : string;

    property Req : TRESTRequest read FReq;
    property Rsp : TRESTResponse read FRsp;
    property Client  : TRESTClient  read FClient;

    property ReqResult : TRESTExecutionThread read FReqResult;
  end;

implementation

{ TRequest }

constructor TRequest.Create;
begin
  FClient   := TRESTClient.Create('');
  FReq      := TRESTRequest.Create( FClient );
  FRsp      := TRESTResponse.Create( nil );

  FReqResult:= nil;

  Req.Response := FRsp;
end;

destructor TRequest.Destroy;
begin
  Req.Free;
  FClient.Free;
  FRsp.Free;

  inherited;
end;

function TRequest.GetResponse: string;
begin
  Result := FReq.Response.Content;
end;

procedure TRequest.init(url: string);
begin
  FClient.BaseURL := url;


end;

function TRequest.Request(AMethod: TRESTRequestMethod; AResource, ABody: string;
  var OutJson, OutRes: string): boolean;
begin

end;

function TRequest.CanRequest : boolean;
begin
  if ( FReqResult = nil ) or
     (( FReqResult <> nil ) and (FReqResult.Finished )) then
    Result := true
  else
    Result := false;
end;

function TRequest.RequestAsync(aHandler: TCompletionHandler;
  AMethod: TRESTRequestMethod; AResource: string; bOption : boolean ): TRESTExecutionThread;
begin

  if bOption then
  begin
    FReq.Accept := '*/*';
    FReq.AcceptEncoding := 'gzip, deflate';
  end;

  with FReq do
  begin
    Method    := AMethod;
    Resource  := AResource;
    Result:= ExecuteAsync(aHandler);
    //Result    := FReqResult <> nil;
  end;
end;

end.

