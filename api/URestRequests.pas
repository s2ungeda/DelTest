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
    FStatusText: string;
    FStatusCode: integer;
    function CanRequest: boolean;

  public
    Constructor Create;
    Destructor  Destroy; override;

    procedure init( url : string;  bOpt : boolean = false );
    function Request( AMethod : TRESTRequestMethod;  AResource : string;  ABody : string;
      var OutJson, OutRes  : string  ) : boolean;
    function RequestAsync( aHandler: TCompletionHandler; AMethod : TRESTRequestMethod;  AResource : string;
       bOption : boolean = false  ) : TRESTExecutionThread;
    function GetResponse : string;

    property Req : TRESTRequest read FReq;
    property Rsp : TRESTResponse read FRsp;
    property Client  : TRESTClient  read FClient;

    property StatusCode : integer read FStatusCode;
    property StatusText : string  read FStatusText;

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

procedure TRequest.init(url: string;  bOpt : boolean);
begin
  FClient.BaseURL := url;
	if bOpt then
  begin
    FReq.Accept := '*/*';
    FReq.AcceptEncoding := 'gzip, deflate';  
  end;  
end;

function TRequest.Request(AMethod: TRESTRequestMethod; AResource, ABody: string;
  var OutJson, OutRes: string): boolean;
  var
  	idx : integer;
begin
  with FReq do
  begin
    Method   := AMethod;
    Resource := AResource;
    if ABody <> '' then
      Body.Add( ABody);
  end;      

  try
    try

      FReq.Execute;

      FStatusCode := Rsp.StatusCode;
      FStatusText := Rsp.StatusText;     

      if FStatusCode <> 200 then
      begin
        OutRes := Format( 'status : %d, %s', [ FStatusCode, FStatusText ] );
        OutJson:= Rsp.Content;
        Exit( false );
      end;                   

//      idx := Rsp.Headers.IndexOfName('Remaining-Req');
//      if idx >=0 then
//      	OutRes := Rsp.Headers[idx]; 	

      OutJson := Rsp.Content;
      Result := true;

    except
      on E: Exception do
      begin
        OutRes := E.Message;
        Exit(false);
      end
    end;
  finally
    FReq.Params.Clear;
  end;
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

