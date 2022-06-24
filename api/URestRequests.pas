unit URestRequests;
interface
uses
  System.Classes, System.SysUtils, System.DateUtils,
  REST.Client, REST.Types  , windows
  ;
type
  TRequest = class
  private
    FReq: TRESTRequest;
    FRsp: TRESTResponse;
    FClient: TRESTClient;
    FOnNotify: TNotifyEvent;
    FName: string;
    FReqThread: TRESTExecutionThread;
    FState: integer;
    FSeq: integer;
    FInterval: integer;
    FThreadID: int64;
    FSndCnt: int64;
    FRcvCnt: int64;
    function GetContent: string;
    function GetStatusCode: integer;
    function GetStatusText: string;
  public
    StTime, EnTime : int64;

	  LastTime : DWORD;
    PrevTime : DWORD;
    Count    : integer;    

    Constructor Create;
    Destructor  Destroy; override;
    procedure init( url : string;  bOpt : boolean = false ); overload;
    procedure init( AMethod : TRESTRequestMethod; AUrl, AResource, AName : string;
      bOpt : boolean = false ); overload;
    function Request( AMethod : TRESTRequestMethod;  AResource  : string;  ABody : string;
      var OutJson, OutRes  : string  ) : boolean;
    function RequestAsync( aHandler: TCompletionHandler; AMethod : TRESTRequestMethod;
      AResource : string ) : TRESTExecutionThread; overload;
    function RequestAsync : boolean; overload;

    function GetID : integer;

    procedure SetParam(  AMethod : TRESTRequestMethod;  AResource, AName : string ); overload;
    procedure SetParam(  aSeq, aInterval : integer ); overload;
    
    procedure ASyncProc;
    property Req : TRESTRequest read FReq;
    property Rsp : TRESTResponse read FRsp;
    property Client  : TRESTClient  read FClient;
    property ReqThread : TRESTExecutionThread read FReqThread;
    property ThreadID  : int64 read FThreadID;
    property Name : string read FName;
    property StatusCode : integer read GetStatusCode;
    property StatusText : string  read GetStatusText;
    property Content  : string read GetContent;

    property OnNotify : TNotifyEvent read FOnNotify write FOnNotify;

    // 0 : no used  1 : request   2 : response
    property State  : integer read FState write FState;
    property Seq	 	: integer read FSeq		;
    property Interval	: integer read FInterval ;

    property SndCnt : int64 read FSndCnt write FSndCnt;
    property RcvCnt : int64 read FRcvCnt write FRcvCnt;

  end;
implementation


{ TRequest }

procedure TRequest.ASyncProc;
begin
  inc(FRcvCnt);
  if Assigned(FOnNotify) then
  begin
    EnTime  := GetTickCount;
    FOnNotify( Self );
  end;
end;

constructor TRequest.Create;
begin
  FClient   := TRESTClient.Create('');
  FReq      := TRESTRequest.Create( FClient );
  FRsp      := TRESTResponse.Create( nil );
  Req.Response := FRsp;
  FReqThread  := nil;
  FOnNotify   := nil;

  FState      := 0;

  StTime      := 0;
  EnTime      := 0;

  LastTime := 0;
  Count    := 0;
  FSndCnt  := 0;
  FRcvCnt  := 0;
end;
destructor TRequest.Destroy;
begin
  Req.Free;
  FClient.Free;
  FRsp.Free;
  inherited;
end;


function TRequest.GetContent: string;
begin
  Result := Rsp.Content;
end;


function TRequest.GetID: integer;
begin
  Result := 0;
  if FReqThread <> nil then
    Result := FReqThread.ThreadID;
end;

function TRequest.GetStatusCode: integer;
begin
  Result := Rsp.StatusCode;
end;

function TRequest.GetStatusText: string;
begin
  Result := Rsp.StatusText;
end;

procedure TRequest.init(AMethod: TRESTRequestMethod; AUrl, AResource, AName: string;
  bOpt: boolean);
begin
  FName := AName;

  FClient.BaseURL := AUrl;
  FReq.Method     := AMethod;
  FReq.Resource   := AResource;

	if bOpt then
  begin
    FReq.Accept := '*/*';
    FReq.AcceptEncoding := 'gzip, deflate';
  end;
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
      if Rsp.StatusCode <> 200 then
      begin
        OutRes := Format( 'status : %d, %s', [ Rsp.StatusCode, Rsp.StatusText ] );
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

function TRequest.RequestAsync: boolean;
begin
  FReqThread := FReq.ExecuteAsync(ASyncProc);
  StTime := GetTickCount;
  Result := FReqThread <> nil;
  FThreadID := FReqThread.ThreadID;
  inc( FSndCnt );
end;

procedure TRequest.SetParam(aSeq, aInterval: integer);
begin

	FSeq			:= aSeq;
  FInterval := aInterval;
end;

procedure TRequest.SetParam(AMethod : TRESTRequestMethod;  AResource, AName : string);
begin
  FName  := AName;
  with FReq do
  begin
    Method    := AMethod;
    Resource  := AResource;
  end;
end;

function TRequest.RequestAsync(aHandler: TCompletionHandler;
  AMethod: TRESTRequestMethod; AResource: string  ): TRESTExecutionThread;
begin
  with FReq do
  begin
    Method    := AMethod;
    Resource  := AResource;
    Result    := ExecuteAsync(aHandler);
  end;
end;
end.

