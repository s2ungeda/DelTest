unit URestThread;
interface
uses
  system.Classes, system.SysUtils, system.DateUtils
  , Windows, SyncObjs
  , URestRequests   , REST.Types
  ;
type

	TRestThread = class;

	TReqeustItem = class
  public
	  Req : TRequest;
    AMethod : TRESTRequestMethod;  
    AResource : string;
    sJson, sOut	: string;
		owner :    TRestThread;
  	constructor Create;
    Destructor  Destroy; override;	    
    procedure ASyncProc;
  end;

	
  TRestThread = class( TThread)
  private
    FEvent  : TEvent;
    FMutex  : HWND;
    FQueue ,FQueue2 : TList;
    FOnNotify: TGetStrProc;
    FData		: TReqeustItem;
    sJson, sOut	: string;
  protected
    procedure Execute; override;
    procedure SyncProc;
  public
    constructor Create( aProc : TGetStrProc );
    destructor Destroy; override;

    procedure PushQueue( aReqItem : TReqeustItem );
    function  PopQueue : TReqeustItem;
    
    property OnNotify : TGetStrProc read FOnNotify;
    
  end;

  var
  	RT : TRestThread;
implementation
{ TRestThread }

constructor TRestThread.Create( aProc : TGetStrProc );
begin

	FOnNotify := aProc;
	
  inherited Create(false);
  FreeOnTerminate := false;
  Priority  := tpNormal;
  
  FEvent  := TEvent.Create( nil, False, False, '');
  FMutex  := CreateMuTex( nil, false, 'Binance_Mutex_1' );
  FQueue  := TList.Create;
  FQueue2	:= TList.Create;

  RT := self;
end;

destructor TRestThread.Destroy;
begin

  CloseHandle( FMutex );
  FEvent.Free;
  FQueue.Free;
  FQueue2.Free;
  inherited;
end;

procedure TRestThread.Execute;
begin
  inherited;

  while not Terminated do
  begin

    if not( FEvent.WaitFor( 300 ) in [wrSignaled] ) then
    begin
    	FData	:= PopQueue;
      if FData <> nil then
      begin
        FData.Req.Request( FData.AMethod, FData.AResource, '', sJson, sOut );       
        Synchronize( SyncProc ); 
        //FData.Req.RequestAsync( FData.ASyncProc, FData.AMethod, FData.AResource)     ;
//        FData.Req.RequestAsync(
//        	procedure begin
//          	Synchronize( SyncProc );
//          end,
//        FData.AMethod, FData.AResource);

        FData.Free;

      end;
    end;
			


//    while FQueue.Count > 0 do begin
//      FData := PopData;
//      if FData <> nil then begin
//        Synchronize( SyncProc );
//        Dispose( FData );
//      end;
//      Application.ProcessMessages;
//    end;
  end;


end;

function TRestThread.PopQueue: TReqeustItem;
begin

	if FQueue.Count < 1 then exit (nil);
	WaitForSingleObject(FMutex, INFINITE);
	Result := FQueue.Items[0];
	FQueue.Delete(0);
//  FQueue2.Add( Result);
  ReleaseMutex(FMutex);              
    
end;

procedure TRestThread.PushQueue(aReqItem: TReqeustItem);
begin
	WaitForSingleObject(FMutex, INFINITE);
	FQueue.Add( aReqItem );
  ReleaseMutex(FMutex);              
end;

procedure TRestThread.SyncProc;
begin
	if Assigned( FOnNotify ) then
  	FOnNotify( sJson );//FormatDateTime('hh:nn:ss.zzz', now ) );
end;

{ TReqeustItem }

procedure TReqeustItem.ASyncProc;
begin
	RT.OnNotify( Req.GetResponse );
end;

constructor TReqeustItem.Create;
begin
	Req := TRequest.Create;
end;

destructor TReqeustItem.Destroy;
begin
  Req.Free;
  inherited;
end;

end.

