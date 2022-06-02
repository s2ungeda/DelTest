unit URestThread;
interface
uses
  system.Classes, system.SysUtils, system.DateUtils
  , Windows, SyncObjs
  , URestRequests   , REST.Types, REST.Client
  ;
type

	TRestThread = class;

	TReqeustItem = class
  private
    FOnNotify: TNotifyEvent;
  public
	  Req : TRequest;
    AMethod : TRESTRequestMethod;
    ResThread: TRESTExecutionThread;
    AResource : string;
    Result :string;
    Name : string;
    sJson, sOut	: string;
		owner :    TRestThread;
    st	: DWORD;
    et	: DWORD;
  	constructor Create;
    Destructor  Destroy; override;
    procedure ASyncProc;
    property OnNotify : TNotifyEvent read FOnNotify write FOnNotify;
  end;

	
  TRestThread = class( TThread)
  private
    FEvent  : TEvent;
    FMutex  : HWND;
    FQueue ,FQueue2 : TList;
    FOnNotify: TNotifyEvent;
    FData		: TReqeustItem;
    FList   : TList;
    sJson, sOut	: string;
  protected
    procedure Execute; override;
    procedure SyncProc;
  public
    constructor Create( aProc : TNotifyEvent; aList : TList );
    destructor Destroy; override;

    procedure PushQueue( aReqItem : TReqeustItem );
    function  PopQueue : TReqeustItem;

    property OnNotify : TNotifyEvent read FOnNotify;
    
  end;


implementation
{ TRestThread }

constructor TRestThread.Create( aProc : TNotifyEvent; aList : TList );
begin

	FOnNotify := aProc;
	
  inherited Create(false);
  FreeOnTerminate := false;
  Priority  := tpNormal;
  
  FEvent  := TEvent.Create( nil, False, False, '');
  FMutex  := CreateMuTex( nil, false, 'Binance_Mutex_1' );
  FQueue  := TList.Create;
  FQueue2	:= TList.Create;

  FList   := aList;


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


  while not Terminated do
  begin

    if not( FEvent.WaitFor( 20 ) in [wrSignaled] ) then
    begin
      var aData : TReqeustItem;
      aData := PopQueue;
      if aData <> nil then
      begin
//        aData.ResThread := aData.Req.RequestAsync(
//          procedure begin
//
//            if aData <> nil then
//            begin
//           //   FOnNotify( aData.AResource + '_' + aData.sJson );
//              aData.Free;
//            end;
//
//          end
//        , aData.AMethod, aData.AResource, true);
     //   FList.Add( aData );
      end;
    end;


//    	FData	:= PopQueue;
//      if FData <> nil then
//      begin
//      	FData.st := GetTickCount;
////        FData.Req.Request( FData.AMethod, FData.AResource, '', sJson, sOut );
//        FData.ResThread := FData.Req.RequestAsync( SyncProc, FData.AMethod, FData.AResource, true);
//        FList.Add( FData );
////        Synchronize( SyncProc );
////        FData.Free;
//        //FData := nil;
//      end;
//    end;

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
var
	sTmp : string;
begin

//	if Assigned( FOnNotify ) then  begin
//  	FOnNotify( '---' ) ;
//  end;

end;

{ TReqeustItem }

procedure TReqeustItem.ASyncProc;
var
  sId, sTmp : string;
begin
  if Assigned( OnNotify) then
  begin
    if ResThread <> nil then
      sID := Format('%d', [ ResThread.ThreadID ] )
    else
      sID := '';

//    Result := Format('%s : %s, %d, %100.100s', [ sID, Name,  Req.StatusCode, Req.GetResponse ] );
    OnNotify( Self );
  end;



end;

constructor TReqeustItem.Create;
begin
	Req := TRequest.Create;
  st := 0;
  et := 0;
end;

destructor TReqeustItem.Destroy;
begin
  Req.Free;
//  if ResThread <> nil then
//    ResThread.Free;
  inherited;
end;

end.

