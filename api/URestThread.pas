unit URestThread;
interface
uses
  system.Classes, system.SysUtils, system.DateUtils
  , Windows, SyncObjs  , REST.Types
  , URestRequests  
  , UApiTypes, UApiConsts , URestItems
  
  ;
type      
	TResponseNotify = procedure( iCode : integer; sName : string; sData : string ) of Object;
	
  TRestThread = class( TThread )
  private
    FEvent  : TEvent;
    FMutex  : HWND;
    FQueue  : TList;
    FData		: TRequest;
    FDivInfo: TDivInfo;
    FOnResNotify: TResponseNotify;
    function MakeuniqueName( sAddSt : string ) : string;
    
  protected
    procedure Execute; override;
    procedure SyncProc;
  public
    constructor Create( aInfo : TDivInfo );
    destructor Destroy; override;   
    procedure PushQueue( aReqItem : TRequest );
    function  PopQueue : TRequest;
    function  QCount : integer;
    property  DivInfo : TDivInfo read FDivInfo;
    property  OnResNotify : TResponseNotify read FOnResNotify;
    
  end;

implementation
uses
	GApp , UTypes

  ;
{ TRestThread }
constructor TRestThread.Create( aInfo : TDivInfo  );
begin
  FDivInfo 			:= aInfo;
//  FOnResNotify	:=  aProc;
	
  inherited Create(false);
  FreeOnTerminate := true;
  Priority  := tpNormal;
  
  FEvent  := TEvent.Create( nil, False, False, MakeuniqueName('Event') );
  FMutex  := CreateMuTex( nil, false, PChar(MakeuniqueName('Mutex')) );
  FQueue  := TList.Create;   
end;
destructor TRestThread.Destroy;
begin
  CloseHandle( FMutex );
  FEvent.Free;
  FQueue.Free;
  inherited;
end;
procedure TRestThread.Execute;
begin

  while not Terminated do
  begin
    if not( FEvent.WaitFor( FDivInfo.WaitTime ) in [wrSignaled] ) then
    begin
     // var aData : TRequest;
      FData := PopQueue;
      if FData <> nil then
      begin
        if FData.RequestAsync then
          FData.State := 1
        else
          App.Log(llError, 'RequestAsync error : %s, %s',[ FDAta.Req.Resource, Fdata.Name]  );
      end;
      
//    	FData	:= PopQueue;
//      if FData <> nil then
//      begin      	
//       //	FData.st := GetTickCount;       
//        FData.Req.Request( FData.AMethod, FData.AResource, '', FData.JsonData, FData.OutData );       
//        Synchronize( SyncProc );
//        FData.Free;
//        FData := nil;
//      end;
    end; 
  end;

end;

function TRestThread.MakeuniqueName(sAddSt: string): string;
begin
	Result := Format('%s_%s_%s_%d', [ TExShortDesc[ FDivInfo.Kind] 
  	,   TMarketTypeDesc[ FDivInfo.Market ], sAddSt,  FDivInfo.Index ] );
end;
function TRestThread.PopQueue: TRequest;
begin
	if FQueue.Count < 1 then exit (nil);
	WaitForSingleObject(FMutex, INFINITE);
	Result := FQueue.Items[0];
	FQueue.Delete(0);
//  FQueue2.Add( Result);
  ReleaseMutex(FMutex);              
    
end;

procedure TRestThread.PushQueue(aReqItem: TRequest);
begin
	WaitForSingleObject(FMutex, INFINITE);
	FQueue.Add( aReqItem );
  ReleaseMutex(FMutex);              
end;
function TRestThread.QCount: integer;
begin
  Result := FQueue.Count;
end;

procedure TRestThread.SyncProc;
begin
//	if Assigned( FOnResNotify ) then begin
//  	if FData.Name = 'status' then
//    begin
//    //	App.DebugLog( '----%s----', [FData.OutData] );
//    end;
//  	FOnResNotify( FData.Req.StatusCode, FData.Name, FData.JsonData );
//  end;
    
end;

end.
