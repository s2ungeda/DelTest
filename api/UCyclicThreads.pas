unit UCyclicThreads;

interface

uses
  System.Classes, System.SyncObjs
  , Windows
  , URestRequests//UCyclicItems
  ;

const
  INTERVAL = 30;

type      


  TCyclicThread = class(TThread)
  private
    { Private declarations }
   FMutex  : HWND;
   FEvent  : TEvent;
   FITems  : TList;
   FOnNotify  : TNotifyEvent;

   FData : TRequest;
   procedure SyncProc;
  protected
    procedure Execute; override;
  public
    constructor Create( aList : TList; ANotify : TNotifyEvent ; AName : string);
    destructor Destroy; override;

    procedure doJob;
  end;

implementation

uses
	Forms
  ;

{ IntervalThread }

constructor TCyclicThread.Create( aList : TList; ANotify : TNotifyEvent; AName : string );
begin

  FITems    := aList;
  FOnNotify := ANotify;

  inherited Create(true);
  FreeOnTerminate := true;
  Priority  := tpNormal;
  FMutex  := CreateMuTex( nil, false, PChar(AName) );
  FEvent  := TEvent.Create( nil, False, False, '');
end;

destructor TCyclicThread.Destroy;
begin
  CloseHandle( FMutex );
  FEvent.Free;
  inherited;
end;

procedure TCyclicThread.doJob;
begin
  FEvent.SetEvent;
end;

procedure TCyclicThread.Execute;
var
  I, iSnd, gap : Integer;
  aItem : TRequest;
  nTick : DWORD;
  bSend : boolean;
begin
  { Place thread code here }

  while not Terminated do
  begin
//    WaitForSingleObject( FMutex,  INTERVAL );
                                                
//    if not(FEvent.WaitFor( INFINITE ) in [wrSignaled]) then Continue;
    if not(FEvent.WaitFor( INTERVAL ) in [wrTimeout]) then Continue;           

    iSnd := 0;
    
    for I := 0 to FItems.Count-1 do
    begin

      if Terminated then break;

      aItem := TRequest( FItems.Items[i] );
      if aItem = nil then continue;

      bSend := false;     
      nTick := GetTickCount;

      if aITem.LastTime <= 0 then
      begin
        gap := aItem.Count * INTERVAL;
        if gap >= aItem.Interval then
          bSend := true;
        inc( aItem.Count );
        
      end else
      begin
        gap   := nTick - aItem.LastTime ;
        if gap >= aItem.Interval then
          bSend := true;
      end;

      if bSend and (aItem.State <> 1) then begin
        aItem.PrevTime  := aItem.LastTime;
        aItem.LastTime  := nTick;
        FData := aItem;
        Synchronize( SyncProc );  
        inc( iSnd );
      end;
			// 루프 한번에 한건만 조회 하기 위해..
      if iSnd > 0 then break;               
   //   Application.ProcessMessages;
    end;


  end;

end;

procedure TCyclicThread.SyncProc;
begin
  if Assigned( FOnNotify ) then
    FOnNotify( FData );
end;

end.
