unit UCyclicThreads;

interface

uses
  System.Classes, System.SyncObjs
  , Windows
  , UCyclicItems
  ;

const
  INTERVAL = 30;

type      


  TCyclicThread = class(TThread)
  private
    { Private declarations }
   FMutex  : HWND;
   FEvent  : TEvent;
   FITems  : TCyclicItems;
   FOnNotify  : TNotifyEvent;

   FData : TCyclicItem;
   procedure SyncProc;
  protected
    procedure Execute; override;
  public
    constructor Create( AItems : TCyclicItems; ANotify : TNotifyEvent );
    destructor Destroy; override;

    procedure doJob;
  end;

implementation

uses
	Forms
  ;

{ IntervalThread }

constructor TCyclicThread.Create( AItems : TCyclicItems; ANotify : TNotifyEvent );
begin

  FITems    := AItems;
  FOnNotify := ANotify;

  inherited Create(true);
  FreeOnTerminate := true;
  Priority  := tpNormal;
  FMutex  := CreateMuTex( nil, false, 'Binance_Mutex_1' );
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
  I, gap : Integer;
  aItem : TCyclicItem;
  nTick : DWORD;
  bSend : boolean;
begin
  { Place thread code here }

  while not Terminated do
  begin
//    WaitForSingleObject( FMutex,  INTERVAL );
                                                
//    if not(FEvent.WaitFor( INFINITE ) in [wrSignaled]) then Continue;
    if not(FEvent.WaitFor( INTERVAL ) in [wrTimeout]) then Continue;


    for I := 0 to FItems.Count-1 do
    begin

      if Terminated then break;

      aItem := FItems.Cyclic[i];
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

      if bSend then begin
        aItem.PrevTime  := aItem.LastTime;
        aItem.LastTime  := nTick;
        FData := aItem;
        Synchronize( SyncProc );
      end;

      Application.ProcessMessages;
    end;


  end;

end;

procedure TCyclicThread.SyncProc;
begin
  if Assigned( FOnNotify ) then
    FOnNotify( FData );
end;

end.