unit UCyclicThreads;

interface

uses
  System.Classes, System.SyncObjs
  , Windows
  , UCyclicItems
  ;

const
  INTERVAL = 20;

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

{
  Important: Methods and properties of objects in visual components can only be
  used in a method called using Synchronize, for example,

      Synchronize(UpdateCaption);  

  and UpdateCaption could look like,

    procedure IntervalThread.UpdateCaption;
    begin
      Form1.Caption := 'Updated in a thread';
    end; 
    
    or 
    
    Synchronize( 
      procedure 
      begin
        Form1.Caption := 'Updated in thread via an anonymous method' 
      end
      )
    );
    
  where an anonymous method is passed.
  
  Similarly, the developer can call the Queue method with similar parameters as 
  above, instead passing another TThread class as the first parameter, putting
  the calling thread in a queue with the other thread.
    
}

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

    if not(FEvent.WaitFor(INTERVAL) in [wrTimeout]) then Continue;


    for I := 0 to FItems.Count-1 do
    begin

      if Terminated then continue;

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
    end;

    Application.ProcessMessages;
  end;

end;

procedure TCyclicThread.SyncProc;
begin
  if Assigned( FOnNotify ) then
    FOnNotify( FData );
end;

end.
