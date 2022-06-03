unit UTCyclicThreads;

interface

uses
  System.Classes, System.SyncObjs
  , Windows;

const
  INTERVAL = 20;

type




  TCyclicThread = class(TThread)
  private
    { Private declarations }
   FMutex  : HWND;
   FITems  : TStrings;
   procedure SyncProc;
  protected
    procedure Execute; override;
  public
    constructor Create( AItems : TStrings );
    destructor Destroy; override;
  end;

implementation

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

constructor TCyclicThread.Create;
begin

  inherited Create(false);
  FreeOnTerminate := false;
  Priority  := tpNormal;

  FMutex  := CreateMuTex( nil, false, 'Binance_Mutex_1' );
end;

destructor TCyclicThread.Destroy;
begin

  inherited;
end;

procedure TCyclicThread.Execute;
begin
  { Place thread code here }

  while not Terminated do
  begin
    WaitForSingleObject( FMutex, INTERVAL );
  end;

end;

procedure TCyclicThread.SyncProc;
begin

end;

end.
