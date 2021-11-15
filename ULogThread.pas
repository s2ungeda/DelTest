unit ULogThread;

interface

uses
  System.Classes, System.SysUtils  , Windows, Forms
  ;

type

  {
  1. LogLevel
  2. PreFix
  3. Data
  }

  PLogData = ^TLogData;
  TLogData = record
    Level : char;
    PreFix: String;
    Data: String;
  end;


  TLogThread = class(TThread)
  private

    LogMutex: HWND;

    { Private declarations }
    function  LogPopQueue: PLogData;
    procedure LogWriteFile(vLogData: PLogData); overload;
    procedure LogFileWrite(cLevel : char; stPrefix, stData : string);
  protected
    procedure Execute; override;
  public
    LogQueue: TList;

    constructor Create;
    destructor Destroy; override;

    procedure Log( cLevel : char; stPrefix, stData : string ); overload;
    procedure Log( cLevel : char; stPrefix : string; const fmt: string; const Args: array of const ); overload;


  end;

implementation

{ 
  Important: Methods and properties of objects in visual components can only be
  used in a method called using Synchronize, for example,

      Synchronize(UpdateCaption);  

  and UpdateCaption could look like,

    procedure TLogThread.UpdateCaption;
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

{ TLogThread }

constructor TLogThread.Create;
begin
  Inherited Create(False);
  FreeOnTerminate := True;
  LogMutex := CreateMutex(nil, False, 'LogMutex');
  LogQueue := TList.Create;

//  if gEnv.RunMode = rtSimulation then
//    Priority := tpNormal
//  else
//    Priority := tpLowest;
end;

destructor TLogThread.Destroy;
begin
  CloseHandle(LogMutex);
  LogQueue.Free;
  Inherited Destroy;
end;

procedure TLogThread.Execute;
var
  vLogData: PLogData;
begin
  { Place thread code here }
  while not Terminated do begin
    WaitForSingleObject(Handle, 1);
    if LogQueue.Count > 0 then begin
      vLogData := LogPopQueue;
      if vLogData = nil then Continue;
      LogWriteFile(vLogData);
      Dispose(vLogData);
      Application.ProcessMessages;
    end;
  end;
end;

procedure TLogThread.LogWriteFile(vLogData: PLogData);
begin
  LogFileWrite(vLogData.Level, vLogData.PreFix, vLogData.Data); //파일로 쓰기..
end;

procedure TLogThread.LogFileWrite(cLevel: char; stPrefix, stData: string);
  function IsFileUse(fName: String): Boolean;
  var
    HFile: THandle;
  begin
    Result := false;
    if not FileExists(fName) then exit;
    HFile := CreateFile(PChar(fName), GENERIC_READ or GENERIC_WRITE, 0, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
    Result := (HFile = INVALID_HANDLE_VALUE);
    if not Result then begin
      try
        //Memo_Log.Lines.Add('Value = ' + IntToStr(HFile));
      finally
        CloseHandle(HFile);
      end;
    end;
  end;

var
  OutFile: TextFile;
  stDate, LogDir, LogFileName: String;
begin

  stDate := FormatDateTime('YYYYMMDD',Date);
  LogDir := ExtractFilePath(ParamStr(0))+'Log\';

  if not DirectoryExists(LogDir) then CreateDir(LogDir);

  LogFileName := LogDir + stPrefix+'_'+stDate + '.log';

  try
    if not IsFileUse(LogFileName) then begin
    {$I-}
      AssignFile(OutFile, LogFileName);
      try
        if Not FileExists(LogFileName) then
          ReWrite(OutFile)
        else Append(OutFile);
        Writeln(OutFile,stData);
      finally
        CloseFile(OutFile);
      end;
    {$I+}
    end;
  Except
  end;

end;


function TLogThread.LogPopQueue: PLogData;
var
  vResult: PLogData;
begin
  Result := nil;

  if LogQueue.Count < 1 then exit;
  New(Result);
  vResult := PLogData(LogQueue.Items[0]);
  WaitForSingleObject(LogMutex, INFINITE);
  LogQueue.Delete(0);
  ReleaseMutex(LogMutex);
  Result := vResult;

end;

procedure TLogThread.Log(cLevel: char; stPrefix: string; const fmt: string;
  const Args: array of const);
begin
  Log( cLevel, stPreFix, Format( fmt, Args ) );
end;



procedure TLogThread.Log(cLevel: char; stPreFix, stData: string);
var
  vLogData: PLogData;
  stDiv   : string;
begin
  New(vLogData);

  stDiv := 'DEBUG';

  vLogData.Level  := cLevel;
  vLogData.PreFix := stPreFix;
  vLogData.Data   := Format('[%s][%s]:%s', [ FormatDateTime('HH:NN:SS.ZZZ', Now)
    , stDiv, stData ]);

  WaitForSingleObject(LogMutex, INFINITE);
  LogQueue.Add(vLogData);
  ReleaseMutex(LogMutex);
end;


end.
