unit USharedThread;

interface

uses
  Winapi.Windows, System.Classes, System.SysUtils,
  USharedData
  ;
type

  TSharedThread = class(TThread)
  private
    hTerminate: THandle;

    hMapLock: array [0..1] of THandle;
    hMapEvent:array [0..1] of THandle;
    hMapping: array [0..1] of THandle;
    PMapData: array [0..1] of Pointer;

    FOnNotify: TSharedDataNotify;
    FData : string;
    FDataItem : TDataItem;
    FIsMain   : boolean;

    POP, PUSH : integer;

    MapName, LockName, EvntName : array [0..1] of string;

    procedure LockMap( idx : integer );
    procedure UnlockMap( idx : integer );
    procedure OpenMap;
    { Private declarations }
  protected
    procedure Execute; override;
    procedure DoTerminate; override;
    procedure TerminatedSet; override;

    procedure SyncProc;

  public
    constructor Create( aCallBack : TSharedDataNotify; bMain : boolean );
    destructor Destroy; override;

    procedure PushData( c1, c2, c3 : char; s1, s2 : string );

    property OnNotify : TSharedDataNotify read FOnNotify write FOnNotify;
  end;

implementation

uses

   GLibs
 // , GApp
  , USharedConsts
  ;


constructor TSharedThread.Create( aCallBack : TSharedDataNotify; bMain : boolean );

begin
  FOnNotify := aCallBack;
  // dalin : true   rest : false;
  FIsMain   := bMain;
  if FIsMain then begin
    PUSH := 0; POP := 1;
  end else begin
    PUSH := 1; POP := 0;
  end;

  MapName[PUSH] := ifThenStr( FIsMain, 'dalinMap', 'restMap' );
  LockName[PUSH]:= ifThenStr( FIsMain, 'dalinLock', 'restLock' ) ;
  EvntName[PUSH]:= ifThenStr( FIsMain, 'dalinEvent', 'restEvent' );

  MapName[POP] := ifThenStr(  FIsMain, 'restMap', 'dalinMap' );
  LockName[POP]:= ifThenStr(  FIsMain, 'restLock', 'dalinLock' ) ;
  EvntName[POP]:= ifThenStr(  FIsMain, 'restEvent', 'dalinEvent' );

  inherited Create(False);
  hTerminate := CreateEvent(nil, True, False, nil);
  if hTerminate = 0 then RaiseLastOSError;

  OpenMap;
end;

destructor TSharedThread.Destroy;
begin
  if hTerminate <> 0 then CloseHandle(hTerminate);

  UnmapViewOfFile(PMapData[PUSH]);
  CloseHandle(hMapping[PUSH]);
  CloseHandle(hMapLock[PUSH]);
  CloseHandle(hMapEvent[PUSH]);

end;

procedure TSharedThread.DoTerminate;
begin
  FOnNotify := nil;
  // 내가 만든거..
  UnmapViewOfFile(PMapData[PUSH]);
  CloseHandle(hMapping[PUSH]);
  CloseHandle(hMapLock[PUSH]);
  CloseHandle(hMapEvent[PUSH]);
  // 니가 만든거..
  if PMapData[POP]  <> nil then UnmapViewOfFile(PMapData[POP]);
  if hMapping[POP]  <> 0   then CloseHandle(hMapping[POP]);
  if hMapEvent[POP] <> 0   then CloseHandle(hMapEvent[POP]);
  if hMapLock[POP]  <> 0   then CloseHandle(hMapLock[POP]);
  inherited;
end;

procedure TSharedThread.Execute;
var
  llInit: Boolean;
  llRet, llValue: DWORD;
  llHandles: array[0..1] of THandle;
  iSize, i : integer;
  vData : PSharedData;
  tmp : string;
  item : TDataItem;
  aaa : ansiString;
begin
  iSize  := SizeOf(TSharedData);
  hMapEvent[POP] := CreateEvent(nil, False, False, PChar(EvntName[POP]));
  if hMapEvent[POP] = 0 then RaiseLastOSError;

  hMapLock[POP] := CreateMutex(nil, False, PChar(LockName[POP]));
  if hMapLock[POP] = 0 then RaiseLastOSError;

  hMapping[POP] := CreateFileMapping(INVALID_HANDLE_VALUE, nil, PAGE_READWRITE, 0, iSize, PChar(MapName[POP]));
  if hMapping [POP]= 0 then RaiseLastOSError;
  // Check if already exists
  llInit := (GetLastError() <> ERROR_ALREADY_EXISTS);

  PMapData[POP] := MapViewOfFile(hMapping[POP], FILE_MAP_WRITE, 0, 0, iSize);
  if PMapData[POP] = nil then RaiseLastOSError;
  if llInit then
  begin
    // Init block to #0 if newly created
    FillChar(PMapData[POP]^, iSize, 0);
  end;

  llHandles[0] := hMapEvent[POP];
  llHandles[1] := hTerminate;

  while not Terminated do
  begin
    llRet := WaitForMultipleObjects(2, PWOHandleArray(@llHandles), False, INFINITE);
    case llRet of
      WAIT_OBJECT_0+0:
      begin
        llRet := WaitForSingleObject(hMapLock[POP], 5000);
        if llRet = WAIT_OBJECT_0 then
        begin
          try

            vData   := PSharedData( PMapData[POP] );
            var iWorkCnt : integer;  iWorkCnt := 0;

            while ( not vData.IsEmpty ) and ( iWorkCnt < 10 ) do
            begin

              vData.Front := ( vData.Front + 1) mod Q_SIZE;
              inc(iWorkCnt);
              CopyMemory(@item, @(vData.SharedData[vData.Front])  , sizeof( TDataItem) )        ;

 //             App.DebugLog('queue(%d) : %d, %d %s', [ vData.Count, vData.Front, vdata.Rear, item.ref ]    );

              FDataItem := item;
              Synchronize(SyncProc );
              //App.DebugLog('---------- %d, %s, %s', [ FDataItem.size, AnsiString( FDataItem.data ), FDataItem.ref ] );

            end;

          finally
            UnlockMap(POP);
          end;
          // use llValue as needed...
          Continue;
        end;
      end;
      WAIT_OBJECT_0+1:
      begin
        Exit;
      end;
    end;
    if llRet <> WAIT_FAILED then SetLastError(llRet);
    RaiseLastOSError;
  end;
end;


procedure TSharedThread.PushData(c1, c2, c3: char; s1, s2: string);
var
  vData : PSharedData;
  aItem : TDataItem;
  data  : ansiString;
begin

  LockMap(PUSH);
  try

    vData := PSharedData( PMapData[PUSH] );

    if vData.IsFull then Exit;
    // 한칸 앞으로 이동 후 데이타 추가
    vData.Rear := ( vData.Rear + 1 ) mod Q_SIZE;

    FillChar( aItem.data, DATA_SIZE, $0 );
    data  :=  s1;
    aItem.exKind  := c1;
    aItem.market  := c2;
    aItem.ref     := s2;
    aItem.size    := Length( data );

    move(  data[1], aItem.data, Length(data) );

    CopyMemory(@(vData.SharedData[vData.Rear]), @aItem, sizeof(TDataItem));
//    PDword(PMapData)^ := StrToInt(Edit1.Text);
    SetEvent(hMapEvent[PUSH]);

  finally
    UnlockMap(PUSH);
  end;
end;




procedure TSharedThread.OpenMap;
var
  llInit: Boolean;
  iSize : integer;
begin
  llInit := False;
  iSize  := SizeOf(TSharedData);

  if hMapEvent[PUSH] = 0 then
  begin
    hMapEvent[PUSH] := CreateEvent(nil, False, False, PChar(EvntName[PUSH]));
    if hMapEvent[PUSH] = 0 then RaiseLastOSError;
  end;

  if hMapLock[PUSH] = 0 then
  begin
    hMapLock[PUSH] := CreateMutex(nil, False, PChar(LockName[PUSH]));
    if hMapLock[PUSH] = 0 then RaiseLastOSError;
  end;

  if hMapping[PUSH] = 0 then
  begin
    hMapping[PUSH] := CreateFileMapping(INVALID_HANDLE_VALUE, nil, PAGE_READWRITE, 0, iSize, PChar(MapName[PUSH]));
    if hMapping[PUSH] = 0 then RaiseLastOSError;
    // Check if already exists
    llInit := (GetLastError() <> ERROR_ALREADY_EXISTS);
  end;

  if PMapData[PUSH] = nil then
  begin
    PMapData[PUSH] := MapViewOfFile(hMapping[PUSH], FILE_MAP_WRITE, 0, 0, iSize);
    if PMapData[PUSH] = nil then RaiseLastOSError;

    if llInit then
    begin
      // Init block to #0 if newly created
      ZeroMemory(PMapData[PUSH], iSize);
    end;
  end;

end;


procedure TSharedThread.LockMap( idx : integer );
var
  llRet: DWORD;
 begin
  llRet := WaitForSingleObject(hMapLock[idx], 5000);
  if llRet = WAIT_OBJECT_0 then Exit;
  if llRet <> WAIT_FAILED then SetLastError(llRet);
  RaiseLastOSError;
end;

procedure TSharedThread.UnlockMap( idx : integer );
begin
  ReleaseMutex(hMapLock[idx]);
end;

procedure TSharedThread.SyncProc;
begin
  if Assigned( FOnNotify ) then
    FOnNotify( FDataItem );
end;

procedure TSharedThread.TerminatedSet;
begin
  SetEvent(hTerminate);
end;

end.
