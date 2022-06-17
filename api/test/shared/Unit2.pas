unit Unit2;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

 const
  MAPFILESIZE = 512;

type
  TForm2 = class(TForm)
    Edit1: TEdit;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    procedure LockMap;
    procedure UnlockMap;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

  hTerminate: THandle;
  hMapLock: THandle;
  hMapEvent: THandle;
  hMapping: THandle;
  PMapData: Pointer;

implementation

{$R *.dfm}

procedure OpenMap;
var
  llInit: Boolean;
begin
  llInit := False;

  if hMapEvent = 0 then
  begin
    hMapEvent := CreateEvent(nil, True, False, PChar('wowsniffDataReady'));
    if hMapEvent = 0 then RaiseLastOSError;
  end;

  if hMapLock = 0 then
  begin
    hMapLock := CreateMutex(nil, False, PChar('wowsniffDataLock'));
    if hMapLock = 0 then RaiseLastOSError;
  end;

  if hMapping = 0 then
  begin
    hMapping := CreateFileMapping(INVALID_HANDLE_VALUE, nil, PAGE_READWRITE, 0, MAPFILESIZE, PChar('wowsniff'));
    if hMapping = 0 then RaiseLastOSError;
    // Check if already exists
    llInit := (GetLastError() <> ERROR_ALREADY_EXISTS);
  end;

  if PMapData = nil then
  begin
    PMapData := MapViewOfFile(hMapping, FILE_MAP_WRITE, 0, 0, MAPFILESIZE);
    if PMapData = nil then RaiseLastOSError;

    if llInit then
    begin
      // Init block to #0 if newly created
      ZeroMemory(PMapData, MAPFILESIZE);
    end;
  end;
end;





procedure TForm2.Button1Click(Sender: TObject);
begin
  LockMap;
  try
    PDword(PMapData)^ := StrToInt(Edit1.Text);
    SetEvent(hMapEvent);
  finally
    UnlockMap;
  end;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  OpenMap;
end;

procedure TForm2.FormDestroy(Sender: TObject);
begin
  UnmapViewOfFile(PMapData);
  CloseHandle(hMapping);
  CloseHandle(hMapLock);
  CloseHandle(hMapEvent);
end;

procedure TForm2.LockMap;
var
  llRet: DWORD;
 begin
  llRet := WaitForSingleObject(hMapLock, 5000);
  if llRet = WAIT_OBJECT_0 then Exit;
  if llRet <> WAIT_FAILED then SetLastError(llRet);
  RaiseLastOSError;
end;

procedure TForm2.UnlockMap;
begin
  ReleaseMutex(hMapLock);
end;

end.
