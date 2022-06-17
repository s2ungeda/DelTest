unit uNamedShareMemory;

interface

uses
  Windows, SysUtils;

type
  TNamedShareMemory=class
    const
      BUF_SIZE=256;
      TMP_NAME='TempName';
    public
      class function Read(Name: String): String;
      class function Write(Name, Msg: String): Boolean;
  end;

implementation

{ TNamedShareMemory }

// 읽기
// _____________________________________________________________________________
class function TNamedShareMemory.Read(Name: String): String;
var
  hMapFile: THandle;
  pBuf: PChar;
begin
  Result:='';
  if Length(Trim(Name))=0 then Name:=Self.TMP_NAME;

  hMapFile:=OpenFileMapping(FILE_MAP_ALL_ACCESS, False, PWideChar(Name));
  if hMapFile=0 then begin
    OutputDebugString(PWideChar(Format('[ERROR] OpenFileMapping = %d', [GetLastError])));
    Exit;
  end;

  pBuf:=MapViewOfFile(hMapFile, FILE_MAP_ALL_ACCESS, 0, 0, BUF_SIZE);
  if pBuf=nil then begin
    OutputDebugString(PWideChar(Format('[ERROR] MapViewOfFile = %d', [GetLastError])));
    CloseHandle(hMapFile);
    Exit;
  end;

  Result:=pBuf;
  UnmapViewOfFile(pBuf);
  CloseHandle(hMapFile);
end;

// 쓰기
// _____________________________________________________________________________
class function TNamedShareMemory.Write(Name, Msg: String): Boolean;
var
  hMapFile: THandle;
  pBuf: PChar;
begin
  Result:=False;

  if Length(Trim(Name))=0 then Name:=Self.TMP_NAME;

  hMapFile:=OpenFileMapping(FILE_MAP_ALL_ACCESS, False, PWideChar(Name));
  if hMapFile=0 then begin
    hMapFile:=CreateFileMapping(INVALID_HANDLE_VALUE, nil, PAGE_READWRITE, 0, BUF_SIZE, PWideChar(Name));
    if hMapFile=0 then begin
      OutputDebugString(PWideChar(Format('[ERROR] CreateFileMapping = %d', [GetLastError])));
      Exit;
    end;
  end;

  pBuf:=MapViewOfFile(hMapFile, FILE_MAP_ALL_ACCESS, 0, 0, BUF_SIZE);
  if pBuf=nil then begin
    OutputDebugString(PWideChar(Format('[ERROR] MapViewOfFile = %d', [GetLastError])));
    Exit;
  end;

  CopyMemory(pBuf, PChar(Msg), Length(Msg)*SizeOf(Char));
  UnmapViewOfFile(pBuf);
  Result:=True;
end;

end.
