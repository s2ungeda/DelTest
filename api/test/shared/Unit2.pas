unit Unit2;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, REST.Types,
  REST.Client, Data.Bind.Components, Data.Bind.ObjectScope
  , USharedData
  ;


type


  TForm2 = class(TForm)
    Edit1: TEdit;
    Button1: TButton;
    Timer1: TTimer;
    CheckBox1: TCheckBox;
    RESTClient1: TRESTClient;
    req: TRESTRequest;
    res: TRESTResponse;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
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
  iSize : integer;
begin
  llInit := False;
  iSize  := SizeOf(TSharedData);

  if hMapEvent = 0 then
  begin
    hMapEvent := CreateEvent(nil, False, False, PChar('wowsniffDataReady'));
    if hMapEvent = 0 then RaiseLastOSError;
  end;

  if hMapLock = 0 then
  begin
    hMapLock := CreateMutex(nil, False, PChar('wowsniffDataLock'));
    if hMapLock = 0 then RaiseLastOSError;
  end;

  if hMapping = 0 then
  begin
    hMapping := CreateFileMapping(INVALID_HANDLE_VALUE, nil, PAGE_READWRITE, 0, iSize, PChar('wowsniff'));
    if hMapping = 0 then RaiseLastOSError;
    // Check if already exists
    llInit := (GetLastError() <> ERROR_ALREADY_EXISTS);
  end;

  if PMapData = nil then
  begin
    PMapData := MapViewOfFile(hMapping, FILE_MAP_WRITE, 0, 0, iSize);
    if PMapData = nil then RaiseLastOSError;

    if llInit then
    begin
      // Init block to #0 if newly created
      ZeroMemory(PMapData, iSize);
    end;
  end;
end;


procedure TForm2.Button1Click(Sender: TObject);
var
  vData : PSharedData;
  aItem : TDataItem;
  data, tmp : ansiString;
begin
  LockMap;
  try
    vData := PSharedData( PMapData);

    if vData.IsFull then Exit;
    // 한칸 앞으로 이동 후 데이타 추가
    vData.Rear := ( vData.Rear + 1 ) mod Q_SIZE;

    FillChar( aItem.data, DATA_SIZE, $0 );
    data  :=  AnsiString( edit1.Text );
    aItem.ex  := 'A';
    aItem.market  := 'B';
    tmp := AnsiString( Format('%4.4d', [ Length(data)]) );
    move(  tmp[1],  aItem.size, sizeof(aItem.size));
    move(  data[1], aItem.data, Length(data) );

    CopyMemory(@(vData.SharedData[vData.Rear]), @aItem, sizeof(TDataItem));
//    PDword(PMapData)^ := StrToInt(Edit1.Text);
    SetEvent(hMapEvent);
  finally
    UnlockMap;
  end;
end;

procedure TForm2.Button2Click(Sender: TObject);
begin
  req.Resource  := '/api/v3/depth?symbol=BTCUSDT&limit=1000';
  req.Method    := rmGET;
  req.Execute;
  edit1.Text := req.Response.Content;
end;

procedure TForm2.CheckBox1Click(Sender: TObject);
begin
  Timer1.Enabled := CheckBox1.Checked;
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

procedure TForm2.Timer1Timer(Sender: TObject);
begin
  Edit1.Text := FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', now) ;
  Button1Click(nil);
end;

procedure TForm2.UnlockMap;
begin
  ReleaseMutex(hMapLock);
end;

{ TSharedData }


end.
