unit Unit2;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, REST.Types,
  REST.Client, Data.Bind.Components, Data.Bind.ObjectScope
  , USharedData  , USharedThread
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
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    FOnPushData: TSharedPushData;
    procedure LockMap;
    procedure UnlockMap;
    { Private declarations }
  public
    { Public declarations }
    refCnt : integer;
    mt : TSharedThread;
    procedure OnSharedDataNotify( aData : TDataItem );
    property OnPushData : TSharedPushData read FOnPushData write FOnPushData;
  end;

var
  Form2: TForm2;

  hTerminate: THandle;
  hMapLock: THandle;
  hMapEvent: THandle;
  hMapping: THandle;
  PMapData: Pointer;

implementation

uses
  USharedConsts
  ;

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
    hMapEvent := CreateEvent(nil, False, False, PChar('dalinEvent'));
    if hMapEvent = 0 then RaiseLastOSError;
  end;

  if hMapLock = 0 then
  begin
    hMapLock := CreateMutex(nil, False, PChar('dalinLock'));
    if hMapLock = 0 then RaiseLastOSError;
  end;

  if hMapping = 0 then
  begin
    hMapping := CreateFileMapping(INVALID_HANDLE_VALUE, nil, PAGE_READWRITE, 0, iSize, PChar('dalinMap'));
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

  data  :=  AnsiString( edit1.Text );
  mt.PushData('B', 'F', data, IntToStr( refCnt ) );
  inc( refCnt );

  exit;

  LockMap;
  try
    vData := PSharedData( PMapData);

    if vData.IsFull then Exit;
    // 한칸 앞으로 이동 후 데이타 추가
    vData.Rear := ( vData.Rear + 1 ) mod Q_SIZE;
    inc( refCnt );
    FillChar( aItem.data, DATA_SIZE, $0 );
    data  :=  AnsiString( edit1.Text );
    aItem.exKind  := 'B';
    aItem.market  := 'F';
    aItem.trDiv   := 'P';
    aItem.ref     := IntToStr( refCnt );
    aItem.size    := Length( data );
//    tmp := AnsiString( Format('%4.4d', [ Length(data)]) );
//    move(  tmp[1],  aItem.size, sizeof(aItem.size));
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
//  OpenMap;
  mt := TSharedThread.Create( OnSharedDataNotify, true);

  refCnt := 0;
end;

procedure TForm2.FormDestroy(Sender: TObject);
begin
  if mt <> nil then
    mt.Terminate;
//  UnmapViewOfFile(PMapData);
//  CloseHandle(hMapping);
//  CloseHandle(hMapLock);
//  CloseHandle(hMapEvent);
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

procedure TForm2.OnSharedDataNotify(aData: TDataItem);
var
  s : string;
begin

  s := Format( ' %s, %s, %s, %s ', [ aData.exKind, aData.market, ansistring(aData.data), aData.ref  ]);
  memo1.Lines.Add( s )
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
