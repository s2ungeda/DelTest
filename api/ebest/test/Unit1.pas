unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.OleCtrls, XA_SESSIONLib_TLB,
  Vcl.StdCtrls, XA_DATASETLib_TLB, Vcl.Menus, Vcl.ExtCtrls, Vcl.Buttons;

type
  TFrmExRate = class(TForm)
    xas: TXASession;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    xaq: TXAQuery;
    xar: TXAReal;
    TrayIcon1: TTrayIcon;
    PopupMenu1: TPopupMenu;
    Timer1: TTimer;
    N2: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
    Timer2: TTimer;
    Edit1: TEdit;
    sbTimer: TSpeedButton;
    lbPrice: TLabel;
    lbLog: TLabel;
    lbTimer: TLabel;
    Button5: TButton;
    procedure xasDisconnect(Sender: TObject);
    procedure xasLogin(ASender: TObject; const szCode, szMsg: WideString);
    procedure xasLogout(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure xaqReceiveData(ASender: TObject; const szTrCode: WideString);
    procedure xaqReceiveMessage(ASender: TObject; bIsSystemError: WordBool;
      const nMessageCode, szMessage: WideString);
    procedure xaqReceiveSearchRealData(ASender: TObject;
      const szTrCode: WideString);

    procedure N4Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure sbTimerClick(Sender: TObject);
    procedure N2Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
  private
    { Private declarations }
    FServerIP : string;
    FPort     : integer;
    FUserID   : string;
    FPass     : string;
    FInterval : integer;
    FTRCode   : string;
    FSymbolCode : string;

    FReady : boolean;
    procedure DoLog( sData : string; iType : integer = 0);
    function LoadConfig: boolean;
    procedure SaveFile(sData: string);
  public
    { Public declarations }
  end;

var
  FrmExRate: TFrmExRate;

implementation

uses
 system.IniFiles
 ;

{$R *.dfm}

procedure TFrmExRate.Button1Click(Sender: TObject);
var
  bRes : boolean;
begin
  if not xas.ConnectServer(FServerIP, FPort) then
  begin
    DoLog( xas.GetErrorMessage( xas.GetLastError ) );
    Exit;
  end;
  DoLog(' ------ Connect --------' );
  sleep(200);
  Button3Click( nil );
end;

procedure TFrmExRate.Button2Click(Sender: TObject);
begin
  FReady := false;
  sbTimer.Down := false;
  sbTimerClick( nil );
  if xas.IsConnected then
    xas.DisconnectServer;
end;

procedure TFrmExRate.Button3Click(Sender: TObject);
begin
  if not xas.Login(FUserID, FPass, '', 0 , false) then
  begin
    DoLog( '-- 로그인 시도 실패 --' );
    Exit;
  end;
end;

procedure TFrmExRate.Button4Click(Sender: TObject);
var
  n : Integer;
  sBlockName : string;
begin

  if not FReady then Exit;

  n := 0;
  sBlockName := FTRCode + 'InBlock';
  xaq.SetFieldData( sBlockName, 'kind', n, 'R');
  xaq.SetFieldData( sBlockName, 'symbol', n, FSymbolCode);
  xaq.SetFieldData( sBlockName, 'cnt', n, '1');
  xaq.SetFieldData( sBlockName, 'jgbn', n, '0');

  xaq.SetFieldData( sBlockName, 'cts_date', n, FormatDateTime('yyyymmdd', now));
  xaq.SetFieldData( sBlockName, 'cts_time', n, FormatDateTime('hhnnss', now));

  xaq.Request(false);

end;

procedure TFrmExRate.Button5Click(Sender: TObject);
begin
  Hide;
end;

procedure TFrmExRate.DoLog(sData: string; iType : integer);
begin
  //memo1.Lines.Add( sData );
  if iType = 0 then
    lbLog.Caption := sData
  else if iType = 1 then
    lbPrice.Caption := Format('%s : %s', [ sData, FormatDateTime('hh:nn:ss', now)]);
end;

procedure TFrmExRate.FormCreate(Sender: TObject);
begin
  FReady  := false;

//  xaq.ResFileName := 'Bin/

  if not LoadConfig then begin
    ShowMessage('Failed LoadConfig');
    Close;
  end
  else begin
    FPass := 'wnrdj111';
    Button1Click( nil );
  end;
end;

procedure TFrmExRate.N2Click(Sender: TObject);
begin
  Show;
end;

procedure TFrmExRate.N4Click(Sender: TObject);
begin
  close;
end;

procedure TFrmExRate.sbTimerClick(Sender: TObject);
begin
//  if sbTimer.Down then
  Timer1.Enabled  :=  sbTimer.Down;
  Timer1.Interval :=  StrToIntDef( edit1.Text , 3 ) * 1000;
  if sbTimer.Down then
    sbTimer.Caption := 'Stop'
  else
    sbTimer.Caption := 'Start';

  if Timer1.Enabled then
    lbTimer.Caption := '동작중'
  else
    lbTimer.Caption := '멈춤';
end;

procedure TFrmExRate.Timer1Timer(Sender: TObject);
begin
  Button4Click(nil);
end;

procedure TFrmExRate.xaqReceiveData(ASender: TObject; const szTrCode: WideString);
var
  n , I: Integer;
  sBlockName, sData : string;
begin

  sBlockName := Format('%sOutBlock1', [ FTRCode ] );

  n := xaq.GetBlockCount(sBlockName);
  for I := 0 to n-1 do
  begin
    sData := xaq.GetFieldData( sBlockName, 'price', i) ;
    DoLog( Format('%s', [ sData ]), 1 );
    SaveFile( sData );
  end;

end;

procedure TFrmExRate.SaveFile( sData : string);
var
  LogFileName : string;
  OutFile: TextFile;

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
begin

  LogFileName :='data\exchangeRate.log';

  try

    if not IsFileUse(LogFileName) then begin
      AssignFile(OutFile, LogFileName);
      ReWrite(OutFile);
      try
        Writeln(OutFile,sData);
      finally
        CloseFile(OutFile);
      end;
    end;
  Except
  end;

end;

procedure TFrmExRate.xaqReceiveMessage(ASender: TObject; bIsSystemError: WordBool;
  const nMessageCode, szMessage: WideString);
begin
  DoLog('ReceiveMessage - ' + nMessageCode + ' : ' + szMessage );
end;

procedure TFrmExRate.xaqReceiveSearchRealData(ASender: TObject;
  const szTrCode: WideString);
begin
  DoLog('ReceiveSearchRealData - ' + szTrCode );
end;


procedure TFrmExRate.xasDisconnect(Sender: TObject);
begin
  FReady  := false;
  DoLog(' ------ DisConnect --------' );
end;

procedure TFrmExRate.xasLogin(ASender: TObject; const szCode, szMsg: WideString);
begin
  if szCode = '0000' then begin
    FReady := true;
    sbTimer.Down := true;
    sbTimerClick( nil );
  end;

  DoLog( ' Login : ' + szCode + ' : ' +  szMsg);
end;

procedure TFrmExRate.xasLogout(Sender: TObject);
begin
DoLog(' ------ Logout --------' );
end;


function TFrmExRate.LoadConfig: boolean;
var
  pIniFile : TIniFile;
  stDir : string;
begin
  result := true;

  try
    try
      stDir := ExtractFilePath( paramstr(0) )+'Config\';
      pIniFile := TIniFile.Create(stDir + 'Config.ini' );

      /////////////////////////////////////////////////////////////
      FServerIP := pIniFile.ReadString('EBEST', 'ServerIP', 'Sauri');
      FPort     := pIniFile.ReadInteger('EBEST', 'Port', 20001);
      FUserID   := pIniFile.ReadString('EBEST', 'UserID', 'Sauri');
//      FPass     := pIniFile.ReadString('EBEST', 'LogDir', 'Sauri');

      FInterval := pIniFile.ReadInteger('EBEST', 'Interval', 3);
      FTRCode   := pIniFile.ReadString('EBEST', 'TRCode', 'Sauri');
      FSymbolCode   := pIniFile.ReadString('EBEST', 'SymbolCode', 'Sauri');
      /////////////////////////////////////////////////////////////

    except
      result := false;
    end;
  finally
    pIniFile.Free;
  end;

end;

end.
