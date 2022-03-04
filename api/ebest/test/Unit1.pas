unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.OleCtrls, XA_SESSIONLib_TLB,
  Vcl.StdCtrls, XA_DATASETLib_TLB;

type
  TForm1 = class(TForm)
    xas: TXASession;
    Memo1: TMemo;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    xaq: TXAQuery;
    xar: TXAReal;
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
    procedure xarReceiveRealData(ASender: TObject; const szTrCode: WideString);
    procedure xarRecieveLinkData(ASender: TObject; const szLinkName, szData,
      szFiller: WideString);
  private
    { Private declarations }
    procedure DoLog( sData : string);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  bRes : boolean;
begin
  if not xas.ConnectServer('demo.ebestsec.co.kr', 20001) then
  begin
    DoLog( xas.GetErrorMessage( xas.GetLastError ) );
    Exit;
  end;
  DoLog(' ------ Connect --------' );
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  if xas.IsConnected then
    xas.DisconnectServer;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  if not xas.Login('molpang', 'wnrdj111', '',1, false) then
  begin
    DoLog( '-- 로그인 시도 실패 --' );
    Exit;
  end;
end;

procedure TForm1.Button4Click(Sender: TObject);
var
  sBlockName : string;
  n : Integer;
begin

  //if xas. then
  sBlockName := 't3518InBlock';  n := 0;
  xaq.SetFieldData( sBlockName, 'kind', n, 'R');
  xaq.SetFieldData( sBlockName, 'symbol', n, 'USDKRWSMBS');
  xaq.SetFieldData( sBlockName, 'cnt', n, '1');
  xaq.SetFieldData( sBlockName, 'jgbn', n, '0');
//  xaq.SetFieldData( sBlockName, 'nmin', n, '');
  xaq.SetFieldData( sBlockName, 'cts_date', n, FormatDateTime('yyyymmdd', now));
  xaq.SetFieldData( sBlockName, 'cts_time', n, FormatDateTime('hhnnss', now));

  xaq.Request(false);

//  xar.SetFieldData( sBlockName, 'kind', 'S');
//  xar.SetFieldData( sBlockName, 'symbol', 'USD');
//  xar.SetFieldData( sBlockName, 'cnt', '1');
//  xar.SetFieldData( sBlockName, 'jgbn', '0');
//  xar.SetFieldData( sBlockName, 'nmin', '0');
//  xar.SetFieldData( sBlockName, 'cts_date', FormatDateTime('yyyymmdd', now));
//  xar.SetFieldData( sBlockName, 'cts_time', FormatDateTime('hhnnss', now));
////  xar.SetFieldData(sBlockName,'CUR', 'USD   ' );
//  xar.AdviseRealData;
end;

procedure TForm1.DoLog(sData: string);
begin
  memo1.Lines.Add( sData );
end;

procedure TForm1.xaqReceiveData(ASender: TObject; const szTrCode: WideString);
var
  n : integer;
  I: Integer;
begin


  DoLog('ReceiveData - ' + szTrCode + ' , ' );
  DoLog( 'res : ' +xaq.GetFieldData('t3518InBlock', 'cts_date', 0 )
      + ' :  ' +xaq.GetFieldData('t3518InBlock', 'cts_time', 0 ) );
  n := xaq.GetBlockCount('t3518OutBlock1');
  DoLog('ReceiveData - ' + IntTostr( n )  );

  for I := 1 to n do
  begin
    DoLog( Format('%d : %s', [ i,  xaq.GetFieldData( 't3518OutBlock1', 'price', 0) ] ));
    DoLog( Format('%d : %s', [ i,  xaq.GetFieldData( 't3518OutBlock1', 'symbol', 0) ] ));
  end;

end;

procedure TForm1.xaqReceiveMessage(ASender: TObject; bIsSystemError: WordBool;
  const nMessageCode, szMessage: WideString);
begin
  DoLog('ReceiveMessage - ' + nMessageCode + ' : ' + szMessage );
end;

procedure TForm1.xaqReceiveSearchRealData(ASender: TObject;
  const szTrCode: WideString);
begin
  DoLog('ReceiveSearchRealData - ' + szTrCode );
end;

procedure TForm1.xarReceiveRealData(ASender: TObject;
  const szTrCode: WideString);
begin
  DoLog('ReceiveRealData - ' + szTrCode );
end;

procedure TForm1.xarRecieveLinkData(ASender: TObject; const szLinkName, szData,
  szFiller: WideString);
begin
  DoLog('RecieveLinkData - ' + szLinkName + ','+ szData);
end;

procedure TForm1.xasDisconnect(Sender: TObject);
begin
  DoLog(' ------ DisConnect --------' );
end;

procedure TForm1.xasLogin(ASender: TObject; const szCode, szMsg: WideString);
begin
  DoLog( ' Login : ' + szCode + ' : ' +  szMsg);
end;

procedure TForm1.xasLogout(Sender: TObject);
begin
DoLog(' ------ Logout --------' );
end;

end.
