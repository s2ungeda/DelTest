unit UExchangeRate;

interface

uses
  System.Classes, System.SysUtils

  , XA_SESSIONLib_TLB,  XA_DATASETLib_TLB
  ;

type

  TExchangeRate = class
  private
    FXas: TXASession;
    FXaq: TXAQuery;
    FUserID: string;
    FPort: integer;
    FPass: string;
    FServerIP: string;
    FErrMessage: string;
    FReady: boolean;
    FPrice: double;

    procedure OnDisconnect(Sender: TObject);
    procedure OnLogin(ASender: TObject; const szCode, szMsg: WideString);
    procedure OnReceiveData(ASender: TObject; const szTrCode: WideString);

  public
    constructor create;
    destructor  destroy; override;

    function Init( sIP : string; iPort : integer;  sID, sPass : string ) : boolean;
    function Connect: boolean;

    procedure Login;
    procedure Close;
    procedure RequestPrice;

    property Xas: TXASession read FXas;
    property Xaq: TXAQuery   read FXaq;

    property Price : double read FPrice;

    property Ready : boolean read FReady;

    property ServerIP : string read FServerIP;
    property Port     : integer read FPort;
    property UserID   : string read FUserID;
    property Pass     : string read FPass;

    property ErrMessage : string read FErrMessage;
  end;

implementation

{ TExchangeRate }

procedure TExchangeRate.Close;
begin
  if FXas.IsConnected then
    FXas.DisconnectServer;
end;

function TExchangeRate.Connect: boolean;
begin
  if FXas.IsConnected then
    FXas.DisconnectServer;

  if not FXas.ConnectServer(FServerIP, FPort) then
  begin
    FErrMessage := FXas.GetErrorMessage( FXas.GetLastError ) ;
    Exit (false);
  end;

  Result := true;
end;

constructor TExchangeRate.create;
begin
  FXas:= TXASession.Create( nil );
  FXaq:= TXAQuery.Create( nil );

  FErrMessage := '';
  FReady      := false;

end;

destructor TExchangeRate.destroy;
begin
  FXas.Free;
  FXaq.Free;
end;

function TExchangeRate.Init(sIP: string; iPort: integer; sID,
  sPass: string): boolean;
begin

  FUserID:= sID;
  FPort:= iPort;
  FPass:= sPass;
  FServerIP:= sIP;

  Result := Connect;

end;

procedure TExchangeRate.Login;
begin
  if not xas.Login( FUserID, FPass, '', 0, false) then
  begin
    FErrMessage := 'Failed Login';
    Exit;
  end;
end;

procedure TExchangeRate.OnDisconnect(Sender: TObject);
begin
  FReady := false;
end;

procedure TExchangeRate.OnLogin(ASender: TObject; const szCode,
  szMsg: WideString);
begin
  if szCode = '0000' then
    FReady := true
  else
    FErrMessage :=  szMsg;
end;

procedure TExchangeRate.OnReceiveData(ASender: TObject;
  const szTrCode: WideString);
var
  n : integer;
  I: Integer;
begin

  n := xaq.GetBlockCount('t3518OutBlock1');

  for I := 1 to n do
  begin
    FPrice := StrToFloatDef( xaq.GetFieldData( 't3518OutBlock1', 'price', 0) , 0.0 );
  end;

end;

procedure TExchangeRate.RequestPrice;
var
  sBlockName : string;
  n : Integer;
begin

  sBlockName := 't3518InBlock';  n := 0;
  FXaq.SetFieldData( sBlockName, 'kind', n, 'R');
  FXaq.SetFieldData( sBlockName, 'symbol', n, 'USDKRWSMBS');
  FXaq.SetFieldData( sBlockName, 'cnt', n, '1');
  FXaq.SetFieldData( sBlockName, 'jgbn', n, '0');

  FXaq.SetFieldData( sBlockName, 'cts_date', n, FormatDateTime('yyyymmdd', now));
  FXaq.SetFieldData( sBlockName, 'cts_time', n, FormatDateTime('hhnnss', now));

  FXaq.Request(false);
end;

end.
