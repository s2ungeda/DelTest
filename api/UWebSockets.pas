unit UWebSockets;
interface
uses
  System.Classes, System.SysUtils, System.DateUtils , Windows
  , SyncObjs
  , ScWebSocketClient
  , UApiTypes
  ;
type
  PReceiveData = ^TReceiveData;
  TReceiveData = record
    Size: Integer;
    Packet  : string;
//    Packet: array of Char;
  end;
{
sConnecting :  Indicates that the client is in the state of establishing a connection.
sOpen : Indicates that a connection with a Web server was established.
sCloseSent : Indicates that the client sent a Close control message to the server and is waiting for a response from the server.
sCloseReceived : Indicates that the client received a Close control message from the server, but hasn't sent a response message to the server.
sClosed : Indicates that the connection was closed normally.
sAborted : Indicates that the connection was closed abnormally, e.g., without sending or receiving a Close control message.
}
  TWebsocket = class//( TThread )
  private
    //FEvent: TEvent;
    //FData : PReceiveData;
    FWebSocket: TScWebSocketClient;
    FEndPoint: string;
    FLiveTime: TDateTime;
    FSockDiv: integer;
    FConnectTry: integer;
    FSeq: integer;
    FOnNotify: TGetStrProc;
    FPort: integer;
    FExchangeKind: TExchangeKind;


    function Desc : string;
  protected
//    procedure Execute ; override;
//    procedure SyncProc; virtual ;
    procedure OnAfterConnect(Sender: TObject);  virtual;
    procedure OnAfterDisconnect(Sender: TObject);  virtual;
    procedure OnConnectFail(Sender: TObject); virtual;
    procedure OnControlMessage(Sender: TObject;
      ControlMessageType: TScWebSocketControlMessageType); virtual;
    procedure OnMessage(Sender: TObject; const Data: TArray<System.Byte>;
      MessageType: TScWebSocketMessageType; EndOfMessage: Boolean); virtual;
    procedure OnBeforeConnect(Sender: TObject);


  public
//    FReceiveMutex : HWND;
//    FSocketMute   : HWND;
//    FQueue        : TList;
    constructor Create( iSockDiv, iSeq : integer; aExKind : TExchangeKind );
    destructor Destroy; override;

    procedure SubscribeAll; virtual; abstract;

    procedure init(sAddr : string; iPort : integer = 443);
    procedure DoConnect;
    procedure DoDissConnect( bStart : boolean = false );
    procedure SendData( sData : string );

//    procedure PushQueue(Size: Integer; msg: string);
//    procedure PushQueue(Size: Integer; Packet: PChar);
//    function  PopQueue :  PReceiveData;
    function  GetSockType : string;
    function  GetSockState : string;

    property WebSocket : TScWebSocketClient read FWebSocket;
    property EndPoint  : string read FEndPoint;
    property Port      : integer read FPort;
    property SockDiv  : integer read FSockDiv;
    property Seq      : integer read FSeq;

    property LiveTime : TDateTime read FLiveTime;
    property ExchangeKind   : TExchangeKind read FExchangeKind;

    property ConnectTry : integer read FConnectTry write FConnectTry;
    property OnNotify   : TGetStrProc read FOnNotify write FOnNotify;
  end;
implementation
 uses
  ScCLRClasses
  ,Vcl.Forms
  ,GApp
  , UApiConsts
  ;


{ TWebsocket }
constructor TWebsocket.Create( iSockDiv, iSeq : integer; aExKind : TExchangeKind );
begin
  FWebSocket  :=   TScWebSocketClient.Create( nil );//TComponent( aOwner) );
  FConnectTry := 0;
  FSockDiv    := iSockDiv;

//  FEvent  := TEvent.Create( nil, False, False, '');
//  FQueue  := TList.Create;
  FSeq    := iSeq;

  FExchangeKind := aExKind;
//  FReceiveMutex := CreateMutex( nil, False, PChar( Format('Recv_%s_%d_%d'
//    , [ TExchangeKindDesc[FExchangeKind],iSockDiv, FSeq ]) ) );
//  inherited Create( true );
//  Priority  := tpHigher;
  FWebSocket.AfterConnect     := OnAfterconnect;
  FWebSocket.AfterDisconnect  := OnAfterDisconnect;
  FWebSocket.OnConnectFail    := OnConnectFail;
  FWebSocket.OnControlMessage := OnControlMessage;
  FWebSocket.OnMessage        := OnMessage;
  FWebSocket.BeforeConnect    := OnBeforeConnect;
//  FEvent.SetEvent;
//  Resume;

//  WebSocketClient.HeartBeatOptions.Enabled := True;
//  WebSocketClient.Options.Credentials.UserName := 'Peter';
//  WebSocketClient.Options.Credentials.Password := '12345';
//  WebSocketClient.Options.UserAgent := 'devart_chat_client';
//  WebSocketClient.Options.RequestHeaders['Content-Language'] := 'en-US';
end;
function TWebsocket.Desc: string;
begin
  Result := Format('%s_%d', [ TExchangeKindDesc[FExchangeKind], FSeq ]);
end;

destructor TWebsocket.Destroy;
begin
//  if FWebSocket.State = sOpen then
//    FWebSocket.Close;
  FwebSocket.Free;
//  FQueue.Free;
//  CloseHandle(FReceiveMutex);
//  FEvent.Free;
  inherited;
end;

procedure TWebsocket.DoConnect;
begin
  DoDissConnect(true);
  FWebSocket.Connect( 'wss://'+FEndPoint);

end;
procedure TWebsocket.DoDissConnect( bStart : boolean );
begin
  try

    if FWebSocket.State = sOpen then
      FWebSocket.Close;
  except on e : WebSocketException do
    App.Log(llError, '%s DisConnect Error : %s, %s, %d:%s',[Desc, e.Message,  e.ToString,
      integer(FWebSocket.State),  FWebSocket.CloseStatusDescription ] );
  end;
end;

function TWebsocket.GetSockState: string;
begin
  case FWebSocket.State of
    sConnecting: Result := 'Connecting';
    sOpen: Result := 'Open';
    sCloseSent: Result := 'CloseSent';
    sCloseReceived: Result := 'CloseReceived';
    sClosed:Result := 'Closed' ;
    sAborted: Result := 'Aborted';
  end;
end;
function TWebsocket.GetSockType: string;
begin
  if FSockDiv = 0 then
    Result := 'trade'
  else
    Result := 'quote';
end;
procedure TWebsocket.init( sAddr: string; iPort : integer);
begin

  FPort := iPort;
  FEndPoint := sAddr;
end;
procedure TWebsocket.OnAfterConnect(Sender: TObject);
begin
  FLiveTime := now;
  inc( FConnectTry );

end;
procedure TWebsocket.OnAfterDisconnect(Sender: TObject);
begin
  if TScWebSocketClient(Sender).CloseStatus <> csNormalClosure then
    App.Log(llError, '%s was closed with error %s ',
      [ TExchangeKindDesc[FExchangeKind], TScWebSocketClient(Sender).CloseStatusDescription ] );

end;
procedure TWebsocket.OnBeforeConnect(Sender: TObject);
begin

end;
procedure TWebsocket.OnConnectFail(Sender: TObject);
begin
   App.Log(llError, '%s OnConnectFail  %s', [ TExchangeKindDesc[FExchangeKind],
    FWebsocket.CloseStatusDescription ] );

end;
procedure TWebsocket.OnControlMessage(Sender: TObject;
  ControlMessageType: TScWebSocketControlMessageType);
  var
    sTmp : string;
begin
  case ControlMessageType of
    cmtPing: begin sTmp := 'ping'; FWebSocket.Pong;   end;
    cmtPong: sTmp := 'pong';
  end;

  App.Log(llInfo, '%s %d %s', [ TExchangeKindDesc[FExchangeKind], FSeq, sTmp ] );
end;
procedure TWebsocket.OnMessage(Sender: TObject; const Data: TArray<System.Byte>;
  MessageType: TScWebSocketMessageType; EndOfMessage: Boolean);
var
  sData: string;
begin


  if MessageType = mtText then begin
    sData := Encoding.Default.GetString(Data);
  end else
  if MessageType = mtClose then begin
    sData := ''
  end else
  if MessageType = mtBinary then
  begin
    sData := Encoding.ASCII.GetString(Data);
  end else
    sData := '' ;

  if ( sData <> '') and ( Assigned(FOnNotify)) then begin
    FLiveTime := now;
    FOnNotify( sData );
  end;

//    PushQueue( Length(sData), sData );
//    PushQueue( Length(sData), PChar(sData) );
end;


procedure TWebsocket.SendData(sData: string);
begin
  try
    if sData <> '' then begin
      FWebSocket.Send(sData);
      App.Log(llInfo, '%s Send Data : %s', [ TExchangeKindDesc[FExchangeKind], sData ] );
    end;
  except
    on e : WebSocketException do
    begin
      App.Log(llError, 'Failed Send Data : %s, %s', [ e.Message, sData ] );
    end;
  end;
end;

end.

