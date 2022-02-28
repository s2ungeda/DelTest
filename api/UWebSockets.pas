unit UWebSockets;

interface

uses

  System.Classes, System.SysUtils, System.DateUtils , Windows
  , SyncObjs
  , ScWebSocketClient
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

  TWebsocket = class( TThread )
  private
    FEvent: TEvent;

    FData : PReceiveData;

    FWebSocket: TScWebSocketClient;
    FEndPoint: string;
    FLiveTime: TDateTime;
    FSockDiv: integer;
    FConnectTry: integer;
    FSeq: integer;
    FOnNotify: TGetStrProc;
    FPort: integer;

  protected

    procedure Execute ; override;
    procedure SyncProc; virtual ;

    procedure OnAfterConnect(Sender: TObject);  virtual;
    procedure OnAfterDisconnect(Sender: TObject);  virtual;
    procedure OnConnectFail(Sender: TObject); virtual;
    procedure OnControlMessage(Sender: TObject;
      ControlMessageType: TScWebSocketControlMessageType); virtual;
    procedure OnMessage(Sender: TObject; const Data: TArray<System.Byte>;
      MessageType: TScWebSocketMessageType; EndOfMessage: Boolean); virtual;
    procedure OnBeforeConnect(Sender: TObject);

  public

    FReceiveMutex : HWND;
    FSocketMute   : HWND;
    FQueue        : TList;

    constructor Create( iSockDiv : integer );
    destructor Destroy; override;

    procedure init(iSeq: integer;sAddr : string; iPort : integer = 443);

    procedure DoConnect;
    procedure DoDissConnect;
    procedure SendData( sData : string );

    procedure PushQueue(Size: Integer; msg: string);
//    procedure PushQueue(Size: Integer; Packet: PChar);
    function  PopQueue :  PReceiveData;

    function  GetSockType : string;
    function  GetSockState : string;

    property WebSocket : TScWebSocketClient read FWebSocket;
    property EndPoint  : string read FEndPoint;
    property Port      : integer read FPort;
    property SockDiv  : integer read FSockDiv;
    property Seq      : integer read FSeq;

    property Data : PReceiveData read FData;

    property LiveTime : TDateTime read FLiveTime;
    property ConnectTry : integer read FConnectTry write FConnectTry;
    property OnNotify   : TGetStrProc read FOnNotify write FOnNotify;
  end;

implementation

 uses
  ScCLRClasses
  ,Vcl.Forms
  ,GApp
  ;




{ TWebsocket }

constructor TWebsocket.Create( iSockDiv : integer );
begin
  FWebSocket  :=   TScWebSocketClient.Create( nil );//TComponent( aOwner) );

  FConnectTry := 0;
  FSockDiv    := iSockDiv;
  FSeq        := -1;

  FEvent  := TEvent.Create( nil, False, False, '');
  FQueue  := TList.Create;
  FReceiveMutex := CreateMutex( nil, False, PChar( Format('Recv_%d', [ iSockDiv]) ) );

  inherited Create( true );
  Priority  := tpHigher;

  FWebSocket.AfterConnect     := OnAfterconnect;
  FWebSocket.AfterDisconnect  := OnAfterDisconnect;
  FWebSocket.OnConnectFail    := OnConnectFail;
  FWebSocket.OnControlMessage := OnControlMessage;
  FWebSocket.OnMessage        := OnMessage;
  FWebSocket.BeforeConnect    := OnBeforeConnect;

  FEvent.SetEvent;
  Resume;
end;

destructor TWebsocket.Destroy;
begin
  if FWebSocket.State = sOpen then
    FWebSocket.Close;
  FwebSocket.Free;

  FQueue.Free;
  CloseHandle(FReceiveMutex);
  FEvent.Free;

//  inherited;
end;



procedure TWebsocket.DoConnect;
begin
  DoDissConnect;
  FWebSocket.Connect( 'wss://'+FEndPoint);
end;

procedure TWebsocket.DoDissConnect;
begin
  try
    if FWebSocket.State = sOpen then
      FWebSocket.Close;
  except
//    OnNotify( IntTostr( integer(FWebSocket.State)));
  end;
end;

procedure TWebsocket.Execute;
var
  vSend: Boolean;
  iSleep , iCount: integer;
begin
  while not Terminated do begin

    if not(FEvent.WaitFor(INFINITE) in [wrSignaled]) then Continue;

    while FQueue.Count > 0 do begin
      FData := PopQueue;
      if FData <> nil then begin
        Synchronize(SyncProc);
        Dispose(FData);
      end;
      Application.ProcessMessages;
    end;
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

procedure TWebsocket.init(iSeq: integer; sAddr: string; iPort : integer);
begin
  FSeq  := iSeq;
  FPort := iPort;
  FEndPoint := sAddr;
end;

procedure TWebsocket.OnAfterConnect(Sender: TObject);
begin
  FLiveTime := now;
  inc( FConnectTry );
  //OnNotify( Format(' ------ %s, %d Connected --------', [ GetsockType, FSeq]) );
end;

procedure TWebsocket.OnAfterDisconnect(Sender: TObject);
begin
  //OnNotify( Format(' ------ %s, %d DissConnected --------', [ GetSockType, FSeq]) );
end;

procedure TWebsocket.OnBeforeConnect(Sender: TObject);
begin
//  OnNotify(' ------ OnBeforeConnect --------' );
end;

procedure TWebsocket.OnConnectFail(Sender: TObject);
begin
//  OnNotify(' ------ OnConnectFail --------' );
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
//  OnNotify( Format(' OnControlMessage : %s ', [ sTmp ] ) );
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

  FLiveTime := now;

  if sData <> '' then
    PushQueue( Length(sData), sData );
//    PushQueue( Length(sData), PChar(sData) );
end;

function TWebsocket.PopQueue: PReceiveData;
begin

  if FQueue.Count < 1 then exit (nil);

  WaitForSingleObject(FReceiveMutex, INFINITE);
  Result := PReceiveData(FQueue.Items[0]);
  FQueue.Delete(0);
  ReleaseMutex(FReceiveMutex );

end;

procedure TWebsocket.PushQueue(Size: Integer; msg: string);
//procedure TWebsocket.PushQueue(Size: Integer; Packet: PChar);
var
  vData: PReceiveData;
  ivar: Integer;
  tStr: String;
  lwResult : LongWord;
begin
  New(vData);
  vData.Size  := Size;
//  SetLength(vData.Packet, Size);
//  Move(Packet[0], (vData.Packet)[0], Size);
  vData.Packet:= msg;

  lwResult := WaitForSingleObject(FReceiveMutex, INFINITE);
  FQueue.Add(vData);
  ReleaseMutex(FReceiveMutex);

  FEvent.SetEvent;

end;

procedure TWebsocket.SendData(sData: string);
begin
  try
    if sData <> '' then begin
      FWebSocket.Send(sData);
      App.Log(llInfo, 'Send Data : %s', [ sData ] );
    end;
  except
    on e : exception do
    begin
      App.Log(llError, 'Failed Send Data : %s, %s', [ e.Message, sData ] );
    end;
  end;
end;

procedure TWebsocket.SyncProc;
begin
//  OnNotify( Format('recv[%s]', [  FData.Packet ]) );
end;

end.
