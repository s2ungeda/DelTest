unit UBithWebSockets;
interface
uses
  System.Classes, System.SysUtils, System.DateUtils
  , UWebSockets  , USymbols
  , UApiTypes
  ;
type
  TBithWebSocket = class( TWebSocket )
  private
    FMarketType: TMarketType;
    FSubIndex: integer;

    FSendQueue: TStrings;
    function GetDescript: string;
    procedure OnAfterConnect(Sender: TObject); override;
    procedure OnAfterDisconnect(Sender: TObject);  override;

    procedure OnMessage( const S : string );

  public
    Constructor Create( iSockDiv, iSeq : Integer; aMtType : TMarketType ); overload;
    destructor Destroy; override;
    procedure Send;
    procedure SetSubList( aList : TStringList ) ;

    procedure SubScribe( aSymbol : TSymbol ) ;
    procedure UnSubScribe( aSymbol : TSymbol ) ;

    procedure SubscribeAll; override;
    procedure MakeSubData; override;

    property MarketType  : TMarketType read FMarketType;

    property SubIndex : integer   read FSubIndex;
    property Descript : string    read GetDescript;
    property SendQueue : TStrings read FSendQueue;
  end;
implementation
uses
  GApp , GLibs
  , UTypes
  , UApiConsts
  , UBithParse
  ;
{ TBithWebSocket }
constructor TBithWebSocket.Create(iSockDiv, iSeq: Integer; aMtType: TMarketType);
begin
  inherited Create( iSockDiv ,iSeq, ekBithumb);
  FMarketType := aMtType;

  FSubIndex   := 0;
  FSendQueue  := TStringList.Create;
  OnNotify    := OnMessage;

  with WebSocket do
  begin
    HeartBeatOptions.Enabled := true;
    HeartBeatOptions.Interval:= 100;

  end;
end;
destructor TBithWebSocket.Destroy;
begin

  FSendQueue.Free;
  inherited;
end;
function TBithWebSocket.GetDescript: string;
begin
  Result := Format('%s-%s-%d', [ 'BT', TMarketTypeDesc[FMarketType], Seq ]);
end;

procedure TBithWebSocket.MakeSubData;
var
  sParam : string;
  i : Integer;
begin
  if SubList.Count > 0 then Exit;

  sParam := '';
  with App.Engine.SymbolCore.Symbols[ekBithumb] do
    for I := 0 to Count-1 do
    begin
      sParam := sParam + Format('"%s"', [ Symbols[i].OrgCode ]);
      if i < Count-1  then
        sParam := sParam + ','
    end;

//    SendData( Format('{"type":"orderbookdepth", "symbols":[%s]}', [ sParam ]) );
  SubList.Add( Format('{"type":"transaction", "symbols":[%s]}', [ sParam ] ));
  SubList.Add( Format('{"type":"ticker", "symbols":[%s],"tickTypes":["24H"] }', [ sParam ] ));

  App.DebugLog( '========== bithumb sub ===========');
  for I := 0 to SubList.Count-1 do
    App.DebugLog('(%d):%s',[ i, SubList[i] ]);

end;

procedure TBithWebSocket.OnAfterConnect(Sender: TObject);
begin

  inherited OnAfterConnect(Sender);
  App.Log(llInfo, ' %s  %d.th Connected', [ Descript, ConnectTry ]);

  if ( ConnectTry > 1 ) and ( GetSockState = 'Open' ) then
    SubscribeAll;
//  if (FSubList.Count > 0 ) then
//    SubScribe( true );

end;
procedure TBithWebSocket.OnAfterDisconnect(Sender: TObject);
begin
  inherited OnAfterDisconnect(Sender);
  App.Log(llInfo, ' %s Disconnected', [ Descript]);
end;

procedure TBithWebSocket.OnMessage(const S: string);
begin
  gBithReceiver.ParseSocketData( FMarketType, S);
end;

procedure TBithWebSocket.Send;
var
  I: Integer;
  sData : string;
begin
  Exit;
  for I := 0 to FSendQueue.Count-1 do
  begin
    sData := FsendQueue[0];
    SendData(sData);
    FSendQueue.Delete(0);
    break;
  end;
end;

procedure TBithWebSocket.SetSubList(aList: TStringList);
var
  i : integer;
  sData, sParam : string;
begin
  if aList.Count <= 0 then Exit ;
  sParam := '';
  for I := 0 to aList.Count-1 do
  begin
    sParam := sParam + Format('"%s"', [aList[i]]);
    if i < aList.Count-1  then
      sParam := sParam + ','
  end;
  sData := Format('{"type":"orderbookdepth", "symbols":[%s]}', [ sParam]);
  SubList.Add( sData );
  SubList.Add( Format('{"type":"transaction", "symbols":[%s]}', [ sParam] ));
  SubList.Add( Format('{"type":"ticker", "symbols":[%s],"tickTypes":["24H"] }', [ sParam] ));
end;

procedure TBithWebSocket.SubScribe(aSymbol: TSymbol);
var
  i : integer;
begin

  if SubList.IndexOf(aSymbol.OrgCode) < 0 then
    SubList.Add(aSymbol.OrgCode);

  if App.AppStatus <> asShow then Exit;

  SubscribeAll;
end;

procedure TBithWebSocket.UnSubScribe(aSymbol: TSymbol);
var
  idx : integer;
begin
  idx := SubList.IndexOf(aSymbol.OrgCode);
  if idx >= 0 then
    SubList.Delete(idx);
end;

procedure TBithWebSocket.SubscribeAll;
var
  sParam : string;
  i : Integer;
begin

  if SubList.Count < 0 then Exit;

  sParam := '';
  for I := 0 to SubList.Count-1 do
  begin
    sParam := sParam + Format('"%s"', [SubList[i]]);
    if i < SubList.Count-1  then
      sParam := sParam + ','
  end;

//    SendData( Format('{"type":"orderbookdepth", "symbols":[%s]}', [ sParam ]) );
  SendData( Format('{"type":"transaction", "symbols":[%s]}', [ sParam ] ));
  SendData( Format('{"type":"ticker", "symbols":[%s],"tickTypes":["24H"] }', [ sParam ] ));
end;

//procedure TBithWebSocket.SubscribeAll;
//var
//  i : integer;
//begin
//  for I := 0 to SubList.Count-1 do begin
//    SendData( SubList[i] );
//    sleep(10);
//  end;
//end;



end.
