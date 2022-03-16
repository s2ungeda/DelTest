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
    FSubList: TStrings;
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
    procedure SubScribe( bSub : boolean ) ; overload;
    procedure SubScribe( aSymbol : TSymbol ) ; overload;

    procedure SubscribeAll; override;

    property MarketType  : TMarketType read FMarketType;
    property SubList  : TStrings  read FSubList;
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
  FSubList    := TStringList.Create;
  FSubIndex   := 0;
  FSendQueue  := TStringList.Create;
  OnNotify    := OnMessage;
end;
destructor TBithWebSocket.Destroy;
begin
  FSubList.Free;
  FSendQueue.Free;
  inherited;
end;
function TBithWebSocket.GetDescript: string;
begin
  Result := Format('%s-%s-%d', [ 'BT', TMarketTypeDesc[FMarketType], Seq ]);
end;
procedure TBithWebSocket.OnAfterConnect(Sender: TObject);
begin
  App.Log(llInfo, ' %s Connected', [ Descript]);
//  if (FSubList.Count > 0 ) then
//    SubScribe( true );
end;
procedure TBithWebSocket.OnAfterDisconnect(Sender: TObject);
begin
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
  FSubList.Add( sData );
  FSubList.Add( Format('{"type":"transaction", "symbols":[%s]}', [ sParam] ));
  FSubList.Add( Format('{"type":"ticker", "symbols":[%s],"tickTypes":["24H"] }', [ sParam] ));
end;

procedure TBithWebSocket.SubScribe(aSymbol: TSymbol);
var
  i : integer;
  sParam : string;
begin

  if FSubList.IndexOf(aSymbol.OrgCode) < 0 then
    FSubList.Add(aSymbol.OrgCode);

  if App.AppStatus <> asShow then Exit;

  SubscribeAll;

end;

procedure TBithWebSocket.SubscribeAll;
var
  sParam : string;
  i : Integer;
begin

  if FSubList.Count < 0 then Exit;

  sParam := '';
  for I := 0 to FSubList.Count-1 do
  begin
    sParam := sParam + Format('"%s"', [FSubList[i]]);
    if i < FSubList.Count-1  then
      sParam := sParam + ','
  end;

//    SendData( Format('{"type":"orderbookdepth", "symbols":[%s]}', [ sParam ]) );
  SendData( Format('{"type":"transaction", "symbols":[%s]}', [ sParam ] ));
  SendData( Format('{"type":"ticker", "symbols":[%s],"tickTypes":["24H"] }', [ sParam ] ));
end;

procedure TBithWebSocket.SubScribe(bSub: boolean);
var
  I: Integer;
  sData, sParam, sTmp : string;
begin
//  for I := 0 to FSubList.Count-1 do
//  begin
//    SendData(FSubList[i]);
////    App.DebugLog(' %d : %s , %s', [i, Descript, FSubList[i]] ) ;
//  end;
end;

end.