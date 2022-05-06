unit UUpbitWebSockets;
interface
uses
  System.Classes, System.SysUtils, System.DateUtils
  , UWebSockets  , USymbols
  , UApiTypes
  ;
type
  TUpbitWebSocket = class( TWebSocket )
  private
    FMarketType: TMarketType;
    FSubList: TStrings;
    FParam, FParam2 : string;
    function GetDescript: string;
    procedure OnAfterConnect(Sender: TObject); override;
//    procedure OnAfterDisconnect(Sender: TObject);  override;

    procedure OnMessage( const S : string );
    function GetSubData: string;

  public


    Constructor Create( iSockDiv, iSeq : Integer; aMtType : TMarketType ); overload;
    destructor Destroy; override;

      // 한종목씩
    procedure SubScribe( aSymbol : TSymbol ) ;
    procedure UnSubScribe( aSymbol : TSymbol ) ;

    procedure SubscribeAll; override;
    procedure MakeSubData; override;

    property MarketType  : TMarketType read FMarketType;
    property SubList  : TStrings  read FSubList;
    property Descript : string    read GetDescript;

  end;
implementation
uses
  GApp , GLibs , UEncrypts , UTypes
  , UApiConsts
  , UUpbitParse
  ;
{ TUpbitWebSocket }

constructor TUpbitWebSocket.Create(iSockDiv, iSeq: Integer; aMtType: TMarketType);
begin
  inherited Create( iSockDiv, iSeq, ekUpbit );

  FMarketType := aMtType;
  FSubList    := TStringList.Create;
  OnNotify    := OnMessage;

  with WebSocket do
  begin
    HeartBeatOptions.Enabled := true;
  end;
end;

destructor TUpbitWebSocket.Destroy;
begin
  FSubList.Free;
  inherited;
end;

function TUpbitWebSocket.GetDescript: string;
begin
  Result := Format('%s-%s-%d', [ 'UP', TMarketTypeDesc[FMarketType], Seq ]);
end;

function TUpbitWebSocket.GetSubData: string;
begin
  Result := Format('[{"ticket":"%s"},{"type":"trade","codes":[%s],"isOnlyRealtime":true } '
   // + ',{"type":"ticker","codes":[%s],"isOnlyRealtime":true}'
    + ',{"type":"orderbook","codes":[%s],"isOnlyRealtime":true}'
    + ',{"format":"SIMPLE"}]'
    , [ GetUUID, FParam, FParam, FParam2] );
end;

procedure TUpbitWebSocket.MakeSubData;
var
  sData : string;
  i : integer;
begin
  if SubList.Count > 0 then Exit;

  FParam := '';   FParam2 := '';
  with App.Engine.SymbolCore.Symbols[ ekUpbit] do
    for I := iStart to  iEnd-1 do
    begin
      FParam := FParam + Format('"%s"', [Symbols[i].OrgCode  ]);
      FParam2:= FParam2 + Format('"%s.5"', [Symbols[i].OrgCode ]);
      if i < iEnd-1  then begin
        FParam  := FParam + ',' ;
        FParam2 := FParam2 + ',';
      end;
    end;


  SubList.Add( GetSubData );
  App.DebugLog( '========== upbit sub ===========');

  for I := 0 to SubList.Count-1 do
    App.DebugLog('(%d):%s',[ i, SubList[i] ]);
end;

procedure TUpbitWebSocket.OnAfterConnect(Sender: TObject);
begin
  inherited OnAfterConnect(Sender);
  App.Log(llInfo, ' %s  %d.th Connected', [ Descript, ConnectTry ]);

  if ( ConnectTry > 1 ) and  ( ConnectTry < 10 ) and ( GetSockState = 'Open' ) then
    SubscribeAll;
end;

//procedure TUpbitWebSocket.OnAfterDisconnect(Sender: TObject);
//begin
//  inherited OnAfterDisconnect(Sender);
//  App.Log(llInfo, ' %s Disconnected', [ Descript]);
//end;

procedure TUpbitWebSocket.OnMessage(const S: string);
begin
  gUpReceiver.ParseSocketData(FMarketType, S);
end;

procedure TUpbitWebSocket.SubScribe(aSymbol: TSymbol);
var
  sData : string;
  i : integer;
begin

  if FSubList.IndexOf(aSymbol.OrgCode) < 0 then
    FSubList.Add(aSymbol.OrgCode);

  FParam := '';   FParam2 := '';
  for I := 0 to FSubList.Count-1 do
  begin
    FParam := FParam + Format('"%s"', [FSubList[i]]);
    FParam2:= FParam2 + Format('"%s.5"', [FSubList[i]]);
    if i < FSubList.Count-1  then begin
      FParam  := FParam + ',' ;
      FParam2 := FParam2 + ',';
    end;
  end;

  if App.AppStatus <> asShow then Exit;

  SendData( GetSubData );
end;

procedure TUpbitWebSocket.SubscribeAll;
begin
  SendData( GetSubData );
end;

procedure TUpbitWebSocket.UnSubScribe(aSymbol: TSymbol);
var
  idx : integer;
begin
  idx := FSubList.IndexOf(aSymbol.OrgCode);
  if idx >= 0 then
    FSubList.Delete(idx);
end;



end.


