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

    function GetDescript: string;
    procedure OnAfterConnect(Sender: TObject); override;
    procedure OnAfterDisconnect(Sender: TObject);  override;

    procedure SubScribe( aSymbol : TSymbol; bSub : boolean ) ; overload;

    procedure OnMessage( const S : string );

  public

    Constructor Create( iSockDiv, iSeq : Integer; aMtType : TMarketType ); overload;
    destructor Destroy; override;

      // 한종목씩
    procedure SubScribe( aSymbol : TSymbol ) ; overload;
    procedure UnSubScribe( aSymbol : TSymbol ) ;

    procedure SubscribeAll; override;
    procedure MakeSubData; override;

    property MarketType  : TMarketType read FMarketType;
    property SubList  : TStrings  read FSubList;
    property Descript : string    read GetDescript;

  end;
implementation
uses
  GApp , GLibs
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

procedure TUpbitWebSocket.MakeSubData;
var
  sParam, sParam2, sData : string;
  i : integer;
begin
  if SubList.Count > 0 then Exit;

  sParam := '';   sParam2 := '';
  with App.Engine.SymbolCore do
    for I := 0 to   Spots[ ekUpbit].Count-1 do
    begin
      sParam := sParam + Format('"%s"', [Symbols[ ekUpbit].Symbols[i].OrgCode  ]);
      sParam2:= sParam2 + Format('"%s.5"', [Symbols[ ekUpbit].Symbols[i].OrgCode ]);
      if i < Symbols[ ekUpbit].Count-1  then begin
        sParam  := sParam + ',' ;
        sParam2 := sParam2 + ',';
      end;
    end;

  sData := Format('[{"ticket":"real"},{"type":"trade","codes":[%s]} '
    + ',{"type":"ticker","codes":[%s]}'
    + ',{"type":"orderbook","codes":[%s]},{"format":"SIMPLE"}]'
    , [ sParam, sParam, sParam2] );

  SubList.Add( sData );
  App.DebugLog( '========== upbit sub ===========');

  for I := 0 to SubList.Count-1 do
    App.DebugLog('(%d):%s',[ i, SubList[i] ]);
end;

procedure TUpbitWebSocket.OnAfterConnect(Sender: TObject);
begin
  App.Log(llInfo, ' %s Connected', [ Descript]);

end;

procedure TUpbitWebSocket.OnAfterDisconnect(Sender: TObject);
begin
  App.Log(llInfo, ' %s Disconnected', [ Descript]);
end;

procedure TUpbitWebSocket.OnMessage(const S: string);
begin
  gUpReceiver.ParseSocketData(FMarketType, S);
end;

procedure TUpbitWebSocket.SubScribe(aSymbol: TSymbol; bSub: boolean);

begin

end;

procedure TUpbitWebSocket.SubScribe(aSymbol: TSymbol);
var
  sParam, sParam2, sData : string;
  i : integer;
begin

  if FSubList.IndexOf(aSymbol.OrgCode) < 0 then
    FSubList.Add(aSymbol.OrgCode);

  sParam := '';   sParam2 := '';
  for I := 0 to FSubList.Count-1 do
  begin
    sParam := sParam + Format('"%s"', [FSubList[i]]);
    sParam2:= sParam2 + Format('"%s.5"', [FSubList[i]]);
    if i < FSubList.Count-1  then begin
      sParam  := sParam + ',' ;
      sParam2 := sParam2 + ',';
    end;
  end;

  sData := Format('[{"ticket":"real"},{"type":"trade","codes":[%s]} '
    + ',{"type":"ticker","codes":[%s]}'
    + ',{"type":"orderbook","codes":[%s]},{"format":"SIMPLE"}]'
    , [ sParam, sParam, sParam2] );

  SendData( sData );
//  ws[0].SendData('[{"ticket":"test"},{"type":"orderbook","codes":['+sTmp+']},{"format":"SIMPLE"}]');
end;

procedure TUpbitWebSocket.SubscribeAll;
var
  i : integer;
begin
  for I := 0 to SubList.Count-1 do begin
    SendData( SubList[i] );
    sleep(10);
  end;
end;

procedure TUpbitWebSocket.UnSubScribe(aSymbol: TSymbol);
begin

end;



end.


