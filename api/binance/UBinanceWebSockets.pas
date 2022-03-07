unit UBinanceWebSockets;
interface
uses
  System.Classes, System.SysUtils, System.DateUtils
  , UWebSockets, USymbols
  , UApiTypes
  ;
type
  TBinanceWebSocket = class( TWebSocket )
  private
    FMarketType: TMarketType;
    FSubList: TStrings;
    FSubIndex: integer;
    procedure OnAfterConnect(Sender: TObject); override;
    procedure OnAfterDisconnect(Sender: TObject);  override;

    function GetDescript: string;
    procedure SubScribe( aSymbol : TSymbol; bSub : boolean ) ; overload;

    procedure OnMessage( const S : string );
  public
    Constructor Create( iSockDiv, iSeq : Integer; aMtType : TMarketType ); overload;
    destructor Destroy; override;
    procedure CheckPingPong;
    procedure SubScribe( aSymbol : TSymbol ) ; overload;
    procedure UnSubScribe( aSymbol : TSymbol ) ;
    procedure SubscribeAll; override;
    property MarketType  : TMarketType read FMarketType;
    property SubList  : TStrings  read FSubList;
    property SubIndex : integer   read FSubIndex;
    property Descript : string    read GetDescript;
  end;
implementation
uses
  GApp , GLibs   , UTypes
  , UApiConsts
  , UBinanceParse
  ;
{ TBinanceWebSocket }
procedure TBinanceWebSocket.CheckPingPong;
var
  iGap : integer;
begin
//  iGap := SecondsBetween( now, LiveTime );
end;
constructor TBinanceWebSocket.Create(iSockDiv, iSeq: Integer; aMtType: TMarketType);
begin
  inherited Create( iSockDiv, iSeq, ekBinance );
  FMarketType := aMtType;
  FSubList    := TStringList.Create;
  FSubIndex   := 0;
  OnNotify    := OnMessage;
end;
destructor TBinanceWebSocket.Destroy;
begin
  FSubList.Free;
  inherited;
end;

function TBinanceWebSocket.GetDescript: string;
begin
  Result := Format('%s-%s-%d', [ 'BN', TMarketTypeDesc[FMarketType], Seq ]);
end;
{
//  Spot
//  @bookTicker  : best bid and ask , realtime
//  @trade       : tick , real time
//  @miniTicker  : 24hour rolling OHLC, 1sec
//  Future
//  @depth5      : Top bids and asks, Valid are 5,  250ms
//  @aggTrade    : Aggregate Trade , 100ms
//  @miniTicker  : 24hour rolling OHLC, 500ms
}


procedure TBinanceWebSocket.SubscribeAll;
var
  aList : TStrings;
  I: Integer;
  sParam, sData : string;
begin
  aList := TStringList.Create;
  try

    if FSubList.Count <= 0 then Exit;

    for I := 0 to FSubList.Count-1 do
    begin
      if FMarketType = mtSpot then
      begin
          aList.Add( FSubList[i] +'@bookTicker' );
          aList.Add( FSubList[i] +'@trade' );
          aList.Add( FSubList[i] +'@miniTicker' );
      end else
      if FMarketType = mtFutures then
      begin
          aList.Add( FSubList[i] +'@depth5' );
          aList.Add( FSubList[i] +'@aggTrade' );
          aList.Add( FSubList[i] +'@miniTicker' );
      end ;
    end;

    sParam := '';
    for I := 0 to aList.Count-1 do
    begin
      sParam := sParam + Format('"%s"', [aList[i]]);
      if i < aList.Count-1  then
        sParam := sParam + ','
    end;

    inc(FSubIndex);
    sData := Format('{"method": "SUBSCRIBE","params":[%s],"id": %d}', [ sParam, FSubIndex ] );
    SendData(sData);

  finally
    aList.Free;
  end;
end;

procedure TBinanceWebSocket.SubScribe( aSymbol : TSymbol; bSub : boolean );
var
  I: Integer;
  sData, sParam, sTmp : string;
  aList : TStrings;
begin
  aList := TStringList.Create;
  try
    if FMarketType = mtSpot then
    begin
        aList.Add( aSymbol.OrgCode +'@bookTicker' );
        aList.Add( aSymbol.OrgCode +'@trade' );
        aList.Add( aSymbol.OrgCode +'@miniTicker' );
    end else
    if FMarketType = mtFutures then
    begin
        aList.Add( aSymbol.OrgCode +'@depth5' );
        aList.Add( aSymbol.OrgCode +'@aggTrade' );
        aList.Add( aSymbol.OrgCode +'@miniTicker' );
    end else
      Exit;

    sParam := '';
    for I := 0 to aList.Count-1 do
    begin
      sParam := sParam + Format('"%s"', [aList[i]]);
      if i < aList.Count-1  then
        sParam := sParam + ','
    end;

    sTmp := ifThenStr( bSub,'SUBSCRIBE', 'UNSUBSCRIBE');
    inc(FSubIndex);
    sData := Format('{"method": "%s","params":[%s],"id": %d}', [ sTmp, sParam, FSubIndex ] );
    SendData(sData);

  finally
    aList.Free;
  end;
end;

procedure TBinanceWebSocket.SubScribe(aSymbol: TSymbol);
var
  i : integer;
  aList : TStrings;
  sParam, sData : string;
begin
  if FSubList.IndexOf(aSymbol.OrgCode) < 0 then
    FSubList.Add(aSymbol.OrgCode);

  if App.AppStatus <> asShow then Exit;

  SubScribe( aSymbol, true );
end;

procedure TBinanceWebSocket.UnSubScribe(aSymbol: TSymbol);
var
  i : integer;
  aList : TStrings;
  sParam, sData : string;
begin
  i := FSubList.IndexOf(aSymbol.OrgCode) ;
  if i >= 0 then FSubList.Delete(i);

  SubScribe( aSymbol, false );
end;


procedure TBinanceWebSocket.OnAfterConnect(Sender: TObject);
begin
//  if (FSubList.Count > 0 ) then
//    SubScribe( true );
  App.Log(llInfo, ' %s Connected', [ Descript]);
end;
procedure TBinanceWebSocket.OnAfterDisconnect(Sender: TObject);
begin
  //inherited;
  App.Log(llInfo, ' %s Disconnected %d %s', [ Descript , integer(WebSocket.State),
    WebSocket.CloseStatusDescription ]);

end;

procedure TBinanceWebSocket.OnMessage(const S: string);
begin
  gBinReceiver.ParseSocketData( FMarketType, S);
end;




end.
