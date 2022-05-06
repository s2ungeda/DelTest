unit UBinanceWebSockets;
interface
uses
  System.Classes, System.SysUtils, System.DateUtils
  , UWebSockets, USymbols
  , UApiTypes
  , UQuoteTimers
  ;
type
  TBinanceWebSocket = class( TWebSocket )
  private
    FMarketType: TMarketType;
    FUnSubList: TStrings;
    FSubIndex: integer;
    FTimer   : TQuoteTimer;
    FSendList: TStrings;
    procedure OnAfterConnect(Sender: TObject); override;
//    procedure OnAfterDisconnect(Sender: TObject);  override;

    function GetDescript: string;
    procedure SubScribe( aSymbol : TSymbol; bSub : boolean ) ; overload;

    procedure OnMessage( const S : string );
    procedure MakeSubList( var aList : TStrings ); overload;
    function MakeSubString: string;

    procedure OnSubTimer( Sender : TObject );
    procedure AddSendData( aData : string );
    procedure DelSendData( aData : string );
  public
    Constructor Create( iSockDiv, iSeq : Integer; aMtType : TMarketType ); overload;
    destructor Destroy; override;
    procedure CheckPingPong;
    procedure SubScribe( aSymbol : TSymbol ) ; overload;
    procedure UnSubScribe( aSymbol : TSymbol ) ;
    procedure SubscribeAll; override;
    procedure UnSubScribeAll;
    procedure MakeSubData; override;

    procedure AllMarketTicker( bSub : boolean );

    property MarketType  : TMarketType read FMarketType;
    property UnSubList  : TStrings  read FUnSubList;
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

procedure TBinanceWebSocket.AllMarketTicker( bSub : boolean );
var
  sTmp, sData : string;
begin
  sTmp := ifThenStr( bSub,'SUBSCRIBE', 'UNSUBSCRIBE');
  inc(FSubIndex);
  sData := Format('{"method": "%s","params":["%s"],"id": %d}', [ sTmp, '!miniTicker@arr', FSubIndex ] );
  SendData(sData);
end;

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
  FUnSubList    := TStringList.Create;
  FSubIndex   := 0;
  OnNotify    := OnMessage;

  FSendList:= TStringList.Create;

  FTimer  := App.Engine.QuoteBroker.Timers.New;
  FTimer.Interval := 1000;
  FTimer.OnTimer  := OnSubTimer;
end;

procedure TBinanceWebSocket.AddSendData(aData: string);
begin
  FSendList.Add( aData );
end;


procedure TBinanceWebSocket.DelSendData(aData: string);
begin

end;

destructor TBinanceWebSocket.Destroy;
begin
  FSendList.Free;
  FUnSubList.Free;
  inherited;
end;

function TBinanceWebSocket.GetDescript: string;
begin
  Result := Format('%s-%s-%d', [ 'BN', TMarketTypeDesc[FMarketType], Seq ]);
end;

procedure TBinanceWebSocket.MakeSubList( var aList : TStrings );
var
  sParam, sData : string;
  k : Integer;
begin
  sParam := '';

  for k := 0 to aList.Count-1 do
  begin
    sParam := sParam + Format('"%s"', [aList[k]]);
    if k < aList.Count-1  then
      sParam := sParam + ','
  end;

  aList.Clear;
  inc(FSubIndex);
  sData := Format('{"method": "SUBSCRIBE","params":[%s],"id": %d}', [ sParam, FSubIndex ] );
  SubList.Add(sData);
end;

procedure TBinanceWebSocket.MakeSubData;
var
  sParam, sData : string;
  iCnt, iMod, i, j, k : Integer;
  aList : TStrings;
begin
  if SubList.Count > 0 then Exit;

  iCnt := App.Engine.SymbolCore.Symbols[ekBinance].Count div 20;
  iMod := App.Engine.SymbolCore.Symbols[ekBinance].Count mod 20;

  if iMod > 0 then inc(iCnt);

  j := 0;
  aList := TStringList.Create;
  try

    with App.Engine.SymbolCore.Symbols[ekBinance] do
      for I := 0 to Count-1 do
      begin

        if FMarketType <> Symbols[i].Spec.Market then Continue;

        if Symbols[i].Spec.Market = mtSpot then
        begin
            aList.Add( Symbols[i].OrgCode +'@bookTicker' );
            aList.Add( Symbols[i].OrgCode +'@trade' );
            aList.Add( Symbols[i].OrgCode +'@miniTicker' );

        end else
        if Symbols[i].Spec.Market = mtFutures then
        begin
            continue;
            aList.Add( Symbols[i].OrgCode +'@depth5' );
            aList.Add( Symbols[i].OrgCode +'@aggTrade' );
            aList.Add( Symbols[i].OrgCode +'@miniTicker' );
        end ;

        inc(j);
        if ((j mod 30) = 0) and ( j>0) then
        begin
          MakeSubList( aList );
          j := 0;
        end;
      end;

    MakeSubList( aList );

    App.DebugLog( '========== Biance %s sub ===========', [GetDescript] );
    for I := 0 to SubList.Count-1 do
      App.DebugLog('(%d):%s',[ i, SubList[i] ]);


  finally
    aList.Free;
  end;



end      ;


//procedure TBinanceWebSocket.SubscribeAll;
//var
//  i : integer;
//begin
//  for I := 0 to SubList.Count-1 do begin
//    SendData( SubList[i] );
//    sleep(500);
//  end;
//
//end;




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
//        aList.Add( aSymbol.OrgCode +'@miniTicker' );
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

    if FTimer.Enabled then
      AddSendData( sData )
    else
      SendData(sData);

  finally
    aList.Free;
  end;
end;

procedure TBinanceWebSocket.SubScribe(aSymbol: TSymbol);
begin
  if SubList.IndexOf(aSymbol.OrgCode) < 0 then
    SubList.Add(aSymbol.OrgCode);

  if App.AppStatus <> asShow then Exit;

  SubScribe( aSymbol, true );
end;

procedure TBinanceWebSocket.UnSubScribe(aSymbol: TSymbol);
var
  i : integer;
  aList : TStrings;
  sParam, sData : string;
begin
  i := SubList.IndexOf(aSymbol.OrgCode) ;
  if i >= 0 then SubList.Delete(i);

  SubScribe( aSymbol, false );
end;


function TBinanceWebSocket.MakeSubString : string;
var
  aList : TStrings;
  I: Integer;
begin
  aList := TStringList.Create;
  try
    Result := '';
    if SubList.Count <= 0 then Exit;

    for I := 0 to SubList.Count-1 do
    begin
      if FMarketType = mtSpot then
      begin
          aList.Add( SubList[i] +'@bookTicker' );
          aList.Add( SubList[i] +'@trade' );
          aList.Add( SubList[i] +'@miniTicker' );
      end else
      if FMarketType = mtFutures then
      begin
          aList.Add( SubList[i] +'@depth5' );
          aList.Add( SubList[i] +'@aggTrade' );
//          aList.Add( SubList[i] +'@miniTicker' );
      end ;
    end;

    for I := 0 to aList.Count-1 do
    begin
      Result := Result + Format('"%s"', [aList[i]]);
      if i < aList.Count-1  then
        Result := Result + ','
    end;

  finally
    aList.Free;
  end;

end;
procedure TBinanceWebSocket.SubscribeAll;
var
  sParam, sData : string;
begin
  sParam := MakeSubString;
  if sParam <> '' then
  begin
    inc(FSubIndex);
    sData := Format('{"method": "SUBSCRIBE","params":[%s],"id": %d}', [ sParam, FSubIndex ] );
    SendData(sData);
  end;
  //  바이낸스는..추가로 구독
  AllMarketTicker(true);
  //
  FTimer.Enabled := true;

end;

procedure TBinanceWebSocket.UnSubScribeAll;
var
  sParam, sData : string;
begin
  sParam := MakeSubString;
  if sParam <> '' then
  begin
    inc(FSubIndex);
    sData := Format('{"method": "UNSUBSCRIBE","params":[%s],"id": %d}', [ sParam, FSubIndex ] );
    SendData(sData);
  end;
  //  바이낸스는..추가로 구독취소
  AllMarketTicker(false);
end;


procedure TBinanceWebSocket.OnAfterConnect(Sender: TObject);
begin

  inherited OnAfterConnect(Sender);
  App.Log(llInfo, ' %s  %d.th Connected', [ Descript, ConnectTry ]);

  if ( ConnectTry > 1 ) and  ( ConnectTry < 10 ) and ( GetSockState = 'Open' ) then
    SubscribeAll;
end;

//procedure TBinanceWebSocket.OnAfterDisconnect(Sender: TObject);
//begin
//  //inherited;
//  App.Log(llInfo, ' %s Disconnected %d %s', [ Descript , integer(WebSocket.State),
//    WebSocket.CloseStatusDescription ]);
//
//end;



procedure TBinanceWebSocket.OnMessage(const S: string);
begin
  gBinReceiver.ParseSocketData( FMarketType, S);
end;


procedure TBinanceWebSocket.OnSubTimer(Sender: TObject);
var
  sTmp : string;
begin
  if FSendList.Count <= 0 then Exit;

  sTmp := FSendList[0];
  FSendList.Delete(0);
  SendData( sTmp );

end;

end.
