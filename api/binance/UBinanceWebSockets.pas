unit UBinanceWebSockets;

interface

uses
  System.Classes, System.SysUtils, System.DateUtils

  , UWebSockets

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

    procedure SyncProc;  override;
    function GetDescript: string;

  public
    Constructor Create( iSockDiv : Integer; aMtType : TMarketType ); overload;
    destructor Destroy; override;

    procedure CheckPingPong;

    procedure SetSubList( aList : TStringList ) ;
    procedure SubScribe( bSub : boolean ) ;


    property MarketType  : TMarketType read FMarketType;
    property SubList  : TStrings  read FSubList;
    property SubIndex : integer   read FSubIndex;
    property Descript : string    read GetDescript;
  end;

implementation

uses
  GApp , GLibs
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

constructor TBinanceWebSocket.Create(iSockDiv: Integer; aMtType: TMarketType);
begin
  inherited Create( iSockDiv );

  FMarketType := aMtType;
  FSubList    := TStringList.Create;
  FSubIndex   := 0;
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
// 1. 구독취소 함
// 2. 구독취소 성공 후 구독 함..
procedure TBinanceWebSocket.SetSubList(aList: TStringList);
var
  sData, sParam : string;
  i : integer;
begin

  if aList.Count <= 0 then Exit ;

  // 구독 json pack
  if FMarketType = mtSpot then
  begin
    for I := 0 to aList.Count-1 do
    begin
      FSubList.Add( aList[i]+'@bookTicker' );
      FSubList.Add( aList[i]+'@trade' );
      FSubList.Add( aList[i]+'@miniTicker' );
    end;
  end else
  if FMarketType = mtFutures then
  begin
    for I := 0 to aList.Count-1 do
    begin
      FSubList.Add( aList[i]+'@depth5' );
      FSubList.Add( aList[i]+'@aggTrade' );
      FSubList.Add( aList[i]+'@miniTicker' );
    end;
  end;

//  DoConnect;

//  SubScribe( true );
end;

procedure TBinanceWebSocket.SubScribe( bSub : boolean );
var
  I: Integer;
  sData, sParam, sTmp : string;
begin

  sParam := '';
  for I := 0 to FSubList.Count-1 do
  begin
    sParam := sParam + Format('"%s"', [FSubList[i]]);
    if i < FSubList.Count-1  then
      sParam := sParam + ','
  end;

  sTmp := ifThenStr( bSub,'SUBSCRIBE', 'UNSUBSCRIBE');

  inc(FSubIndex);
  sData := Format('{"method": "%s","params":[%s],"id": %d}', [ sTmp, sParam, FSubIndex ] );

  SendData(sData);
  if not bSub then FSubList.Clear;

  App.DebugLog(' %s %s :%s', [ Descript, sTmp, sData] ) ;

end;

procedure TBinanceWebSocket.OnAfterConnect(Sender: TObject);
begin
  if (FSubList.Count > 0 ) then
    SubScribe( true );
end;

procedure TBinanceWebSocket.OnAfterDisconnect(Sender: TObject);
begin
  //inherited;
  App.Log(llInfo, ' %s Disconnected', [ Descript]);
end;



procedure TBinanceWebSocket.SyncProc;
begin
  //
  gBinReceiver.ParseSocketData( FMarketType, string(Data.Packet));
end;

end.
