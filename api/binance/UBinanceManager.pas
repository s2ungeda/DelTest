unit UBinanceManager;

interface

uses
  System.Classes, System.SysUtils, System.DateUtils,

  UExchangeManager, USymbols, UBinanceParse , UQuoteTimers

  ,UApiTypes

  ;

type

  TBinanceManager = class( TExchangeManager )
  private
  	FSeq  : int64;
    FParse: TBinanceParse;
    FDepTimer: TQuoteTimer;
    procedure OnTimer( Sender : TObject );
  public

    Constructor  Create( aExType : TExchangeKind );
    Destructor  Destroy; override;

    function InitMarketWebSockets : boolean ; override;
    function SubscribeAll : boolean; override;
    procedure UnSubscribeAll ; override;
    function MakeCloseData : boolean ; override;

    function RequestBalance : boolean; override;
    function RequestPositons : boolean; override;
    function RequestOrders: boolean; override;

    function Subscrib( aSymbol : TSymbol ) : boolean; override;
    function UnSubscrib( aSymbol : TSymbol ) : boolean; override;

    procedure OnDepthTimer(Sender: TObject);

    property Parse : TBinanceParse read FParse;
    property DepTimer : TQuoteTimer read FDepTimer;
    
  end;

implementation

uses
  GApp , Math
  , UApiConsts
  , UBinanceWebSockets, UBinanceFutures, UBinanceSpotNMargin
  ;

{ TBinanceManager }

constructor TBinanceManager.Create(aExType: TExchangeKind);
begin
  inherited Create( aExType );

  FParse:= TBinanceParse.Create(self);
  FSeq	:= 0;
end;

destructor TBinanceManager.Destroy;
//var
//  i : integer;
begin
//
//  for I := 0 to High(QuoteSock) do
//    QuoteSock[i].Free;

  if FDepTimer <> nil then
    FDepTimer.Enabled := false;
  FParse.Free;
  inherited;
end;

function TBinanceManager.InitMarketWebSockets: boolean;
var
  iCount , iMode, i, j : integer;
  aList : TStringList;
  aSymbol : TSymbol;
  sKey : string;
begin

  iCount := 2;
  SetLength( QuoteSock, iCount );

  QuoteSock[0]  := TBinanceWebSocket.Create(QOUTE_SOCK, 0,  mtSpot ) ;
  QuoteSock[0].init( 'stream.binance.com:9443/ws' );

  QuoteSock[1]  := TBinanceWebSocket.Create(QOUTE_SOCK, 1, mtFutures ) ;
//  QuoteSock[1].init( 'fstream.binance.com/ws' );    // 리얼
  QuoteSock[1].init( 'stream.binance.com/ws' ); // 테스트넷


  sKey  := (Exchanges[mtSpot] as TBinanceSpotNMargin).RequestListenKey( true );
  TradeSock[mtSpot]  := TBinanceWebSocket.Create(TRADE_SOCK, 0,  mtSpot ) ;
  TradeSock[mtSpot].init( 'stream.binance.com:9443/ws/'+sKey );

  sKey  := (Exchanges[mtFutures] as TBinanceFutures).RequestListenKey( true );
  TradeSock[mtFutures]  := TBinanceWebSocket.Create(TRADE_SOCK, 1, mtFutures ) ;
  TradeSock[mtFutures].init( 'stream.binancefuture.com/ws/'+sKey ); // 테스트넷

  Timer.OnTimer := OnTimer;

  Result := true;

  FDepTimer  := App.Engine.QuoteBroker.Timers.New;
  FDepTimer.Enabled  := false;
  FDepTimer.Interval := 500;
  FDepTimer.OnTimer  := OnDepthTimer;    
end;



function TBinanceManager.MakeCloseData: boolean;
var
  sTmp : string;
begin
  stmp := App.Engine.SymbolCore.MainSymbols[msBTC][ExchangeKind].OrgCode;
  Result := Exchanges[mtFutures].RequestCandleData('1d', sTmp )
    and Exchanges[mtFutures].RequestCandleData('30m', sTmp )    ;

  stmp := App.Engine.SymbolCore.MainSymbols[msETH][ExchangeKind].OrgCode;
  Result := Exchanges[mtFutures].RequestCandleData('1d', sTmp )
    and Exchanges[mtFutures].RequestCandleData('30m', sTmp )    ;
end;

procedure TBinanceManager.OnDepthTimer(Sender: TObject);
begin
  if Done then
  begin
    FDepTimer.Enabled := false;
    Exit;
  end;

// (Exchanges[mtFutures] as TBinanceFutures).RequestData( FSeq mod 2 );
//  inc(FSeq);
//
//  if FSeq > 99999999 then
//  	FSeq := 0;
end;

procedure TBinanceManager.OnTimer(Sender: TObject);
//var
//  iState, i : integer;
begin
  // 1초에 한번
  //Exchanges[mtSpot].RequestDNWState;

end;

function TBinanceManager.RequestBalance: boolean;
begin
  Result := Exchanges[mtSpot].RequestBalance
    and Exchanges[mtFutures].RequestBalance
end;

function TBinanceManager.RequestOrders: boolean;
begin

end;

function TBinanceManager.RequestPositons: boolean;
begin

end;

function TBinanceManager.SubscribeAll: boolean;
var
  i, j : integer;
begin

  for I := 0 to High(QuoteSock) do begin
    QuoteSock[i].SubscribeAll;
    sleep(500);
  end;

//  FDepTimer.Enabled := true;
    
  result := true;
end;

function TBinanceManager.UnSubscrib(aSymbol: TSymbol): boolean;
var
  i : Integer;
begin
  Result := false;
  try
    i := ifThen( aSymbol.Spec.Market = mtSpot , 0, 1 );
    (QuoteSock[i] as TBinanceWebSocket).UnSubScribe( aSymbol );
    Result := true;
  except
  end;
end;

function TBinanceManager.Subscrib(aSymbol: TSymbol): boolean;
var
  i : integer;
begin
  Result := false;
  try
    i := ifThen( aSymbol.Spec.Market = mtSpot , 0, 1 );
    (QuoteSock[i] as TBinanceWebSocket).SubScribe( aSymbol );
    Result := true;
  except

  end;
end;


procedure TBinanceManager.UnSubscribeAll;
var
  i, j : integer;
begin

  for I := 0 to High(QuoteSock) do
  begin
   (QuoteSock[i] as TBinanceWebSocket).UnSubScribeAll;
    sleep(500);
  end;

end;

//function TBinanceManager.RequestMaster: boolean;
//var
//  I: TMarketType;
//begin
//  for I := mtSpot to High(TMarketType) do
//    if not Exchanges[i].RequestMaster then
//      Exit (false);
//
//  Result := true;
//end;



end.
