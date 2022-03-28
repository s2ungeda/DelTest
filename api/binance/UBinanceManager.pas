unit UBinanceManager;

interface

uses
  System.Classes, System.SysUtils, System.DateUtils,

  UExchangeManager, USymbols, UBinanceParse

  ,UApiTypes

  ;

type

  TBinanceManager = class( TExchangeManager )
  private
    FParse: TBinanceParse;
    procedure OnTimer( Sender : TObject );
  public

    Constructor  Create( aExType : TExchangeKind );
    Destructor  Destroy; override;

    function InitMarketWebSockets : boolean ; override;
    function SubscribeAll : boolean; override;
    procedure UnSubscribeAll ; override;

    function Subscrib( aSymbol : TSymbol ) : boolean; override;
    function UnSubscrib( aSymbol : TSymbol ) : boolean; override;

    property Parse : TBinanceParse read FParse;
  end;

implementation

uses
  GApp , Math
  , UApiConsts
  , UBinanceWebSockets
  ;

{ TBinanceManager }

constructor TBinanceManager.Create(aExType: TExchangeKind);
begin
  inherited Create( aExType );

  FParse:= TBinanceParse.Create(self);

end;

destructor TBinanceManager.Destroy;
//var
//  i : integer;
begin
//
//  for I := 0 to High(QuoteSock) do
//    QuoteSock[i].Free;

  FParse.Free;
  inherited;
end;

function TBinanceManager.InitMarketWebSockets: boolean;
var
  iCount , iMode, i, j : integer;
  aList : TStringList;
  aSymbol : TSymbol;
begin

  iCount := 2;
  SetLength( QuoteSock, iCount );

  QuoteSock[0]  := TBinanceWebSocket.Create(QOUTE_SOCK, 0,  mtSpot ) ;
  QuoteSock[0].init( 'stream.binance.com:9443/ws' );

  QuoteSock[1]  := TBinanceWebSocket.Create(QOUTE_SOCK, 1, mtFutures ) ;
  QuoteSock[1].init( 'fstream.binance.com/ws' );

  Timer.OnTimer := OnTimer;

  Result := true;
end;



procedure TBinanceManager.OnTimer(Sender: TObject);
var
  iState, i : integer;
begin

  // 1�ʿ� �ѹ�
  Exchanges[mtSpot].RequestDNWState;

  for I := 0 to High(QuoteSock) do
  begin
    iState := integer( QuoteSock[i].WebSocket.State );
    if  iState in [4..5] then
    begin
      QuoteSock[i].DoConnect;
 //     QuoteSock[i].
      App.DebugLog('%s, %d %s reconnect ', [ TExchangeKindDesc[ExchangeKind],i, QuoteSock[i].GetSockType ] );

      if QuoteSock[i].GetSockState = 'Open' then
      begin
        QuoteSock[i].SubscribeAll;
        App.Log(llInfo, '%s %.dth %s SubscribeAll ', [ TExchangeKindDesc[ExchangeKind],i, QuoteSock[i].GetSockType ] );
      end;
    end;
  end;
end;

function TBinanceManager.SubscribeAll: boolean;
var
  i, j : integer;
begin
//  Exit (true);
//  QuoteSock[0].DoConnect;
  for I := 0 to High(QuoteSock) do begin
    QuoteSock[i].SubscribeAll;
    sleep(500);
  end;
  result := true;
end;

function TBinanceManager.UnSubscrib(aSymbol: TSymbol): boolean;
var
  i : Integer;
begin
  Result := false;
  try
    i := ifThen( aSymbol.Spec.Market = mtSpot , 0, 1 );
    (QuoteSock[QOUTE_SOCK] as TBinanceWebSocket).UnSubScribe( aSymbol );
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
begin
  inherited;

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
