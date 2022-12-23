unit UExchangeManager;
interface
uses
  system.Classes, system.SysUtils,
  UExchange     , UWebSockets,   USymbols,   UQuoteTimers,
  UOrders,
  UApiTypes 		
  ;
type
  TMarketArray = array [ TMarketType ] of TExchange;

  TExchangeManager = class
  private
    FExchanges: TMarketArray;
    FExchangeKind: TExchangeKind;
    FExchangeIdx: integer;
    FCodes: TStrings;
    FTimer: TQuoteTimer;
    FDone: boolean;
    FRcvCnt: int64;
    FSndCnt: int64;

    function GetMarketCount: integer;
    function CreateMarket( aMarket : TMarketType ) :  TExchange;
    function GetSndRcvCnt: string;
  public

    QuoteSock : array of TWebSocket;
    TradeSock : array [TMarketType] of TWebSocket;
    TopAmtSymbols : array [0..1] of TSymbol;

    Constructor Create( aExType : TExchangeKind ); overload;
    Destructor  Destroy; override;

    // 공통으로 같이 처리할수 있는것은 상속 처리 하지 않음..
    // 1. preparemaster
    // 2. requestmaster
    // 3. all websocket connect;
    // 4. all websocket close;
    function PrepareMaster : boolean; //virtual; abstract;
    function RequestMaster : boolean;
    function ConnectAll : boolean;
    function DissConnectAll : boolean;
    procedure init ;
    procedure StartRequest;

{$REGION 'to shared memory define' }
    function SendOrder( aOrder : TOrder ): boolean;
    procedure RequestOrderList( aSymbol : TSymbol ) ;
    procedure RequestTradeAmt( aSymbol : TSymbol ) ;
    procedure RequestOrdeDetail( aOrder : TOrder ) ;
    procedure RequestPosition( aSymbol : TSymbol  ) ;
		procedure RequestBalance( aSymbol : TSymbol ) ; overload;
    // only binance spot;
    procedure RequestOrderBook( aSymbol : TSymbol );
{$ENDREGION}
    // 상속 받아서 각 거래소 매니저에서 처리
    function InitMarketWebSockets : boolean ; virtual; abstract;
    function SubscribeAll : boolean; virtual; abstract;
    function MakeCloseData : boolean; virtual; abstract;

    // 종목별 주문리스트 조회


    function RequestBalance : boolean; overload; virtual; abstract;
    function RequestPositons : boolean; virtual; abstract;
    function RequestOrders: boolean; virtual; abstract;

    procedure UnSubscribeAll ; virtual; abstract;

    function Subscrib( aSymbol : TSymbol ) : boolean; virtual; abstract;
    function UnSubscrib( aSymbol : TSymbol ) : boolean; virtual; abstract;
    procedure RequestDNWState; virtual; abstract;

    // from shared memory
    procedure ReceivedData( aMarket : TMarketType; aReqType : TRequestType; aData, aRef  : string );

    function GetSndRcvCount : string;
    //  TExchangeMarketType
    property Exchanges   : TMarketArray read FExchanges write FExchanges;
    property MarketCount : integer read GetMarketCount;
    property ExchangeKind: TExchangeKind read FExchangeKind;
    property ExchangeIdx : integer read FExchangeIdx;
    // 교집합 코드를 담을 리스트..
    property Codes       : TStrings read FCodes;
    property Timer       : TQuoteTimer read FTimer write FTimer;
    property Done        : boolean read FDone write FDone;

    // 로그용  snd, rcv count
    property SndCnt : int64 read FSndCnt ;
    property RcvCnt : int64 read FRcvCnt ;
    property SndRcvCnt : string read GetSndRcvCnt;

  end;

implementation
uses
  GApp,  UApiConsts
  , UTypes
  , UBinanceSpotNMargin, UBinanceFutures
  , UBithSpot, UUpbitSpot
  , URestRequests
  ;

{ TExchangeManaager }


constructor TExchangeManager.Create(aExType: TExchangeKind);
var
  i : TMarketType;
begin
  FExchangeKind := aExType;
  FExchangeIdx  := integer( FExchangeIdx );
  
  for I := mtSpot to High(TMarketType) do
  begin
    FExchanges[i] :=  CreateMarket( i);
  end;

  FCodes:= TStringList.Create;

  QuoteSock := nil;
  TradeSock[mtSpot] := nil;
  TradeSock[mtFutures] := nil;

  TopAmtSymbols[0] := nil;
  TopAmtSymbols[1] := nil;

  FDone := false;

end;

function TExchangeManager.CreateMarket(aMarket: TMarketType): TExchange;
begin
  Result := nil;  
  case FExchangeKind of
    ekBinance:
      begin
        case aMarket of
          mtSpot  : Result := TBinanceSpotNMargin.Create( self, aMarket) ;
          mtFutures: Result := TBinanceFutures.Create( self, aMarket) ;
        end;
      end;                 
    ekUpbit:
      begin
        case aMarket of
          mtSpot  : Result := TUpbitSpot.Create( self, aMarket) ;
        end;

      end;
    ekBithumb:
      begin
        case aMarket of
          mtSpot  : Result := TBithSpot.Create( self, aMarket) ;
        end;     
      end;                       
  end;
end;

destructor TExchangeManager.Destroy;
var
  i : TMarketType;
  j : integer;
begin

  if FTimer <> nil then
    FTimer.Enabled := false;

  if QuoteSock <> nil then
    for j := 0 to High(QuoteSock) do
      QuoteSock[j].Free;

  for I := mtSpot to High(TMarketType) do
  begin
    if FExchanges[i] <> nil then
      FExchanges[i].Free;
  end;

  FCodes.Free;

  inherited;
end;


function TExchangeManager.ConnectAll: boolean;
var
  i : integer;
begin
  try
    if QuoteSock <> nil then
      for i := 0 to High(QuoteSock) do
        QuoteSock[i].DoConnect;

    if FExchangeKind = ekBinance then
    begin
      TradeSock[mtSpot].DoConnect;
      TradeSock[mtFutures].DoConnect;
    end;

  except
    on E : Exception do
    begin
      App.Log(llError, '%s %d.th connect error (%s)', [ TExchangeKindDesc[FExchangeKind], i, E.Message ]  );
      exit (false);
    end;
  end;


  FTimer.Enabled  := true;
  Result := true;
end;

function TExchangeManager.DissConnectAll: boolean;
var
  i : integer;
begin

  App.DebugLog('%s %d disconnect all ', [ TExchangeKindDesc[FExchangeKind],  High(QuoteSock) ] );

  if FTimer <> nil then
    FTimer.Enabled  := false;
  FDone := true;
  if QuoteSock <> nil then
    for i := 0 to High(QuoteSock) do
      QuoteSock[i].DoDissConnect;

  if FExchangeKind = ekBinance then
  begin
    if TradeSock[mtSpot] <> nil then TradeSock[mtSpot].DoDissConnect;
    if TradeSock[mtFutures] <> nil then TradeSock[mtSpot].DoDissConnect;
  end;

  Result := true;
end;

function TExchangeManager.GetMarketCount: integer;
begin
  Result := Integer(High(TMarketType));
end;

function TExchangeManager.GetSndRcvCnt: string;
begin
  Result := GetSndRcvCount;
//  Result := Format('%d, %d (%d)', [ FSndCnt, FRcvCnt, FSndCnt-FRcvCnt] );
end;

function TExchangeManager.GetSndRcvCount : string;
var
  aMarket : TMarketType;
  i : integer;
  a : TRequest;
begin
  aMarket := mtSpot;
  if FExchangeKind = ekBinance then
    aMarket := mtFutures;

  for i:=0 to Exchanges[aMarket].ReqItems.Count-1 do
  begin
    a := TRequest( Exchanges[aMarket].ReqItems.Items[i] );
    FSndCnt := FSndCnt + a.SndCnt;
    FRcvCnt := FRcvCnt + a.RcvCnt;
    Result := REsult + Format(' %s:%d,%d(%d)', [a.Name[1], a.SndCnt, a.RcvCnt, a.SndCnt - a.RcvCnt]);
  end;
end;

procedure TExchangeManager.init;
begin
  FTimer  := App.Engine.QuoteBroker.Timers.New;
  FTimer.Interval := 1000;
  FTimer.OnTimer  := nil;
  FTimer.Enabled  := false;
end;

function TExchangeManager.PrepareMaster: boolean;
var
  I: Integer;
  sTmp : string;
begin
  Result := Exchanges[mtSpot].PrepareMaster;

  if Result and ( FExchangeKind = ekBinance ) then
    Result := Exchanges[mtFutures].PrepareMaster;

  App.Log(llDebug, '', ' ---------- %s codes count %d -----------',
    [ TExchangeKindDesc[FExchangeKind], Exchanges[mtSpot].Codes.Count ] );

end;

procedure TExchangeManager.ReceivedData(aMarket: TMarketType;
  aReqType: TRequestType; aData, aRef: string);
begin
	Exchanges[aMarket].ReceivedData(aReqType, aData, aRef );
end;



function TExchangeManager.RequestMaster: boolean;
var
  I: TMarketType;
begin
  for I := mtSpot to High(TMarketType) do
  begin
    if Exchanges[i] = nil then continue;
    if not Exchanges[i].RequestMaster then
      Exit (false);
  end;

  Result := true;

end;

{$REGION 'to shared memory imple' }
procedure TExchangeManager.RequestOrdeDetail(aOrder: TOrder);
begin
  Exchanges[aOrder.Symbol.Spec.Market].RequestOrderDetail( aOrder );
end;

procedure TExchangeManager.RequestOrderBook(aSymbol: TSymbol);
begin
  Exchanges[aSymbol.Spec.Market].RequestOrderBook( aSymbol );
end;

procedure TExchangeManager.RequestOrderList(aSymbol: TSymbol);
begin
  Exchanges[aSymbol.Spec.Market].RequestOrderList( aSymbol );
end;

procedure TExchangeManager.RequestPosition(aSymbol: TSymbol);
begin
  Exchanges[aSymbol.Spec.Market].RequestPosition( aSymbol );
end;

procedure TExchangeManager.RequestTradeAmt(aSymbol: TSymbol);
begin
  Exchanges[aSymbol.Spec.Market].RequestTradeAmt( aSymbol );
end;

procedure TExchangeManager.RequestBalance(aSymbol: TSymbol);
begin
  Exchanges[aSymbol.Spec.Market].RequestBalance( aSymbol );
end;

function TExchangeManager.SendOrder(aOrder: TOrder): boolean;
begin
  if aOrder = nil then Exit ( false );
  result := Exchanges[aOrder.Symbol.Spec.Market].SenderOrder( aOrder );
end;
{$ENDREGION}

procedure TExchangeManager.StartRequest;
var
  I: TMarketType;
begin
  for I := mtSpot to High(TMarketType) do
    if ( Exchanges[i] <> nil ) and ( Exchanges[i].CyclicThreads <> nil )  then 
    	Exchanges[i].CyclicThreads.Resume;
end;

end.
