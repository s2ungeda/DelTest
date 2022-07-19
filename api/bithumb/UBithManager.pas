unit UBithManager;

interface

uses
  System.Classes, System.SysUtils, System.DateUtils,

  UExchangeManager, UBithParse , USymbols, UOrders,  UQuoteTimers,

  UApiTypes

  ;

type

  TBithManager = class( TExchangeManager )
  private
    FParse: TBithParse;
    FDepTimer: TQuoteTimer;
    FCount  : int64;
    procedure OnTimer( Sender : TObject );
  public

    Constructor  Create( aExType : TExchangeKind );
    Destructor  Destroy; override;

    function InitMarketWebSockets : boolean ; override;
    function SubscribeAll : boolean; override;
    procedure UnSubscribeAll ; override;

    function RequestBalance : boolean; override;
    function RequestPositons : boolean; override;
    function RequestOrders: boolean; override;

    function MakeCloseData : boolean ; override;

    function Subscrib( aSymbol : TSymbol ) : boolean; override;
    function UnSubscrib( aSymbol : TSymbol ) : boolean; override;

//    function SendOrder( aOrder : TOrder ): boolean;  override;
//    procedure RequestOrderList( aSymbol : TSymbol ); override;
//    procedure RequestOrdeDetail( aOrder : TOrder ) ; override;
//    procedure RequestBalance( aSymbol : TSymbol ) ; override;
//    procedure ReceivedData( aMarket : TMarketType; aReqType : TRequestType;  aData, aRef : string ); override;

    procedure OnDepthTimer(Sender: TObject);
    procedure OnSendDoneEvent(  Sender : TObject );

    property Parse : TBithParse read FParse;
    property DepTimer : TQuoteTimer read FDepTimer;
  end;

implementation

uses
  GApp
  , UApiConsts
  , UBithSpot
  , UBithWebSockets
  , URestRequests
  , USharedConsts
  ;
{ TBinanceManager }

constructor TBithManager.Create(aExType: TExchangeKind);
begin
  inherited Create( aExType );
  FParse  := TBithParse.Create( self );
  FParse.OnSendDone := OnSendDoneEvent;
  FCount := 0;
end;

destructor TBithManager.Destroy;
begin                                    
	if FDepTimer <> nil then
    FDepTimer.Enabled := false;

  FParse.Free;
  inherited;
end;


const divCnt = 40;
function TBithManager.InitMarketWebSockets: boolean;
var
  i, j, iCount : integer;
//  aList : TStringList;
//  aSymbol : TSymbol;
begin

  iCount := 1;
  SetLength( QuoteSock, iCount );

  for I := 0 to iCount-1 do begin
    QuoteSock[i]  := TBithWebSocket.Create(QOUTE_SOCK, i, mtSpot ) ;
    QuoteSock[i].init('pubwss.bithumb.com/pub/ws' );
  end;

  Result := true;

  // orderbook 을 위한 조회 타이머.
  FDepTimer  := App.Engine.QuoteBroker.Timers.New;
  FDepTimer.Enabled  := false;
  FDepTimer.Interval := 100;
  FDepTimer.OnTimer  := OnDepthTimer;

  Timer.OnTimer   := OnTimer;
end;

function TBithManager.MakeCloseData : boolean ;
var
  sTmp : string;
begin
  stmp := App.Engine.SymbolCore.MainSymbols[msBTC][ExchangeKind].OrgCode;
  Result := Exchanges[mtSpot].RequestCandleData('24h', sTmp )
    and Exchanges[mtSpot].RequestCandleData('30m', sTmp )    ;

  stmp := App.Engine.SymbolCore.MainSymbols[msETH][ExchangeKind].OrgCode;
  Result := Exchanges[mtSpot].RequestCandleData('24h', sTmp )
    and Exchanges[mtSpot].RequestCandleData('30m', sTmp )    ;

end;

procedure TBithManager.OnDepthTimer(Sender: TObject);
begin

//  thread 로 대체
  if Done then
  begin
    FDepTimer.Enabled := false;
    Exit;
  end;

//  (Exchanges[mtSpot] as TBithSpot).RequestData;
//  Exchanges[mtSpot].ProcCyclicWork;
end;


procedure TBithManager.OnTimer(Sender: TObject);
var
  i, j : integer;
  a : TRequest;
begin
  //(Exchanges[mtSpot] as TBithSpot).RequestData(1);
  //inc( FCount);
//  j := 0;
//  for i:=0 to Exchanges[mtSpot].ReqItems.Count-1 do
//  begin
//    a := TRequest(Exchanges[mtSpot].ReqItems.Items[i]);
//    SndCnt := SndCnt + a.SndCnt ;
//    RcvCnt := RcvCnt
//    //a.SndCnt  := 0;
//  end;
//
//  App.Log(llInfo,'rate', '%s req cnt : %d', [  TExchangeKindDesc[ExchangeKind] , j ] )  ;

end;



procedure TBithManager.OnSendDoneEvent(Sender: TObject);
begin
  if Sender = FParse then
    (QuoteSock[QOUTE_SOCK] as TBithWebSocket).Send;
end;


//function TBithManager.SendOrder(aOrder: TOrder): boolean;
//begin
//  if aOrder = nil then Exit ( false );
//  result := Exchanges[aOrder.Symbol.Spec.Market].SenderOrder( aOrder );
//end;

function TBithManager.Subscrib(aSymbol: TSymbol): boolean;
begin
//  self.Exchanges[ aSymbol.Spec.Market ].
  Result := false;
  try
    (QuoteSock[QOUTE_SOCK] as TBithWebSocket).SubScribe( aSymbol);
    Result := true;
  except

  end;

end;

function TBithManager.UnSubscrib(aSymbol: TSymbol): boolean;
begin
(QuoteSock[QOUTE_SOCK] as TBithWebSocket).UnSubScribe( aSymbol);
  Result := true;
end;


function TBithManager.SubscribeAll: boolean;
var
  i, j : integer;
begin
  //Exit (true);

  for I := 0 to High(QuoteSock) do
    QuoteSock[i].SubscribeAll;
  result := true;

  FDepTimer.Enabled := true;
end;


procedure TBithManager.UnSubscribeAll;
begin
  inherited;

end;

// from shared memroy
//procedure TBithManager.ReceivedData( aMarket : TMarketType; aReqType : TRequestType;  aData, aRef: string);
//begin
//	Exchanges[aMarket].ReceivedData(aReqType, aData, aRef );
//end;



function TBithManager.RequestBalance: boolean;
begin
	Result := Exchanges[mtSpot].RequestBalance;
end;



//procedure TBithManager.RequestOrderList(aSymbol: TSymbol);
//begin
//  Exchanges[aSymbol.Spec.Market].RequestOrderList( aSymbol );
//end;
//
//procedure TBithManager.RequestBalance(aSymbol: TSymbol);
//begin
//  Exchanges[aSymbol.Spec.Market].RequestBalance( aSymbol );
//end;
//
//procedure TBithManager.RequestOrdeDetail(aOrder: TOrder);
//begin
//  Exchanges[aOrder.Symbol.Spec.Market].RequestOrderDetail( aOrder );
//end;

function TBithManager.RequestOrders: boolean;
begin
	Result := Exchanges[mtSpot].RequestOrders;
end;

function TBithManager.RequestPositons: boolean;
begin

end;

//function TBithManager.RequestMaster: boolean;
//var
//  I: TMarketType;
//begin
//  for I := mtSpot to High(TMarketType) do
//  begin
//    if Exchanges[i] = nil then continue;
//    if not Exchanges[i].RequestMaster then
//      Exit (false);
//  end;
//
//  Result := true;
//
//end;

end.
