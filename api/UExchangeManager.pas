unit UExchangeManager;
interface
uses
  system.Classes, system.SysUtils,
  UExchange     , UWebSockets,   USymbols,   UQuoteTimers,
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
    function GetMarketCount: integer;
    function CreateMarket( aMarket : TMarketType ) :  TExchange;
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

    // 상속 받아서 각 거래소 매니저에서 처리
    function InitMarketWebSockets : boolean ; virtual; abstract;
    function SubscribeAll : boolean; virtual; abstract;
    function MakeCloseData : boolean; virtual; abstract;
    procedure UnSubscribeAll ; virtual; abstract;


    function Subscrib( aSymbol : TSymbol ) : boolean; virtual; abstract;
    function UnSubscrib( aSymbol : TSymbol ) : boolean; virtual; abstract;
    procedure RequestDNWState; virtual; abstract;

    //  TExchangeMarketType
    property Exchanges   : TMarketArray read FExchanges write FExchanges;
    property MarketCount : integer read GetMarketCount;
    property ExchangeKind: TExchangeKind read FExchangeKind;
    property ExchangeIdx : integer read FExchangeIdx;
    // 교집합 코드를 담을 리스트..
    property Codes       : TStrings read FCodes;
    property Timer       : TQuoteTimer read FTimer write FTimer;
    property Done       : boolean read FDone write FDone;
  end;

implementation
uses
  GApp,  UApiConsts
  , UBinanceSpotNMargin, UBinanceFutures
,   UBithSpot, UUpbitSpot
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
  Result := true;
end;

function TExchangeManager.GetMarketCount: integer;
begin
  Result := Integer(High(TMarketType));
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

end.
