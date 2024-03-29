unit UQuoteBroker;

interface

uses
  System.Classes, System.SysUtils, System.DateUtils

  , UTypes , UApiTypes

  , UCollections   , UQuoteTimers    , UDistributor

  , USymbols
  ;

type

  TQuote = class;

  TQuoteEvent = function(aQuote: TQuote) : boolean of object;
 // TQuoteType = (qtNone, qtMarketDepth, qtTimeNSale, qtCustom, qtUnknown);

  TQuote = class( TCollectionItem )
  private
    FSymbol: TSymbol;
    FDistributor: TDistributor;
    FLastEvent: TQuoteType;
    FReqSnapshot: boolean;
    FEventBuffer: TStrings;
    FRcvSnapshot: boolean;
    FBids: TMarketDepths;
    FAsks: TMarketDepths;

    procedure SetSymbol(const Value: TSymbol);
    procedure checkHoga;

//    procedure CalcKimp( aPrice : double; aSymbol : TSymbol ); overload;
//    procedure CalcKimp; overload;
//    procedure CalcMainKimp;  overload;
//    procedure CalcMainKimp( aExKind: TExchangeKind ) ;   overload;

  public
    constructor Create(aColl: TCollection); override;
    destructor Destroy; override;

    procedure Update( dtTime : TDateTime );
    procedure SetMarketDepth( iSize : integer );

    property Distributor: TDistributor read FDistributor;
    property LastEvent : TQuoteType read FLastEvent write FLastEvent;
    property Symbol: TSymbol read FSymbol write SetSymbol;
    // only binance spot - local order book flag
    property ReqSnapshot : boolean read FReqSnapshot write FReqSnapshot;
    property RcvSnapshot : boolean read FRcvSnapshot write FRcvSnapshot;
    property EventBuffer : TStrings read FEventBuffer write FEventBuffer;

    property Asks : TMarketDepths read FAsks;
    property Bids : TMarketDepths read FBids;

  end;

  TQuoteBroker = class(TCodedCollection)
  private

      // events
    FOnSubscribe: TQuoteEvent;
    FOnCancel: TQuoteEvent;
    FOnFind: TSymbolFindEvent;

      // time stamp
    FLastEventTime: TDateTime;  // PC time, real-time
    FLastQuoteTime: TDateTime;
    FSortList: TList;
     // data(server) time, near real-time or historical

    FExKind : TExchangeKind;

    procedure QuoteUpdated(Sender: TObject);

  public
    QuoteIndex  : integer;

    constructor Create;
    destructor Destroy; override;

    function Subscribe(aSubscriber: TObject; aSymbol: TSymbol; aHandler: TDistributorEvent;  bMakeTerm : boolean): TQuote; overload;

    ///
    function Subscribe(aSubscriber: TObject; aSymbol: TSymbol;  iDataID : integer; aHandler: TDistributorEvent): TQuote; overload;
    function Subscribe(aSubscriber: TObject; aSymbol: TSymbol;  aHandler: TDistributorEvent; EvnetID: TDistributorIDs ): TQuote; overload;

    function Subscribe(aSubscriber: TObject; aSymbol: TSymbol;  aHandler: TDistributorEvent): TQuote; overload;
    function Subscribe(aSubscriber: TObject; stSymbol: String; aHandler: TDistributorEvent): TQuote; overload;

    function Find(stSymbol: String): TQuote;

    procedure Cancel(aSubscriber: TObject; stSymbol: String); overload;
    procedure Cancel(aSubscriber: TObject; aSymbol: TSymbol); overload;
    procedure Cancel(aSubscriber: TObject); overload;
    procedure Cancel(aSubscriber: TObject; bSymbolCore : boolean); overload;

      // quote broker main events
    property OnSubscribe: TQuoteEvent read FOnSubscribe write FOnSubscribe;
    property OnCancel: TQuoteEvent read FOnCancel write FOnCancel;
    property OnFind: TSymbolFindEvent read FOnFind write FOnFind;

      // quote timer related

    property LastQuoteTime: TDateTime read FLastQuoteTime;
    property LastEventTime: TDateTime read FLastEventTime;
      // log
      // Elw

    procedure DummyEventHandler(Sender, Receiver: TObject;
      DataID: Integer; DataObj: TObject; EventID: TDistributorID);

    function SubscribeTest(aSubscriber: TObject; aSymbol: TSymbol;
      aHandler: TDistributorEvent; iTrCode : integer): TQuote; overload;

    function GetSubscribeCount: integer;
    function GetQuoted(iPos: integer): TQuote;
  end;

  TBrokerArray  = array [TExchangeKind] of TQuoteBroker;

  TQuoteBrokerManager = class
  private
    FBrokers: TBrokerArray;
    FQuoteTimers: TQuoteTimers;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Cancel(aSubscriber: TObject);
    procedure SetEvent;

    property Brokers : TBrokerArray read FBrokers;
    property Timers: TQuoteTimers read FQuoteTimers;
  end;

implementation

uses
  GApp  , UApiConsts, Math  , UConsts
  , UTicks
  ;

procedure TQuoteBroker.Cancel(aSubscriber: TObject; bSymbolCore: boolean);
var
  i: Integer;
  aQuote: TQuote;
begin
  for i := Count - 1 downto 0 do
  begin
    aQuote := Items[i] as TQuote;
    aQuote.FDistributor.Cancel(aSubscriber);

    if aQuote.FDistributor.Count = 0 then
    begin
      if Assigned(FOnCancel) then
        FOnCancel(aQuote);

      aQuote.Free;
    end;
  end;

  FSortList.Clear;

end;


constructor TQuoteBroker.Create;
var
  i : integer;
begin
  inherited Create(TQuote);

  FLastEventTime := 0.0;
  FLastQuoteTime := 0.0;

  FSortList := TList.Create;

  QuoteIndex  := 0;
end;

destructor TQuoteBroker.Destroy;
begin

  FSortList.Free;
  //Quotes.Free;

  inherited;
end;



procedure TQuoteBroker.DummyEventHandler(Sender, Receiver: TObject;
  DataID: Integer; DataObj: TObject; EventID: TDistributorID);
begin
  //
end;



//---------------------------------------------------------------------< find >

function TQuoteBroker.Find(stSymbol: String): TQuote;
var
  iPos: Integer;
begin
  Result := nil;

  stSymbol := UpperCase(Trim(stSymbol));

  iPos := FSortedList.IndexOf(stSymbol);
  if iPos >= 0 then
    Result := FSortedList.Objects[iPos] as TQuote;
end;


function TQuoteBroker.GetSubscribeCount : integer;
begin
  Result := FSortedList.Count;
end;

function TQuoteBroker.GetQuoted( iPos : integer) : TQuote;
begin
  Result := FSortedList.Objects[iPos] as TQuote;
end;

procedure TQuoteBroker.QuoteUpdated(Sender: TObject);
begin

end;

//---------------------------------------------------------------< subscribe >

function TQuoteBroker.Subscribe(aSubscriber: TObject; stSymbol: String;
  aHandler: TDistributorEvent): TQuote;
var
  stCode: String;
begin
  Result := Find(stSymbol);

  if Result = nil then
  begin
    stCode := UpperCase(Trim(stSymbol));
    Result := Add(stCode) as TQuote;
//    Result.FSymbolCode := stCode;
//    Result.FOnUpdate := QuoteUpdated;

      // find symbol object (optional)
    if Assigned(FOnFind) then
      Result.Symbol := OnFind(stCode);

//    Result.Symbol.Quote := Result;
      // if necessary, subscribe to quote provider(brokerage server or else)
    if Assigned(FOnSubscribe) then
      FOnSubscribe(Result);
  end;

  Result.FDistributor.Subscribe(aSubscriber, 0, Result, ANY_EVENT, aHandler);
end;

function TQuoteBroker.Subscribe(aSubscriber: TObject; aSymbol: TSymbol;
  aHandler: TDistributorEvent): TQuote;
var
  stCode: String;
begin
  Result := nil;

  if aSymbol = nil then Exit;

  Result := Find(aSymbol.Code);

  if Result = nil then
  begin
    stCode := UpperCase(Trim(aSymbol.Code));
    Result := Add(stCode) as TQuote;
//    Result.FSymbolCode := stCode;
    //Result.SetSymbol( aSymbol );
    Result.Symbol := aSymbol;
//    Result.Symbol.Quote := Result;
//    Result.FOnUpdate := QuoteUpdated;
    // if necessary, subscribe to quote provider(brokerage server or else)
    if Assigned(FOnSubscribe) then
      if not FOnSubscribe(Result) then
      begin
//        Result.Symbol.Quote := nil;
        Result.Free;
        Exit;
      end;
  end;

  Result.FDistributor.Subscribe(aSubscriber, 0, Result, ANY_EVENT, aHandler);
end;

function TQuoteBroker.Subscribe(aSubscriber: TObject; aSymbol: TSymbol;
  aHandler: TDistributorEvent; EvnetID: TDistributorIDs): TQuote;
var
  stCode: String;
begin
  Result := nil;

  if aSymbol = nil then Exit;

  Result := Find(aSymbol.Code);

  if Result = nil then
  begin
    stCode := UpperCase(Trim(aSymbol.Code));
    Result := Add(stCode) as TQuote;
//    Result.FSymbolCode := stCode;
    //Result.SetSymbol( aSymbol );
    Result.Symbol := aSymbol;
//    Result.Symbol.Quote := Result;
//    Result.FOnUpdate := QuoteUpdated;

    // if necessary, subscribe to quote provider(brokerage server or else)
    if Assigned(FOnSubscribe) then
      FOnSubscribe(Result);
  end;

  Result.FDistributor.Subscribe(aSubscriber, 0, Result, EvnetID, aHandler);

end;

function TQuoteBroker.Subscribe(aSubscriber: TObject; aSymbol: TSymbol;
  iDataID: integer; aHandler: TDistributorEvent): TQuote;
var
  stCode: String;
begin
  Result := nil;

  if aSymbol = nil then Exit;

  Result := Find(aSymbol.Code);

  if Result = nil then
  begin
    stCode := UpperCase(Trim(aSymbol.Code));
    Result := Add(stCode) as TQuote;
//    Result.FSymbolCode := stCode;
    //Result.SetSymbol( aSymbol );
    Result.Symbol := aSymbol;
//    Result.Symbol.Quote := Result;
//    Result.FOnUpdate := QuoteUpdated;
    // if necessary, subscribe to quote provider(brokerage server or else)
    if Assigned(FOnSubscribe) then
      FOnSubscribe(Result);

  end;

  Result.FDistributor.Subscribe(aSubscriber, iDataID, Result, ANY_EVENT, aHandler);

end;

function TQuoteBroker.Subscribe(aSubscriber: TObject; aSymbol: TSymbol;
  aHandler: TDistributorEvent; bMakeTerm: boolean ): TQuote;
var
  stCode: String;
begin
  Result := nil;

  if aSymbol = nil then Exit;

  Result := Find(aSymbol.Code);

  if Result = nil then
  begin
    stCode := UpperCase(Trim(aSymbol.Code));
    Result := Add(stCode) as TQuote;
//    Result.FSymbolCode := stCode;
    //Result.SetSymbol( aSymbol );
    Result.Symbol := aSymbol;
//    Result.Symbol.Quote := Result;
//    Result.FOnUpdate := QuoteUpdated;

    // if necessary, subscribe to quote provider(brokerage server or else)
    if Assigned(FOnSubscribe) then
      FOnSubscribe(Result);
  end;

//  if not Result.MakeTerm then
//    Result.MakeTerm := bMakeTerm;

  Result.FDistributor.Subscribe(aSubscriber, 0, Result, ANY_EVENT, aHandler);
end;


function TQuoteBroker.SubscribeTest(aSubscriber: TObject; aSymbol: TSymbol;
  aHandler: TDistributorEvent; iTrCode : integer): TQuote;
var
  stCode: String;
begin
  Result := nil;

  if aSymbol = nil then Exit;

  Result := Find(aSymbol.Code);

  if Result = nil then
  begin
    stCode := UpperCase(Trim(aSymbol.Code));
    Result := Add(stCode) as TQuote;
//    Result.FSymbolCode := stCode;
    Result.Symbol := aSymbol;
//    Result.FOnUpdate := QuoteUpdated;
  end;

  if iTrCode = 9 then
    FSortList.Add( Result );

  Result.FDistributor.Subscribe(aSubscriber, 0, Result, ANY_EVENT, aHandler);
end;

//------------------------------------------------------------------< cancel >

procedure TQuoteBroker.Cancel(aSubscriber: TObject; stSymbol: String);
var
  aQuote: TQuote;
begin
  aQuote := Find(stSymbol);

  if aQuote <> nil then
  begin
    aQuote.FDistributor.Cancel(aSubscriber, 0, aQuote);

    if aQuote.FDistributor.Count = 0 then
    begin
      if Assigned(FOnCancel) then
        FOnCancel(aQuote);

      aQuote.Free;
    end;
  end;
end;

procedure TQuoteBroker.Cancel(aSubscriber: TObject; aSymbol: TSymbol);
var
  aQuote: TQuote;
begin
  if (aSubscriber = nil) or (aSymbol = nil) then Exit;

  aQuote := Find(aSymbol.Code);

  if aQuote <> nil then
  begin
    aQuote.FDistributor.Cancel(aSubscriber, 0, aQuote);

    if aQuote.FDistributor.Count = 0 then
    begin
      if Assigned(FOnCancel) then
        FOnCancel(aQuote);

      aQuote.Free;
    end;
  end;
end;

procedure TQuoteBroker.Cancel(aSubscriber: TObject);
var
  i: Integer;
  aQuote: TQuote;
begin
  for i := Count - 1 downto 0 do
  begin
    aQuote := Items[i] as TQuote;
    aQuote.FDistributor.Cancel(aSubscriber);

    if aQuote.FDistributor.Count = 0 then
    begin
      if Assigned(FOnCancel) then
        FOnCancel(aQuote);

      aQuote.Free;
    end;
  end;
end;


constructor TQuote.Create(aColl: TCollection);
begin
  inherited Create(aColl);
  FReqSnapshot  := false;
  FRcvSnapshot  := false;

  FAsks := nil;
  FBids := nil;

  FSymbol := nil;
  FDistributor := TDistributor.Create;
  FEventBuffer := TStringList.Create;
end;

destructor TQuote.Destroy;
begin
  if FAsks <> nil then
    FAsks.Free;
  if FBids <> nil then
    FBids.Free;
  FEventBuffer.Free;
  FDistributor.Free;
  inherited;
end;



procedure TQuote.SetMarketDepth(iSize: integer);
begin
  FAsks := TMarketDepths.Create;
  FBids := TMarketDepths.Create;
  FAsks.Size  := iSize;
  FBids.Size  := iSize;
end;

procedure TQuote.SetSymbol(const Value: TSymbol);
begin
  FSymbol := Value;
end;



procedure TQuote.Update(dtTime: TDateTime);
var
  aTick : TTickItem;
begin

  FSymbol.AddTerm := false;

  if ( FSymbol.Sales <> nil ) and ( FLastEvent = qtTimeNSale ) then
  begin
//    aTick   := TTickITem( FSymbol.Ticks.Add );
//    aTick.T := FSymbol.Sales.Last.LocalTime;
//    aTick.C := FSymbol.Last;
//    aTick.FillVol := FSymbol.Sales.Last.Volume;
//    aTick.AccVol  := FSymbol.DayVolume;
//    aTick.Side    := FSymbol.Side;
//    aTick.AskPrice:= FSymbol.Asks[0].Price;
//    aTick.BidPrice:= FSymbol.Bids[0].Price;
//
//    FSymbol.Terms.NewTick(aTick)   ;

    App.Engine.SymbolCore.CalcIndex( FSymbol );
    App.Engine.TradeCore.Positions[ FSymbol.Spec.ExchangeType].UpdatePosition( FSymbol );

    FSymbol.LastTradeTime := dtTime;

  end else
  if FLastEvent = qtMarketDepth then
  begin

  end;

  checkHoga;    
  FSymbol.LastEventTime := dtTime;
  FSymbol.LastTime      := now;

  FDistributor.Distribute(Self, 0, Self, 0);
end;

procedure TQuote.checkHoga;
var
  bLog : boolean;
begin
  Exit;
  try

    if FSymbol.Spec.ExchangeType = ekBithumb then Exit;

    if isZero( FSymbol.Asks[0].Price ) or isZero( FSymbol.Bids[0].Price ) or isZero( FSymbol.Last ) then
      Exit;

    bLog := false;
    if FSymbol.Last > ( FSymbol.Asks[0].Price + DOUBLE_EPSILON )  then
      bLog := true
    else if FSymbol.Bids[0].Price > ( FSymbol.Last + DOUBLE_EPSILON )   then
      bLog := true;


    if bLog then
    begin
      var sData : string;
      sData := Format('%s %s : %s %s [%s] %s %s ', [ TExchangeKindDesc[ FSymbol.Spec.ExchangeType ],  FSymbol.Code
        , FSymbol.PriceToStr( FSymbol.Asks[1].Price )
        , FSymbol.PriceToStr( FSymbol.Asks[0].Price )
        , FSymbol.PriceToStr( FSymbol.Last )
        , FSymbol.PriceToStr( FSymbol.Bids[0].Price )
        , FSymbol.PriceToStr( FSymbol.Bids[1].Price )
        ] );
      App.Log(llInfo, 'price', sData );
    end;
  except
  end;

end;

{ TQuoteBrokerCore }

procedure TQuoteBrokerManager.Cancel(aSubscriber: TObject);
var
  I: TExchangeKind;
begin
  for I := ekBinance to High(TExchangeKind) do
    FBrokers[i].Cancel(aSubscriber);
end;

constructor TQuoteBrokerManager.Create;
var
  I: TExchangeKind;
begin
  for I := ekBinance to High(TExchangeKind) do
  begin
    FBrokers[i] := TQuoteBroker.Create;
    FBrokers[i].FExKind := i;
  end;

  FQuoteTimers:= TQuoteTimers.Create;
  FQuoteTimers.Realtime := true;
end;

destructor TQuoteBrokerManager.Destroy;
var
  I: TExchangeKind;
begin
  for I := ekBinance to High(TExchangeKind) do
    FBrokers[i].Free;
  FQuoteTimers.Free;
  inherited;
end;

procedure TQuoteBrokerManager.SetEvent;
var
  I: TExchangeKind;
begin

  for I := ekBinance to High(TExchangeKind) do
  begin
    FBrokers[i].OnSubscribe := App.Engine.ApiManager.Sub;
    FBrokers[i].FOnCancel   := App.Engine.ApiManager.UnSub;
  end;

end;

end.
