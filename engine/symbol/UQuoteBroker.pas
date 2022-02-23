unit UQuoteBroker;

interface

uses
  System.Classes, System.SysUtils, System.DateUtils

  , UTypes

  , UCollections   , UQuoteTimers    , UDistributor

  , USymbols
  ;

type

  TQuote = class;

  TQuoteEvent = function(aQuote: TQuote) : boolean of object;

  TQuote = class( TCollectionItem )
  private
    FSymbol: TSymbol;
    FDistributor: TDistributor;

    procedure SetSymbol(const Value: TSymbol);
  public
    constructor Create(aColl: TCollection); override;
    destructor Destroy; override;


    property Distributor: TDistributor read FDistributor;
    property Symbol: TSymbol read FSymbol write SetSymbol;
  end;

  TQuoteBroker = class(TCodedCollection)
  private
      // utility objects
    FQuoteTimers: TQuoteTimers;

      // events
    FOnSubscribe: TQuoteEvent;
    FOnCancel: TQuoteEvent;
    FOnFind: TSymbolFindEvent;

      // time stamp
    FLastEventTime: TDateTime;  // PC time, real-time
    FLastQuoteTime: TDateTime;
    FSortList: TList;
     // data(server) time, near real-time or historical
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
    property Timers: TQuoteTimers read FQuoteTimers;
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

implementation


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

  FQuoteTimers := TQuoteTimers.Create;
  FQuoteTimers.Realtime := true;// ( gEnv.RunMode <> rtSimulation );

  FLastEventTime := 0.0;
  FLastQuoteTime := 0.0;

  FSortList := TList.Create;

  QuoteIndex  := 0;
end;

destructor TQuoteBroker.Destroy;
begin

  FSortList.Free;
  FQuoteTimers.Free;
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

  //Result.FDistributor.Subscribe(aSubscriber, 0, Result, TICK_EVENTS, aHandler, spType);

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
//
//  if gEnv.Engine.SymbolCore.Futures[0] = aSymbol then    //최근월선물은 항상 만든다
//    Result.MakeTerm := true;

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
  FSymbol := nil;
  FDistributor := TDistributor.Create;
end;

destructor TQuote.Destroy;
begin
  FDistributor.Free;
  inherited;
end;



procedure TQuote.SetSymbol(const Value: TSymbol);
begin
  FSymbol := Value;
end;




end.
