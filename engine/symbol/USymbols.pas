unit USymbols;

interface

uses

  system.Classes, system.SysUtils, system.DateUtils ,

  UFQN , UApiTypes, UMarketSpecs ,

  UTicks
  ;

type

  TSymbol = class;

  TSymbolFindEvent = function(stCode: String): TSymbol of object;

  TMarketDepth = class(TCollectionItem)
  private
    FPrice: Double;   // order price
    FVolume: double; // number of contracts
    FCnt: Integer;
  public
    property Price: Double read FPrice write FPrice;
    property Volume: double read FVolume write FVolume;
    property Cnt: Integer read FCnt write FCnt;
  end;

  TMarketDepths = class(TCollection)
  private
    FVolumeTotal: double;
    FCntTotal: Integer;
    FRealTimeAvg: double;
    FRealCount: integer;
    FRealVolSum: integer;

    function GetSize: Integer;
    procedure SetSize(const Value: Integer);
    function GetDepth(i: Integer): TMarketDepth;
  public
    constructor Create;

    property VolumeTotal: double read FVolumeTotal write FVolumeTotal;
    property CntTotal: Integer read FCntTotal write FCntTotal;

    property RealTimeAvg : double read FRealTimeAvg write FRealTimeAvg;
    property RealVolSum  : integer read FRealVolSum write FRealVolSum;
    property RealCount : integer read FRealCount write FRealCount;
    property Size: Integer read GetSize write SetSize;
    property Depths[i:Integer]: TMarketDepth read GetDepth; default;
  end;

  TTimeNSale = class(TCollectionItem)
  private
      // required
    FPrice: Double;
    FVolume: double;
    FTime: TDateTime;
      // optional I
    FSide: Integer;  // 1=long, -1=short
      // Korea only
    FDailyVolume: double;
    FDailyAmount: Currency;

    FLocalTime: TDateTime;
    FTick : TTickItem;
    FPrevPrice: Double;

  public
    property Price: Double read FPrice write FPrice;
    property PrevPrice : Double read FPrevPrice write FPrevPrice;
    property Volume: double read FVolume write FVolume;
    property Time: TDateTime read FTime write FTime;
    property LocalTime : TDateTime read FLocalTime write FLocalTime;

    property Side: Integer read FSide write FSide;
      //
    property DayVolume: double read FDailyVolume write FDailyVolume;
    property DayAmount: Currency read FDailyAmount write FDailyAmount;

    property Tick : TTickItem read FTick write FTick;
  end;

  TTimeNSales = class(TCollection)
  private
    FLast: TTimeNSale;
    FPrev: TTimeNSale;
    FMaxCount: Integer;
    function GetSale(i: Integer): TTimeNSale;
  public
    constructor Create;

    function New: TTimeNSale;
    property MaxCount: Integer read FMaxCount write FMaxCount;
    property Prev: TTimeNSale read FPrev;
    property Last: TTimeNSale read FLast;
    property Sales[i:Integer]: TTimeNSale read GetSale; default;
  end;

  TSymbol = class( TCollectionItem )
  private
 //   FExchangeCode: string;
    FCode: string;

    FQuoteCode: string;
    FOrgCode: string;
    FBaseCode: string;
    FLast: Double;
    FPrevOpen: double;
    FPrevHigh: Double;
    FDayLow: Double;
    FChange: Double;
    FPrevClose: Double;

    FLocalTime: TDateTime;
    FTime: TDateTime;
    FSide: Integer;
    FVolume: double;
    FDayOpen: Double;
    FBase: Double;
    FDayHigh: Double;
    FDailyAmount: Currency;
//    FPrevLast: Double;
    FPrevLow: Double;
    FDailyVolume: double;
    FSpec: TMarketSpec;
    FSettleCode: string;
    FTradeAble: boolean;
    FIsFuture: boolean;
    FIsMargin: boolean;
    FBids: TMarketDepths;
    FAsks: TMarketDepths;
    FSales: TTimeNSales;
    FTicks: TCollection;
    FSubCode: string;
    FTerms: TSTerms;
    FMarkeTerm: boolean;
    FAddTerm: boolean;
    FLastEventTime: TDateTime;
    FLastTime: TDateTime;
    FKimpPrice: double;
    FKimpAskPrice: double;
    FKimpBidPrice: double;
    FWithdrawlState: boolean;
    FDepositState: boolean;
    FDnwCount: integer;
    FWDCPrice: double;
    procedure OnTermAddEvent(Sender: TObject);
  public
    constructor Create( aColl : TCollection ); override;
    Destructor Destroy ; override;

    function  PriceToStr( Value : double ) : string;
    function  QtyToStr( Value : double ) : string;
    function  CheckDnwState( depoit, withdraw : boolean ) : integer;
    function  DayUpDown : double;
//    property  ExchangeCode : string read FExchangeCode;
    property  Code  : string read FCode write FCode;
    property  OrgCode : string read FOrgCode write FOrgCode;
    // 거래소별, 마켓별, 코드 같은경우가 많아서..유니크한 종목코드를 만들기 위해.
    property  SubCode : string read FSubCode write FSubCode;
    property  BaseCode  : string read FBaseCode write FQuoteCode;
    property  QuoteCode  : string read FQuoteCode write FQuoteCode;
    property  SettleCode : string read FSettleCode write FSettleCode;

    property Base: Double read FBase write FBase;
    property PrevClose: Double read FPrevClose write FPrevClose;
    property DayOpen: Double read FDayOpen write FDayOpen;
    property DayHigh: Double read FDayHigh write FDayHigh;
    property DayLow: Double read FDayLow write FDayLow;

    property Last: Double read FLast write FLast;
    property Change: Double read FChange write FChange;
//    property PrevLast : Double read FPrevLast write FPrevLast;
    property PrevHigh : Double read FPrevHigh write FPrevHigh;
    property PrevLow : Double read FPrevLow write FPrevLow;
    property PrevOpen: double read FPrevOpen write FPrevOpen;

    property KimpPrice : double read FKimpPrice write FKimpPrice;
    property WDCPrice  : double read FWDCPrice  write FWDCPrice;
    // 호가 기준 김프는 안 쓸듯...
    property KimpAskPrice : double read FKimpAskPrice write FKimpAskPrice;
    property KimpBidPrice : double read FKimpBidPrice write FKimpBidPrice;

    property DayVolume: double read FDailyVolume write FDailyVolume;
    property DayAmount: Currency read FDailyAmount write FDailyAmount;

    property Volume: double read FVolume write FVolume;
    property Time: TDateTime read FTime write FTime;
    property LocalTime : TDateTime read FLocalTime write FLocalTime;
    property Side: Integer read FSide write FSide;

    property Spec: TMarketSpec read FSpec write FSpec;
    property Asks : TMarketDepths read FAsks;
    property Bids : TMarketDepths read FBids;
    property Sales: TTimeNSales read FSales;
    property Ticks: TCollection read FTicks;
    //
    property TradeAble : boolean read FTradeAble write FTradeAble;
    property IsMargin  : boolean read FIsMargin write FIsMargin;
    property IsFuture  : boolean read FIsFuture write FIsFuture;
    //
    property WithDrawlState : boolean read FWithdrawlState write FWithdrawlState;
    property DepositState   : boolean read FDepositState   write FDepositState;

    property Terms: TSTerms read FTerms write FTerms;
    property MakeTerm: boolean read FMarkeTerm write FMarkeTerm;
    property AddTerm: boolean read FAddTerm write FAddTerm;

    property DnwCount : integer read FDnwCount;

    property LastTime : TDateTime read FLastTime write FLastTime;
    property LastEventTime  : TDateTime read FLastEventTime write FLastEventTime;
  end;

  TSymbols = class(TCollection)
  protected
  public
    function Find(stCode: String): TSymbol;
  end;

  TSymbolList = class(TStringList)
  private
    function GetSymbol(i: Integer): TSymbol;
  public
    constructor Create;

    function FindCode(stCode: String): TSymbol;
    function FindCode2( stCode : string ) : TSymbol;
    procedure AddSymbol(aSymbol: TSymbol);
    procedure AddSymbol2(aSymbol: TSymbol);
    procedure GetList( aList: TStrings); overload;
    procedure GetList( aList: TStrings; aMarket : TMarketType ); overload;
    procedure GetLists( aList : TSTrings; aMarkets : TMarketTypes );

    property Symbols[i:Integer]: TSymbol read GetSymbol; default;
  end;



  TBaseSymbols = class(TStringList)
  private
    function GetSymbolList( i : integer ) : TSymbolList;
  public
    constructor Create;
    destructor  Destroy; override;
    function FindSymbolList(sBase: String ): TSymbolList;
    function FindSymbol( sBase : string ;  aExKind : TExchangeKind ) : TSymbol; overload;
    function FindSymbol( sBase : string ;  aExKind : TExchangeKind; aMarket : TMarketType ) : TSymbol; overload;
    function FindSymbolEx( sBase : string ;  aExKind : TExchangeKind ) : TSymbol;
    procedure AddSymbol(aSymbol: TSymbol);
  end;

  TSpot  = class(TSymbol);

  TSpots = class(TSymbols)
  private
    function GetSpot(i: Integer): TSpot;
  public
    constructor Create;
    function New(stCode: String): TSpot;
    property Spots[i:Integer]: TSpot read GetSpot; default;
  end;

  TMargin = class(TSpot);

  TMargins = class(TSymbols)
  private
    function GetMargin(i: Integer): TMargin;
  public
    constructor Create;
    function New(stCode: String): TMargin;
    property Margins[i:Integer]: TMargin read GetMargin; default;
  end;

  TFuture = class(TSymbol)
  private
    FDeliveryTime: TDateTime;
    FMarkPrice: double;
    FUnderlying: TSymbol;
    FExpMonth: Integer;
    FExpYear: Integer;
    FExpDate: TDateTime;
    FDaysToExp: Integer;
    procedure SetExpDate(const Value: TDateTime);
  public
    property  DeliveryTime : TDateTime read FDeliveryTime write FDeliveryTime;
    property  MarkPrice : double read FMarkPrice write FMarkPrice;
    property  Underlying : TSymbol read FUnderlying write FUnderlying;

    property  DaysToExp: Integer read FDaysToExp write FDaysToExp;
    property  ExpDate: TDateTime read FExpDate write SetExpDate;
    property  ExpMonth: Integer read FExpMonth;
    property  ExpYear: Integer read FExpYear;
  end;

  TFutures = class(TSymbols)
  private
    function GetFuture(i: Integer): TFuture;
  public
    constructor Create;
    function New(stCode: String): TFuture;
    property Futures[i:Integer]: TFuture read GetFuture; default;
  end;


  TCommSymbolList = class(TList)
  private
    function GetCommSymbol(i: Integer): TSymbol;

  public

    function FindSymbol( aExKind : TExchangeKind;  sCode : string ) : TSymbol; overload;
    procedure SortByDailyAmount;
    property CommSymbols[i:Integer]: TSymbol read GetCommSymbol; default;
  end;




implementation

uses
  USymbolCore    , USymbolUtils
  , UConsts
  , GApp
  ;


{ TSymbol }

function TSymbol.CheckDnwState(depoit, withdraw: boolean): integer;
var
  b1 , b2 : boolean;
begin

  b1 := true; b2 := true;
  Result := 0;
  if FWithdrawlState then
    if not withdraw then
      b1 := false;           
  FWithdrawlState := withdraw;       

  if FDepositState then
    if not depoit then
      b2 := false;            
  FDepositState := depoit;

  if (not b1) and ( not b2 ) then
    Result := DNW_BOTH_FALE
  else if not b1 then
    Result := DWN_WITHDRAW_FALSE
  else if not b2 then
    Result := DWN_DEPOSIT_FALSE;

  if Result > 0 then
    App.Engine.SymbolCore.SymbolDnwStates[ Spec.ExchangeType ].AddSymbol( Self );

  if FDnwCount = 0 then
    Result := 0;

  inc( FDnwCount );

end;

constructor TSymbol.Create(aColl: TCollection);
begin
  inherited Create( aColl );

  FIsFuture:= false;
  FIsMargin:= false;

  FBids:= TMarketDepths.Create;
  FAsks:= TMarketDepths.Create;
  FSales:= TTimeNSales.Create;
  FTicks := TCollection.Create( TTickItem);

  FTerms    := TSTerms.Create;
  FTerms.OnAdd  := OnTermAddEvent;

  FAddTerm  := false;

  FWithdrawlState := true;
  FDepositState   := true;

  FDnwCount := 0;

  FKimpPrice := 0.0;
  FWDCPrice  := 0.0;

end;



function TSymbol.DayUpDown: double;
var
  dTmp : double;
begin
  if DayOpen <= 0 then  dTmp := 1
  else dTmp := DayOpen;

  Result := ( DayHigh - DayOpen) / dTmp * 100;
end;

destructor TSymbol.Destroy;
begin

  FTicks.free;
  FBids.Free;
  FAsks.Free;
  FSales.Free;

  FTerms.Free;

  inherited;
end;

procedure TSymbol.OnTermAddEvent(Sender: TObject);
begin
  FAddTerm  := true;
end;

function TSymbol.PriceToStr(Value: double): string;
var
  iPre : integer;
begin
  iPre   := GetPrecision(Self, Value );
  Result := Format('%.*n', [ iPre , Value + 0.0] );
end;

function TSymbol.QtyToStr(Value: double): string;
begin
  Result := Format('%.*n', [ Spec.QtyPrecision, Value + 0.0 ] );
end;

{ TSymbolList }

procedure TSymbolList.AddSymbol(aSymbol: TSymbol);
begin
  AddObject(aSymbol.Code, aSymbol);
end;

procedure TSymbolList.AddSymbol2(aSymbol: TSymbol);
begin
  AddObject(aSymbol.Spec.FQN, aSymbol);
end;

constructor TSymbolList.Create;
begin
  inherited Create;

  Sorted := True;
end;

function TSymbolList.FindCode(stCode: String): TSymbol;
begin
  Result := GetSymbol(IndexOf(stCode));
end;

function TSymbolList.FindCode2(stCode: string): TSymbol;
begin

end;

procedure TSymbolList.GetList(aList: TStrings);
var
  i: Integer;
  aSymbol: TSymbol;
begin
  if aList = nil then Exit;

  for i := 0 to Count - 1 do
  begin
    aSymbol := GetSymbol(i);
    aList.AddObject(aSymbol.Code, aSymbol);
  end;
end;

procedure TSymbolList.GetList(aList: TStrings; aMarket: TMarketType );
var
  i: Integer;
  aSymbol: TSymbol;
begin
  if aList = nil then Exit;

  for i := 0 to Count - 1 do
  begin
    aSymbol := GetSymbol(i);
    if aSymbol.Spec.Market = aMarket then
      aList.AddObject(aSymbol.Code, aSymbol);
  end;

end;

procedure TSymbolList.GetLists(aList: TSTrings; aMarkets: TMarketTypes);
var
  i : integer;
  aSymbol : TSymbol;
begin
  if aList = nil  then Exit;

  for i := 0 to Count - 1 do
  begin
    aSymbol := GetSymbol(i);
    if aSymbol.Spec.Market in aMarkets then
      aList.AddObject( aSymbol.Code, aSymbol);
  end;
end;

function TSymbolList.GetSymbol(i: Integer): TSymbol;
begin
  if (i >= 0) and (i <= Count-1) then
    Result := TSymbol(Objects[i])
  else
    Result := nil;
end;

{ TSymbols }

function TSymbols.Find(stCode: String): TSymbol;
var
  i: Integer;
begin
  Result := nil;

  for i := 0 to Count - 1 do
    if CompareStr((Items[i] as TSymbol).Code, stCode) = 0 then
    begin
      Result := Items[i] as TSymbol;
      Break;
    end;
end;

{ TStocks }

constructor TSpots.Create;
begin
  inherited Create(TSpot)
end;

function TSpots.GetSpot(i: Integer): TSpot;
begin
  if (i >= 0) and (i <= Count-1) then
    Result := Items[i] as TSpot
  else
    Result := nil;
end;

function TSpots.New(stCode: String): TSpot;
begin
  Result  := Add as TSpot;
  Result.FCode  := stCode;

end;

{ TFutures }

constructor TFutures.Create;
begin
  inherited Create(TFuture);
end;

function TFutures.GetFuture(i: Integer): TFuture;
begin
  if (i >= 0) and (i <= Count-1) then
    Result := Items[i] as TFuture
  else
    Result := nil;
end;

function TFutures.New(stCode: String): TFuture;
begin
  Result := Add as TFuture;
  Result.FCode := stCode;
end;

{ TMargins }

constructor TMargins.Create;
begin
  inherited Create(TMargin);
end;

function TMargins.GetMargin(i: Integer): TMargin;
begin
  if (i >= 0) and (i <= Count-1) then
    Result := Items[i] as TMargin
  else
    Result := nil;
end;


function TMargins.New(stCode: String): TMargin;
begin
  Result := Add as TMargin;
  Result.FCode := stCode;
end;

{ TFuture }

procedure TFuture.SetExpDate(const Value: TDateTime);
var
  wYear, wMonth, wDay: Word;
begin
  FExpDate := Value;

  DecodeDate(Value, wYear, wMonth, wDay);

  FExpMonth := wMonth;
  FExpYear := wYear;
end;

{ TMarketDepths }

constructor TMarketDepths.Create;
begin
  inherited Create(TMarketDepth);
  FRealCount  := 0;
  SetSize(5);
end;

function TMarketDepths.GetDepth(i: Integer): TMarketDepth;
begin
  if (i >= 0) and (i <= Count-1) then
    Result := Items[i] as TMarketDepth
  else
    Result := nil;
end;

function TMarketDepths.GetSize: Integer;
begin
  Result := Count;
end;

procedure TMarketDepths.SetSize(const Value: Integer);
var
  i: Integer;
begin
  if Value > 0 then
    if Value > Count then
    begin
      for i := Count to Value-1 do Add;
    end else
    if Value < Count then
    begin
      for i := Count-1 downto Value do Items[i].Free;
    end;

end;

{ TTimeNSales }

constructor TTimeNSales.Create;
begin
  inherited Create(TTimeNSale);

  FPrev := nil;
  FLast := nil;
  FMaxCount := 1000;
end;

function TTimeNSales.GetSale(i: Integer): TTimeNSale;
begin
  if (i >= 0) and (i <= Count-1) then
    Result := Items[i] as TTimeNSale
  else
    Result := nil;
end;

function TTimeNSales.New: TTimeNSale;
begin
    // limit size
  if (FMaxCount > 0) and (Count >= FMaxCount) then
    Items[Count-1].Free;

    // insert new object
  Result := Insert(0) as TTimeNSale;

  with Result do
  begin
    FVolume := 0;
    FSide := 0; // not assigned
  end;

    // store
  FPrev := FLast;
  FLast := Result;
end;



{ TCommSymbolList }

function TCommSymbolList.FindSymbol(aExKind: TExchangeKind;
  sCode: string): TSymbol;
var
  I: Integer;
  aSymbol : TSymbol;
begin
  Result := nil;
  for I := 0 to Count-1 do
  begin
    aSymbol := TSymbol( Items[i] );
    if ( aSymbol.Code = sCode )
      and ( aSymbol.Spec.ExchangeType = aExKind ) then
    begin
      Result := aSymbol;
      break;
    end;
  end;
end;

function TCommSymbolList.GetCommSymbol(i: Integer): TSymbol;
begin
  if( i < 0 ) or ( i >= Count ) then
    Result := nil
  else
    Result := TSymbol( Items[i] );
end;

procedure TCommSymbolList.SortByDailyAmount;
begin
  Sort( CompareDailyAmount );
end;

{ TBaseSymbols }

//constructor TBaseSymbols.Create;
//begin
//  inherited Create;
//  Sorted := true;
//
//  FSymbols := TList.Create;
//end;
//
//destructor TBaseSymbols.Destroy;
//begin
//  FSymbols.Free;
//  inherited;
//end;
//
//function TBaseSymbols.FindList(sBase: String): TList;
//begin
//  Result := GetList(IndexOf(sBase));
//end;
//
//function TBaseSymbols.GetList(i: Integer): TList;
//begin
//  if (i >= 0) and (i <= Count-1) then
//    Result := Objects[i] as TList
//  else
//    Result := nil;
//end;
//
//function TBaseSymbols.AddSymbols(aSymbol : TSymbol) : TList;
//begin
//  Result := FindList( aSymbol.Spec.BaseCode );
//  if Result = nil then
//  begin
//    AddObject(aSymbol.Spec.BaseCode , aSymbol);
//  end;
//end;

{ TBaseSymbols }

procedure TBaseSymbols.AddSymbol(aSymbol: TSymbol);
var
  aSymbolList : TSymbolList;
begin
  aSymbolList := FindSymbolList( aSymbol.Spec.BaseCode ) ;

  if aSymbolList = nil then
  begin
    aSymbolList := TSymbolList.Create;
    AddObject( aSymbol.Spec.BaseCode, aSymbolList );
  end;

  aSymbolList.AddSymbol2( aSymbol );
end;


constructor TBaseSymbols.Create;
begin
  inherited Create;
  Sorted := true;
end;

destructor TBaseSymbols.Destroy;
var
  I: Integer;
begin

  for I := 0 to Count-1 do
    Objects[i].Free;
  inherited;
end;

function TBaseSymbols.FindSymbol(sBase: string;
  aExKind: TExchangeKind): TSymbol;
  var
    aSymbolList : TSymbolList;
    aSymbol : TSymbol;
    I: Integer;
begin
  Result := nil;
  aSymbolList := FindSymbolList( sBase ) ;
  if aSymbolList <> nil then
    for I := 0 to aSymbolList.Count-1 do
    begin
      aSymbol := aSymbolList.Symbols[i];
      if aSymbol.Spec.ExchangeType = aExKind then
      begin
        Result := aSymbol;
        Break;
      end;
    end;
end;

function TBaseSymbols.FindSymbol(sBase: string; aExKind: TExchangeKind;
  aMarket: TMarketType): TSymbol;
  var
    aSymbolList : TSymbolList;
    aSymbol : TSymbol;
    I: Integer;
begin
  Result := nil;
  aSymbolList := FindSymbolList( sBase ) ;
  if aSymbolList <> nil then
    for I := 0 to aSymbolList.Count-1 do
    begin
      aSymbol := aSymbolList.Symbols[i];
      if ( aSymbol.Spec.ExchangeType = aExKind )
       and ( aSymbol.Spec.Market = aMarket) then
      begin
        Result := aSymbol;
        Break;
      end;
    end;
end;


function TBaseSymbols.FindSymbolEx(sBase: string;
  aExKind: TExchangeKind): TSymbol;
  var
    aSymbolList : TSymbolList;
    aSymbol : TSymbol;
    I: Integer;
begin
  Result := nil;
  aSymbolList := FindSymbolList( sBase ) ;
  if aSymbolList <> nil then
    for I := 0 to aSymbolList.Count-1 do
    begin
      aSymbol := aSymbolList.Symbols[i];

      if aExKind = App.Engine.SymbolCore.MainExKind then
      begin
        if App.Engine.SymbolCore.IsOSMain(aSymbol)then
        begin
          Result := aSymbol;
          Break;
        end
      end
      else begin
        if aSymbol.Spec.ExchangeType = aExKind then
        begin
          Result := aSymbol;
          Break;
        end;
      end;
    end;

end;

function TBaseSymbols.FindSymbolList(sBase: String): TSymbolList;
begin
  Result := GetSymbolList( IndexOf(sBase) );
end;


function TBaseSymbols.GetSymbolList(i: integer): TSymbolList;
begin
  if (i >= 0) and (i <= Count-1) then
    Result := Objects[i] as TSymbolList
  else
    Result := nil;
end;







end.
