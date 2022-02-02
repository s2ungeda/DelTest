unit USymbols;

interface

uses

  system.Classes, system.SysUtils, system.DateUtils ,

  UFQN , UApiTypes, UMarketSpecs
  ;

type


  TSymbol = class( TCollectionItem )
  private
    FExchangeCode: string;
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
    FVolume: int64;
    FDayOpen: Double;
    FBase: Double;
    FDayHigh: Double;
    FDailyAmount: Currency;
    FPrevLast: Double;
    FPrevLow: Double;
    FDailyVolume: int64;
    FSpec: TMarketSpec;
    FSettleCode: string;
    FTradeAble: boolean;
    FIsFuture: boolean;
    FIsMargin: boolean;
  public
    constructor Create( aColl : TCollection ); override;
    Destructor Destroy ; override;

    property  ExchangeCode : string read FExchangeCode;
    property  Code  : string read FCode write FCode;
    property  OrgCode : string read FOrgCode write FOrgCode;
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
    property PrevLast : Double read FPrevLast write FPrevLast;
    property PrevHigh : Double read FPrevHigh write FPrevHigh;
    property PrevLow : Double read FPrevLow write FPrevLow;
    property PrevOpen: double read FPrevOpen write FPrevOpen;

    property DayVolume: int64 read FDailyVolume write FDailyVolume;
    property DayAmount: Currency read FDailyAmount write FDailyAmount;

    property Volume: int64 read FVolume write FVolume;
    property Time: TDateTime read FTime write FTime;
    property LocalTime : TDateTime read FLocalTime write FLocalTime;
    property Side: Integer read FSide write FSide;

    property Spec: TMarketSpec read FSpec write FSpec;
    //
    property TradeAble : boolean read FTradeAble write FTradeAble;
    property IsMargin  : boolean read FIsMargin write FIsMargin;
    property IsFuture  : boolean read FIsFuture write FIsFuture;
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
    procedure GetList( aList: TStrings); overload;
    procedure GetList( aList: TStrings; aMarket : TMarketType ); overload;
    procedure GetLists( aList : TSTrings; aMarkets : TMarketTypes );

    property Symbols[i:Integer]: TSymbol read GetSymbol; default;
  end;

  TSpot  = class(TSymbol);

  TSpots = class(TSymbols)
  private
    function GetSpot(i: Integer): TSpot;
  public
    constructor Create;
    function New(stCode: String): TSpot;
    property Stocks[i:Integer]: TSpot read GetSpot; default;
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

implementation

{ TSymbol }

constructor TSymbol.Create(aColl: TCollection);
begin
  inherited Create( aColl );

  FIsFuture:= false;
  FIsMargin:= false;

end;

destructor TSymbol.Destroy;
begin

  inherited;
end;

{ TSymbolList }

procedure TSymbolList.AddSymbol(aSymbol: TSymbol);
begin
  AddObject(aSymbol.Code, aSymbol);
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

end.
