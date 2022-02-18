unit UMarketSpecs;

interface

uses
  system.Classes ,  system.SysUtils,

  UFQN, UApiTypes, UCollections , USymbolParser
  ;

type
  TMarketSpec = class( TCollectionItem )
  private
    FFQN: String;  // fully qualified name
      // FQN components
    FCountry: String;
    FExchange: String;
    FMarket: TMarketType;
    FUnderlying: String;

      // basic info
    FDescription: String;
    FSector: String;
    FCurrency: Integer;

      // point value
    FContractSize: Double; // for derivatives, in cash markets, it's '1'  ������
    FPriceQuote: Double;    // a unit currency 0.01 = cent if 'currency' is Dolloar
    FPointValue: Double;    // value for '1.0' as price = ContractSize * CurrencyUnit

      // minimum price movement(it's not fixed in Korean market)
    FTickSize: Double;   // actual tick size, if 'Fraction' > 0, TickSize = 1 / FFraction
    FTickValue: Double;  // = FTickSize * FPointValue
    FQtySize: Double;
    FPrecision: Integer;  // floating point precision

    FQuoteCode: string;
    FSettleCode: string;
    FBaseCode: string;
    FSubMarket: string;
    FExchangeType: TExchangeKind;

  public
    constructor Create(aColl: TCollection); override;
    Destructor  Destroy; override;

    procedure SetSpec( iPre : integer; dTickSize, dQtySize : double );

    property FQN: String read FFQN write FFQN;

    property Country: String read FCountry;
    property Exchange: String read FExchange;
    property Underlying: String read FUnderlying;
    property SubMarket : string read FSubMarket;

    property Description: String read FDescription write FDescription;

    property SettleCode: string read FSettleCode write FSettleCode;
    property BaseCode: string read FBaseCode write FBaseCode;
    property QuoteCode: string read FQuoteCode write FQuoteCode;

    property ContractSize: Double read FContractSize;
    property PriceQuote: Double read FPriceQuote;
    property PointValue: Double read FPointValue;

    property TickSize: Double read FTickSize;
    property TickValue: Double read FTickValue;
    property Precision: Integer read FPrecision;

    property Market : TMarketType read FMarket;
    property ExchangeType : TExchangeKind read FExchangeType write FExchangeType;

    property QtySize : double read FQtySize;

  end;

  TMarketSpecs = class(TCodedCollection)
  private
    FDefault: TMarketSpec;
    FFQNParser: TFQNParser;
    FSymbolParser: TSymbolParser;

    function GetSpec(i: Integer): TMarketSpec;
  public
    constructor Create;
    destructor Destroy; override;

    function New(stFQN: String): TMarketSpec;
    function Find(stFQN: String): TMarketSpec;

    property Specs[i: Integer]: TMarketSpec read GetSpec; default;
  end;



implementation

{ TMarketSpec }

constructor TMarketSpec.Create(aColl: TCollection);
begin
  inherited create(aColl);
    // point value
  FContractSize:= 1;
  FPriceQuote:= 1;
  FPointValue:= 1;

  FTickSize:= 1.0;
  FTickValue:= 1.0;
  FPrecision:= 1;

  FSubMarket  := '*';

end;

destructor TMarketSpec.Destroy;
begin

  inherited;
end;

procedure TMarketSpec.SetSpec(iPre: integer; dTickSize, dQtySize: double);
begin
  FPrecision  := iPre;
  FTickSize   := dTickSize;
  FQtySize    := dQtySize;
end;

{ TMarketSpecs }

constructor TMarketSpecs.Create;
begin
  inherited Create(TMarketSpec);

  FFQNParser := TFQNParser.Create;
  FSymbolParser := TSymbolParser.Create;

  FDefault := New('*.*.*');
  FDefault.FDescription := 'Default';
  FDefault.FUnderlying := '';

end;

destructor TMarketSpecs.Destroy;
begin
  FSymbolParser.Free;
  FFQNParser.Free;

  inherited;
end;

function TMarketSpecs.Find(stFQN: String): TMarketSpec;
var
  iPos: Integer;
begin
  stFQN := LowerCase(Trim(stFQN));

  iPos := FSortedList.IndexOf(stFQN);
  if iPos >= 0 then
    Result := SortedItems[iPos] as TMarketSpec
  else
    Result := nil;

end;


function TMarketSpecs.GetSpec(i: Integer): TMarketSpec;
begin
  if (i >= 0) and (i <= Count-1) then
    Result := SortedItems[i] as TMarketSpec
  else
    Result := nil;
end;

function TMarketSpecs.New(stFQN: String): TMarketSpec;
begin
  Result := Add(stFQN) as TMarketSpec;

  FFQNParser.FQN := stFQN;

  Result.FFQN := FFQNParser.MarketFQN;
  Result.FCountry := FFQNParser.Country;
  Result.FExchange := FFQNParser.Bourse;
  Result.FMarket := FFQNParser.MarketType;
  Result.FUnderlying := FFQNParser.Underlying;
  Result.FSubMarket  := FFQNParser.SubMarket;

end;


end.
