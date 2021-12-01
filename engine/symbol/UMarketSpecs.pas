unit UMarketSpecs;

interface

uses
  system.Classes ,

  UFQN, UCollections , USymbolParser
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
    FContractSize: Double; // for derivatives, in cash markets, it's '1'  계약단위
    FPriceQuote: Double;    // a unit currency 0.01 = cent if 'currency' is Dolloar
    FPointValue: Double;    // value for '1.0' as price = ContractSize * CurrencyUnit

      // minimum price movement(it's not fixed in Korean market)
    FTickSize: Double;   // actual tick size, if 'Fraction' > 0, TickSize = 1 / FFraction
    FTickValue: Double;  // = FTickSize * FPointValue
    FPrecision: Integer; // floating point precision
  public
    constructor Create(aColl: TCollection); override;
    Destructor  Destroy; override;

    property FQN: String read FFQN write FFQN;

    property Country: String read FCountry;
    property Exchange: String read FExchange;
    property Underlying: String read FUnderlying;

    property Description: String read FDescription write FDescription;

    property ContractSize: Double read FContractSize;
    property PriceQuote: Double read FPriceQuote;
    property PointValue: Double read FPointValue;

    property TickSize: Double read FTickSize;
    property TickValue: Double read FTickValue;
    property Precision: Integer read FPrecision;
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
    function Find2(stPM: String): TMarketSpec;
      // temporary until find a good architecture
    function FindByUSSymbol(stSymbol: String): TMarketSpec;

    function Represent: String;

    property Specs[i: Integer]: TMarketSpec read GetSpec; default;
  end;

implementation

{ TMarketSpec }

constructor TMarketSpec.Create(aColl: TCollection);
begin
  inherited create(aColl);

end;

destructor TMarketSpec.Destroy;
begin

  inherited;
end;

{ TMarketSpecs }

constructor TMarketSpecs.Create;
begin

end;

destructor TMarketSpecs.Destroy;
begin

  inherited;
end;

function TMarketSpecs.Find(stFQN: String): TMarketSpec;
begin

end;

function TMarketSpecs.Find2(stPM: String): TMarketSpec;
begin

end;

function TMarketSpecs.FindByUSSymbol(stSymbol: String): TMarketSpec;
begin

end;

function TMarketSpecs.GetSpec(i: Integer): TMarketSpec;
begin

end;

function TMarketSpecs.New(stFQN: String): TMarketSpec;
begin

end;

function TMarketSpecs.Represent: String;
begin

end;

end.
