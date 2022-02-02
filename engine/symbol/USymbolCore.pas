unit USymbolCore;

interface

uses
  system.Classes, system.SysUtils ,

  UFQN, UMarketSpecs, USymbols, UMarkets,

  UApiTypes,

  UApiConsts
  ;

type

  TSymbolArray  = array [TExchangeKind] of TSymbolList;
  TSpotArray    = array [TExchangeKind] of TSpots;
  TFutureArray  = array [TExchangeKind] of TFutures;

  TMarketArray        = array [TExchangeKind] of TMarketList;
  TSpotMarketArray    = array [TExchangeKind] of TSpotMarkets;
  TFutureMarketArray  = array [TExchangeKind] of TFutureMarkets;

  TMarketGroupsArray  = array [ TExchangeKind ] of TMarketGroups;


  TSymbolCore = class
  private

    FSymbols: TSymbolArray;
    FSpots: TSpotArray;
    FFutures: TFutureArray;

    FMarkets: TMarketArray;
    FSpotMarkets: TSpotMarketArray;
    FFutureMarkets: TFutureMarketArray;

    FSpecs: TMarketSpecs;

    FExchanges: TMarketGroupsArray;
    FUnderlyings: TMarketGroupsArray;
  public
    constructor Create;
    destructor Destroy; override;

    procedure RegisterSymbol(aExKind : TExchangeKind; aSymbol: TSymbol); overload;
    function  RegisterSymbol(aExKind : TExchangeKind; aMarket : TMarketType; aCode : string ) : TSymbol ; overload;
    function  FindSymbol(aExKind : TExchangeKind; aCode : string ): TSymbol;

    property Specs: TMarketSpecs read FSpecs;

    property Symbols: TSymbolArray read FSymbols;
    property Spots: TSpotArray read FSpots;
    property Futures: TFutureArray read FFutures;

    property Markets: TMarketArray read FMarkets;
    property SpotMarkets: TSpotMarketArray read FSpotMarkets;
    property FutureMarkets: TFutureMarketArray read FFutureMarkets;

    property Underlyings : TMarketGroupsArray read FUnderlyings;
    property Exchanges   : TMarketGroupsArray read FExchanges;
  end;

implementation

{ TSymbolCore }

constructor TSymbolCore.Create;
var
  I: TExchangeKind;
begin
  FSpecs:= TMarketSpecs.Create;

  for I := ekBinance to High( TExchangeKind ) do
  begin
    FSymbols[i] := TSymbolList.Create;
    FSpots[i]   := TSpots.Create;

    FFutures[i] := TFutures.Create;

    FMarkets[i]       := TMarketList.Create;
    FSpotMarkets[i]   := TSpotMarkets.Create;
    FFutureMarkets[i] := TFutureMarkets.Create;

    FExchanges[i]   := TMarketGroups.Create;
    FUnderlyings[i] := TMarketGroups.Create;

  end;

end;

destructor TSymbolCore.Destroy;
var
  I: TExchangeKind;
begin

  for I := ekBinance to High( TExchangeKind ) do
  begin
    FSymbols[i].Free;
    FSpots[i].Free;

    FFutures[i].Free;

    FMarkets[i].Free;
    FSpotMarkets[i].Free;
    FFutureMarkets[i].Free;

    FExchanges[i].Free;
    FUnderlyings[i].Free;
  end;

   FSpecs.Free;

  inherited;
end;

function TSymbolCore.FindSymbol(aExKind: TExchangeKind; aCode: string): TSymbol;
begin
  Result := FSymbols[aExKind].FindCode( aCode );
end;

function TSymbolCore.RegisterSymbol(aExKind: TExchangeKind;
  aMarket: TMarketType; aCode: string): TSymbol;
  var
    aSpec : TMarketSpec;
    sFQN  : string;
    aUnder: TSymbol;
begin
  case aMarket of
    mtSpot:
      begin
        Result := FSpots[aExKind].New(aCode);
        sFQN   := Format( '%s@%s', [ aCode,  GetBaseSpotFQN( aExKind ) ]);
        aSpec  := FSpecs.Find( sFQN );
        if aSpec = nil then
        begin
          aSpec := FSpecs.New(sFQN);
        end;
      end;
    mtFutures:
      begin
        Result := FFutures[aExKind].New(aCode);
        aUnder := FSpots[aExKind].Find( aCode );

        if aUnder = nil then
          Exit ( nil );

        with Result as TFuture do
          Underlying := aUnder;

        sFQN   := Format( '%s@%s', [ aCode,  GetBaseFuturesFQN( aExKind ) ]);
        aSpec  := FSpecs.Find( sFQN );
        if aSpec = nil then
        begin
          aSpec := FSpecs.New(sFQN);
        end;
      end;
  end;

  if aSpec <> nil then
    Result.Spec := aSpec
  else
    Result := nil;
end;

procedure TSymbolCore.RegisterSymbol(aExKind : TExchangeKind; aSymbol: TSymbol);
var
  aMarket : TMarket;
begin
  if aSymbol = nil then Exit;

  if FSymbols[aExKind].IndexOfObject(aSymbol) < 0 then
    FSymbols[aExKind].AddObject(aSymbol.Code, aSymbol );

  if aSymbol.Spec <> nil then
  begin
    aMarket := FMarkets[aExKind].FindMarket( aSymbol.Spec.FQN );

    if aMarket = nil then
    begin

      case aSymbol.Spec.Market of
        mtSpot:     aMarket := FSpotMarkets[aExKind].New(aSymbol.Spec.FQN);
        mtFutures:  aMarket := FFutureMarkets[aExKind].New(aSymbol.Spec.FQN);
        else
          Exit;
      end;
    end;

    aMarket.Spec  := aSymbol.Spec;
    FMarkets[aExKind].AddMarket(aMarket);

    if (aSymbol.Spec.Market = mtFutures ) and (( aSymbol as TFuture ).Underlying <> nil ) then
        FUnderlyings[aExKind].AddMarket(aMarket,
                               aSymbol.Spec.SubMarket
                               + aSymbol.Spec.Underlying
                               + '.' + aSymbol.Spec.Exchange
                               + '.' + aSymbol.Spec.Country,
                               (aSymbol as TFuture).Underlying.Code,
                               (aSymbol as TFuture).Underlying);

    FExchanges[aExKind].AddMarket(aMarket,
                         aSymbol.Spec.Exchange + '.' + aSymbol.Spec.Country,
                         aSymbol.Spec.Exchange);
  end;

  aMarket.AddSymbol( aSymbol );

end;

end.
