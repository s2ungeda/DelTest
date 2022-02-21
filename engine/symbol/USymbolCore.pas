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

    FExchanges  : TMarketGroupsArray;
    FUnderlyings: TMarketGroupsArray;
  public
    constructor Create;
    destructor Destroy; override;

    procedure RegisterSymbol(aExKind : TExchangeKind; aSymbol: TSymbol); overload;
    function  RegisterSymbol(aExKind : TExchangeKind; aMarket : TMarketType; aCode : string ) : TSymbol ; overload;
    function  FindSymbol(aExKind : TExchangeKind; aCode : string ): TSymbol;
    function  FindQuoteSymbol(aExKind : TExchangeKind; sBaseCode : string ): TSymbol;

    procedure Log;

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

function TicksFromPrice(aSymbol: TSymbol; dPrice: Double; iTicks: Integer): Double;
function LocksFromPrice(aSymbol: TSymbol; dPrice: Double; iTicks: Integer): Double;

implementation

uses
  GApp, UConsts
  ;


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

function TSymbolCore.FindQuoteSymbol(aExKind: TExchangeKind;
  sBaseCode: string): TSymbol;
var
  I: Integer;
begin
  Result := nil;
  // 일단 나중에..빠른  검색 로직을 추가 하자..
  for I := 0 to FSpots[aExKind].Count -1 do
    if FSpots[aExKind].Spots[i].Spec.BaseCode = sBaseCode then
    begin
      Result := FSpots[aExKind].Spots[i];
      break;
    end;
end;

function TSymbolCore.FindSymbol(aExKind: TExchangeKind; aCode: string): TSymbol;
begin
  Result := FSymbols[aExKind].FindCode( aCode );
end;

procedure TSymbolCore.Log;
var
  I: TExchangeKind;
  j, k: Integer;
  aFutMarket : TFutureMarket;
  aGroup : TMarketGroup;
  aSymbol : TSymbol;
  aMarket : TMarket;
  stData : string;
begin
  Exit;

  for I := ekBinance to High(TExchangeKind) do
  begin
    App.DebugLog( '============  %s Symbols total : %03d  --> spot : %03d future : %03d'
      ,[ TExchangeKindDesc[i], FSymbols[i].Count, FSpots[i].Count, FFutures[i].Count ] );

    for j := 0 to FFutures[i].Count -1 do
    begin
      aSymbol := FFutures[i].Futures[j] as TSymbol;
      App.DebugLog( 'fut (%d), %s,  %s ', [
        j, aSymbol.Code, aSymbol.Spec.FQN
        ]);
    end;

    for j := 0 to Underlyings[i].Count - 1 do
    begin
      aGroup := Underlyings[i].Groups[j];
      for k := 0 to aGroup.Markets.Count - 1 do
      begin
        aMarket := aGroup.Markets.Markets[k];
        App.DebugLog( 'Under %d(%d), %s, %s, %s ', [
          j,k, aMarket.FQN, aMarket.Spec.FQN, aGroup.FQN
          ]);

      end;
    end;

    for j := 0 to FutureMarkets[i].Count -1 do
    begin
      aFutMarket := FutureMarkets[i].FutureMarkets[j];
      for k := 0 to aFutMarket.Symbols.Count - 1 do
      begin
        aSymbol := aFutMarket.Symbols.Symbols[k];
        App.DebugLog( 'fut %d(%d), %s, (%s) ', [
          j,k , aSymbol.Code, aSymbol.Spec.FQN
          ]);

      end;
    end;

  end;
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
        aUnder := FSpots[aExKind].Find( aCode );
        if aUnder = nil then
          Exit ( nil );

        Result := FFutures[aExKind].New(aCode+Fut_Suf);   // add perpectual future Suffix;
        with Result as TFuture do
          Underlying := aUnder;

        sFQN   := Format( '%s@%s.%s', [ aCode+Fut_Suf, aCode, GetBaseFuturesFQN( aExKind ) ]);
        aSpec  := FSpecs.Find( sFQN );
        if aSpec = nil then
        begin
          aSpec := FSpecs.New(sFQN);
        end;
      end;
  end;

  if aSpec <> nil then begin
    aSpec.ExchangeType  := aExKind;
    Result.Spec := aSpec
  end
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



function TicksFromPrice(aSymbol: TSymbol; dPrice: Double; iTicks: Integer): Double;
var
  i, iSign : Integer;
begin
  Result := dPrice;

  if (iTicks = 0)
     or (aSymbol = nil)
     or (aSymbol.Spec = nil) then Exit;

  case aSymbol.Spec.ExchangeType of
    ekBinance: ;
    ekUpbit: ;
    ekBithumb:
      begin
        iSign := iTicks div Abs(iTicks);

        for i:=1 to Abs(iTicks) do
        if iSign > 0 then
        begin
          if Result > 1000000.0 - EPSILON then
            Result := Result + 1000
          else if Result > 500000.0 - EPSILON then
            Result := Result + 500
          else if Result > 100000 - EPSILON then
            Result := Result + 100
          else if Result > 50000 - EPSILON then
            Result := Result + 50
          else if Result > 10000 - EPSILON then
            Result := Result + 10
          else if Result > 5000 - EPSILON then
            Result := Result + 5
          else if Result > 1000 - EPSILON then
            Result := Result + 1
          else if Result > 100 - EPSILON then
            Result := Result + 0.1
          else if Result > 10 - EPSILON then
            Result := Result + 0.01
          else if Result > 1 - EPSILON then
            Result := Result + 0.001
          else
            Result := Result + 0.0001;
        end else
        begin
          if Result < 1 + EPSILON then
            Result := Result - 0.0001
          else if Result < 10 + EPSILON then
            Result := Result - 0.001
          else if Result < 100 + EPSILON then
            Result := Result - 0.01
          else if Result < 1000 + EPSILON then
            Result := Result - 0.1
          else if Result < 5000 + EPSILON then
            Result := Result - 1
          else if Result < 10000 + EPSILON then
            Result := Result - 5
          else if Result < 50000 + EPSILON then
            Result := Result - 10
          else if Result < 100000 + EPSILON then
            Result := Result - 50
          else if Result < 500000 + EPSILON then
            Result := Result - 100
          else if Result < 1000000 + EPSILON then
            Result := Result - 500
          else
            Result := Result - 1000;
        end;
      end;
  end;

end;

function LocksFromPrice(aSymbol: TSymbol; dPrice: Double; iTicks: Integer): Double;
begin

end;

end.
