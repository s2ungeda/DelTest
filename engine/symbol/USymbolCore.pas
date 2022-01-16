unit USymbolCore;

interface

uses
  system.Classes, system.SysUtils ,

  UFQN, UMarketSpecs, USymbols, UMarkets,

  UApiConsts
  ;

type

  TSymbolArray  = array [0..ExCnt-1] of TSymbolList;
  TSpotArray    = array [0..ExCnt-1] of TSpots;
  TMarginArray  = array [0..ExCnt-1] of TMargins;
  TFutureArray  = array [0..ExCnt-1] of TFutures;

  TMarketArray        = array [0..ExCnt-1] of TMarketList;
  TSpotMarketArray    = array [0..ExCnt-1] of TSpotMarkets;
  TMarginMarketArray  = array [0..ExCnt-1] of TMarginMarkets;
  TFutureMarketArray  = array [0..ExCnt-1] of TFutureMarkets;

  TSymbolCore = class
  private

    FSymbols: TSymbolArray;
    FSpots: TSpotArray;
    FMargins: TMarginArray;
    FFutures: TFutureArray;

    FMarkets: TMarketArray;
    FSpotMarkets: TSpotMarketArray;
    FMarginMarkets: TMarginMarketArray;
    FFutureMarkets: TFutureMarketArray;

    FSpecs: TMarketSpecs;
  public
    constructor Create;
    destructor Destroy; override;

    procedure RegisterSymbol(aSymbol: TSymbol);
    property Specs: TMarketSpecs read FSpecs;

    property Symbols: TSymbolArray read FSymbols;
    property Spots: TSpotArray read FSpots;
    property Margins: TMarginArray read FMargins;
    property Futures: TFutureArray read FFutures;

    property Markets: TMarketArray read FMarkets;
    property SpotMarkets: TSpotMarketArray read FSpotMarkets;
    property MarginMarkets: TMarginMarketArray read FMarginMarkets;
    property FutureMarkets: TFutureMarketArray read FFutureMarkets;
  end;

implementation

{ TSymbolCore }

constructor TSymbolCore.Create;
var
  I: Integer;
begin
  FSpecs:= TMarketSpecs.Create;

  for I := 0 to ExCnt-1 do
  begin
    FSymbols[i] := TSymbolList.Create;
    FSpots[i]   := TSpots.Create;
    FMargins[i] := TMargins.Create;
    FFutures[i] := TFutures.Create;

    FMarkets[i]       := TMarketList.Create;
    FSpotMarkets[i]   := TSpotMarkets.Create;
    FFutureMarkets[i] := TFutureMarkets.Create;
    FMarginMarkets[i] := TMarginMarkets.Create;
  end;

end;

destructor TSymbolCore.Destroy;
var
  i : integer;
begin

  for I := 0 to ExCnt-1 do
  begin
    FSymbols[i].Free;
    FSpots[i].Free;
    FMargins[i].Free;
    FFutures[i].Free;

    FMarkets[i].Free;
    FSpotMarkets[i].Free;
    FFutureMarkets[i].Free;
    FMarginMarkets[i].Free;
  end;

   FSpecs.Free;

  inherited;
end;

procedure TSymbolCore.RegisterSymbol(aSymbol: TSymbol);
begin

end;

end.
