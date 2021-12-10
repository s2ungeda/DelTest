unit USymbolCore;

interface

uses
  system.Classes, system.SysUtils ,

  UFQN, UMarketSpecs, USymbols, UMarkets
  ;

type

  TSymbolCore = class
  private
    FFutures: TFutures;
    FMarginMarkets: TMarginMarkets;
    FMarkets: TMarketList;
    FStockMarkets: TStockMarkets;
    FMargins: TMargins;
    FFutureMarkets: TFutureMarkets;
    FSymbols: TSymbolList;
    FStocks: TStocks;
    FSpecs: TMarketSpecs;
  public
    constructor Create;
    destructor Destroy; override;

    procedure RegisterSymbol(aSymbol: TSymbol);
    property Specs: TMarketSpecs read FSpecs;

    property Symbols: TSymbolList read FSymbols;
    property Stocks: TStocks read FStocks;
    property Margins: TMargins read FMargins;
    property Futures: TFutures read FFutures;

    property Markets: TMarketList read FMarkets;
    property StockMarkets: TStockMarkets read FStockMarkets;
    property MarginMarkets: TMarginMarkets read FMarginMarkets;
    property FutureMarkets: TFutureMarkets read FFutureMarkets;
  end;

implementation

{ TSymbolCore }

constructor TSymbolCore.Create;
begin
  FSpecs:= TMarketSpecs.Create;
  FSymbols:= TSymbolList.Create;

  FStocks:= TStocks.Create;
  FMargins:= TMargins.Create;
  FFutures:= TFutures.Create;

  FMarkets:= TMarketList.Create;
  FStockMarkets:= TStockMarkets.Create;
  FFutureMarkets:= TFutureMarkets.Create;
  FMarginMarkets:= TMarginMarkets.Create;
end;

destructor TSymbolCore.Destroy;
begin

  FMarkets.Free;
  FStockMarkets.Free;
  FFutureMarkets.Free;
  FMarginMarkets.Free;

  FSpecs.Free;
  FSymbols.Free;

  FStocks.Free;
  FMargins.Free;
  FFutures.Free;

  inherited;
end;

procedure TSymbolCore.RegisterSymbol(aSymbol: TSymbol);
begin

end;

end.
