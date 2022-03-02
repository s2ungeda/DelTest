program Dalin;
uses
  Vcl.Forms,
  GApp in 'main\GApp.pas',
  UDalinEngine in 'engine\UDalinEngine.pas',
  GLibs in 'engine\common\GLibs.pas',
  UConfig in 'engine\common\UConfig.pas',
  UTypes in 'engine\common\UTypes.pas',
  ULogThread in 'engine\utils\ULogThread.pas',
  UParsers in 'engine\utils\UParsers.pas',
  UStorage in 'engine\utils\UStorage.pas',
  UFormBroker in 'engine\utils\UFormBroker.pas',
  GAppForms in 'main\GAppForms.pas',
  FPriceTable in 'wins\FPriceTable.pas' {FrmPriceTable},
  UTableConsts in 'wins\_common\UTableConsts.pas',
  DalinMain in 'main\DalinMain.pas' {FrmDalinMain},
  NMainMenu in 'main\NMainMenu.pas' {DataModule1: TDataModule},
  UDalinStatusEvent in 'main\UDalinStatusEvent.pas',
  UFQN in 'engine\symbol\UFQN.pas',
  USymbols in 'engine\symbol\USymbols.pas',
  UMarketSpecs in 'engine\symbol\UMarketSpecs.pas',
  UCollections in 'engine\utils\UCollections.pas',
  USymbolParser in 'engine\symbol\USymbolParser.pas',
  UMarkets in 'engine\symbol\UMarkets.pas',
  UConsts in 'engine\common\UConsts.pas',
  UApiConsts in 'api\UApiConsts.pas',
  UApiConfigManager in 'api\UApiConfigManager.pas',
  USymbolCore in 'engine\symbol\USymbolCore.pas',
  UExchange in 'api\UExchange.pas',
  UApiTypes in 'api\UApiTypes.pas',
  UApiManager in 'api\UApiManager.pas',
  UExchangeManager in 'api\UExchangeManager.pas',
  UBinanceManager in 'api\binance\UBinanceManager.pas',
  UUpbitManager in 'api\upbit\UUpbitManager.pas',
  UBithManager in 'api\bithumb\UBithManager.pas',
  UBinanceSpotNMargin in 'api\binance\UBinanceSpotNMargin.pas',
  UBinanceFutures in 'api\binance\UBinanceFutures.pas',
  UUpbitSpot in 'api\upbit\UUpbitSpot.pas',
  UBithSpot in 'api\bithumb\UBithSpot.pas',
  UEncrypts in 'engine\common\UEncrypts.pas',
  UBinanceParse in 'api\Binance\UBinanceParse.pas',
  UTicks in 'engine\symbol\UTicks.pas',
  UCircularQueue in 'engine\utils\UCircularQueue.pas',
  UQuoteTimers in 'engine\utils\UQuoteTimers.pas',
  UBithParse in 'api\bithumb\UBithParse.pas',
  UUpbitParse in 'api\upbit\UUpbitParse.pas',
  UQuoteBroker in 'engine\symbol\UQuoteBroker.pas',
  UDistributor in 'engine\utils\UDistributor.pas',
  UExchangeRate in 'api\exrate\UExchangeRate.pas',
  UWebSockets in 'api\UWebSockets.pas',
  UBinanceWebSockets in 'api\Binance\UBinanceWebSockets.pas',
  UBithWebSockets in 'api\bithumb\UBithWebSockets.pas',
  UUpbitWebSockets in 'api\upbit\UUpbitWebSockets.pas',
  FJungKopi in 'wins\FJungKopi.pas' {FrmJungKopi};

{$R *.res}
begin

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFrmDalinMain, FrmDalin);
  Application.CreateForm(TDataModule1, DataModule1);
  FrmDalin.Start;
  Application.Run;

end.
