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
  FJungKopi in 'wins\FJungKopi.pas' {FrmJungKopi},
  JOSE.Encoding.Base64 in 'libs\jwt\Source\Common\JOSE.Encoding.Base64.pas',
  JOSE.Hashing.HMAC in 'libs\jwt\Source\Common\JOSE.Hashing.HMAC.pas',
  JOSE.OpenSSL.Headers in 'libs\jwt\Source\Common\JOSE.OpenSSL.Headers.pas',
  JOSE.Signing.Base in 'libs\jwt\Source\Common\JOSE.Signing.Base.pas',
  JOSE.Signing.ECDSA in 'libs\jwt\Source\Common\JOSE.Signing.ECDSA.pas',
  JOSE.Signing.RSA in 'libs\jwt\Source\Common\JOSE.Signing.RSA.pas',
  JOSE.Types.Arrays in 'libs\jwt\Source\Common\JOSE.Types.Arrays.pas',
  JOSE.Types.Bytes in 'libs\jwt\Source\Common\JOSE.Types.Bytes.pas',
  JOSE.Types.JSON in 'libs\jwt\Source\Common\JOSE.Types.JSON.pas',
  JOSE.Types.Utils in 'libs\jwt\Source\Common\JOSE.Types.Utils.pas',
  JOSE.Builder in 'libs\jwt\Source\JOSE\JOSE.Builder.pas',
  JOSE.Consumer in 'libs\jwt\Source\JOSE\JOSE.Consumer.pas',
  JOSE.Consumer.Validators in 'libs\jwt\Source\JOSE\JOSE.Consumer.Validators.pas',
  JOSE.Context in 'libs\jwt\Source\JOSE\JOSE.Context.pas',
  JOSE.Core.Base in 'libs\jwt\Source\JOSE\JOSE.Core.Base.pas',
  JOSE.Core.Builder in 'libs\jwt\Source\JOSE\JOSE.Core.Builder.pas',
  JOSE.Core.JWA.Compression in 'libs\jwt\Source\JOSE\JOSE.Core.JWA.Compression.pas',
  JOSE.Core.JWA.Encryption in 'libs\jwt\Source\JOSE\JOSE.Core.JWA.Encryption.pas',
  JOSE.Core.JWA.Factory in 'libs\jwt\Source\JOSE\JOSE.Core.JWA.Factory.pas',
  JOSE.Core.JWA in 'libs\jwt\Source\JOSE\JOSE.Core.JWA.pas',
  JOSE.Core.JWA.Signing in 'libs\jwt\Source\JOSE\JOSE.Core.JWA.Signing.pas',
  JOSE.Core.JWE in 'libs\jwt\Source\JOSE\JOSE.Core.JWE.pas',
  JOSE.Core.JWK in 'libs\jwt\Source\JOSE\JOSE.Core.JWK.pas',
  JOSE.Core.JWS in 'libs\jwt\Source\JOSE\JOSE.Core.JWS.pas',
  JOSE.Core.JWT in 'libs\jwt\Source\JOSE\JOSE.Core.JWT.pas',
  JOSE.Core.Parts in 'libs\jwt\Source\JOSE\JOSE.Core.Parts.pas',
  FDnwStates in 'wins\FDnwStates.pas' {FrmDnwStates},
  USymbolBroker in 'engine\symbol\USymbolBroker.pas',
  FServerMessage in 'wins\FServerMessage.pas' {FrmServerMessage},
  FQuoteMonitors in 'wins\FQuoteMonitors.pas' {FrmQuoteMonitors},
  USymbolUtils in 'engine\symbol\USymbolUtils.pas',
  URestRequests in 'api\URestRequests.pas',
  FWinConfig in 'FWinConfig.pas' {FrmWinConfig};

{$R *.res}
begin

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFrmDalinMain, FrmDalinMain);
  Application.CreateForm(TDataModule1, DataModule1);
  Application.CreateForm(TFrmWinConfig, FrmWinConfig);
  FrmDalinMain.Start;
  Application.Run;

end.
