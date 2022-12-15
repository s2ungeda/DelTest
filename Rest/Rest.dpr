program Rest;

uses
  Vcl.Forms,
  USharedThread in 'USharedThread.pas',
  FRestMain in 'FRestMain.pas' {FrmRestMain},
  USharedData in 'USharedData.pas',
  UApiConsts in '..\api\UApiConsts.pas',
  UApiTypes in '..\api\UApiTypes.pas',
  GLibs in '..\engine\common\GLibs.pas',
  UConsts in '..\engine\common\UConsts.pas',
  UTypes in '..\engine\common\UTypes.pas',
  ULogThread in '..\engine\utils\ULogThread.pas',
  GApp in 'GApp.pas',
  UApiConfigManager in '..\api\UApiConfigManager.pas',
  UConfig in '..\engine\common\UConfig.pas',
  JOSE.Encoding.Base64 in '..\libs\jwt\Source\Common\JOSE.Encoding.Base64.pas',
  JOSE.Hashing.HMAC in '..\libs\jwt\Source\Common\JOSE.Hashing.HMAC.pas',
  JOSE.OpenSSL.Headers in '..\libs\jwt\Source\Common\JOSE.OpenSSL.Headers.pas',
  JOSE.Signing.Base in '..\libs\jwt\Source\Common\JOSE.Signing.Base.pas',
  JOSE.Signing.ECDSA in '..\libs\jwt\Source\Common\JOSE.Signing.ECDSA.pas',
  JOSE.Signing.RSA in '..\libs\jwt\Source\Common\JOSE.Signing.RSA.pas',
  JOSE.Types.Arrays in '..\libs\jwt\Source\Common\JOSE.Types.Arrays.pas',
  JOSE.Types.Bytes in '..\libs\jwt\Source\Common\JOSE.Types.Bytes.pas',
  JOSE.Types.JSON in '..\libs\jwt\Source\Common\JOSE.Types.JSON.pas',
  JOSE.Types.Utils in '..\libs\jwt\Source\Common\JOSE.Types.Utils.pas',
  JOSE.Builder in '..\libs\jwt\Source\JOSE\JOSE.Builder.pas',
  JOSE.Consumer in '..\libs\jwt\Source\JOSE\JOSE.Consumer.pas',
  JOSE.Consumer.Validators in '..\libs\jwt\Source\JOSE\JOSE.Consumer.Validators.pas',
  JOSE.Context in '..\libs\jwt\Source\JOSE\JOSE.Context.pas',
  JOSE.Core.Base in '..\libs\jwt\Source\JOSE\JOSE.Core.Base.pas',
  JOSE.Core.Builder in '..\libs\jwt\Source\JOSE\JOSE.Core.Builder.pas',
  JOSE.Core.JWA.Compression in '..\libs\jwt\Source\JOSE\JOSE.Core.JWA.Compression.pas',
  JOSE.Core.JWA.Encryption in '..\libs\jwt\Source\JOSE\JOSE.Core.JWA.Encryption.pas',
  JOSE.Core.JWA.Factory in '..\libs\jwt\Source\JOSE\JOSE.Core.JWA.Factory.pas',
  JOSE.Core.JWA in '..\libs\jwt\Source\JOSE\JOSE.Core.JWA.pas',
  JOSE.Core.JWA.Signing in '..\libs\jwt\Source\JOSE\JOSE.Core.JWA.Signing.pas',
  JOSE.Core.JWE in '..\libs\jwt\Source\JOSE\JOSE.Core.JWE.pas',
  JOSE.Core.JWK in '..\libs\jwt\Source\JOSE\JOSE.Core.JWK.pas',
  JOSE.Core.JWS in '..\libs\jwt\Source\JOSE\JOSE.Core.JWS.pas',
  JOSE.Core.JWT in '..\libs\jwt\Source\JOSE\JOSE.Core.JWT.pas',
  JOSE.Core.Parts in '..\libs\jwt\Source\JOSE\JOSE.Core.Parts.pas',
  UEncrypts in '..\engine\common\UEncrypts.pas',
  URestManager in 'URestManager.pas',
  USharedConsts in 'USharedConsts.pas',
  UBinFutRequests in 'UBinFutRequests.pas',
  IRestRequests in 'IRestRequests.pas',
  UBinSpotRequests in 'UBinSpotRequests.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFrmRestMain, FrmRestMain);
  Application.Run;
end.
