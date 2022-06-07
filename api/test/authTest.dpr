program authTest;

uses
  Vcl.Forms,
  Unit3 in 'Unit3.pas' {Form3},
  UEncrpyts in 'UEncrpyts.pas',
  JOSE.Encoding.Base64 in 'delphi-jose-jwt-master\Source\Common\JOSE.Encoding.Base64.pas',
  JOSE.Hashing.HMAC in 'delphi-jose-jwt-master\Source\Common\JOSE.Hashing.HMAC.pas',
  JOSE.OpenSSL.Headers in 'delphi-jose-jwt-master\Source\Common\JOSE.OpenSSL.Headers.pas',
  JOSE.Signing.Base in 'delphi-jose-jwt-master\Source\Common\JOSE.Signing.Base.pas',
  JOSE.Signing.ECDSA in 'delphi-jose-jwt-master\Source\Common\JOSE.Signing.ECDSA.pas',
  JOSE.Signing.RSA in 'delphi-jose-jwt-master\Source\Common\JOSE.Signing.RSA.pas',
  JOSE.Types.Arrays in 'delphi-jose-jwt-master\Source\Common\JOSE.Types.Arrays.pas',
  JOSE.Types.Bytes in 'delphi-jose-jwt-master\Source\Common\JOSE.Types.Bytes.pas',
  JOSE.Types.JSON in 'delphi-jose-jwt-master\Source\Common\JOSE.Types.JSON.pas',
  JOSE.Types.Utils in 'delphi-jose-jwt-master\Source\Common\JOSE.Types.Utils.pas',
  JOSE.Builder in 'delphi-jose-jwt-master\Source\JOSE\JOSE.Builder.pas',
  JOSE.Consumer in 'delphi-jose-jwt-master\Source\JOSE\JOSE.Consumer.pas',
  JOSE.Consumer.Validators in 'delphi-jose-jwt-master\Source\JOSE\JOSE.Consumer.Validators.pas',
  JOSE.Context in 'delphi-jose-jwt-master\Source\JOSE\JOSE.Context.pas',
  JOSE.Core.Base in 'delphi-jose-jwt-master\Source\JOSE\JOSE.Core.Base.pas',
  JOSE.Core.Builder in 'delphi-jose-jwt-master\Source\JOSE\JOSE.Core.Builder.pas',
  JOSE.Core.JWA.Compression in 'delphi-jose-jwt-master\Source\JOSE\JOSE.Core.JWA.Compression.pas',
  JOSE.Core.JWA.Encryption in 'delphi-jose-jwt-master\Source\JOSE\JOSE.Core.JWA.Encryption.pas',
  JOSE.Core.JWA.Factory in 'delphi-jose-jwt-master\Source\JOSE\JOSE.Core.JWA.Factory.pas',
  JOSE.Core.JWA in 'delphi-jose-jwt-master\Source\JOSE\JOSE.Core.JWA.pas',
  JOSE.Core.JWA.Signing in 'delphi-jose-jwt-master\Source\JOSE\JOSE.Core.JWA.Signing.pas',
  JOSE.Core.JWE in 'delphi-jose-jwt-master\Source\JOSE\JOSE.Core.JWE.pas',
  JOSE.Core.JWK in 'delphi-jose-jwt-master\Source\JOSE\JOSE.Core.JWK.pas',
  JOSE.Core.JWS in 'delphi-jose-jwt-master\Source\JOSE\JOSE.Core.JWS.pas',
  JOSE.Core.JWT in 'delphi-jose-jwt-master\Source\JOSE\JOSE.Core.JWT.pas',
  JOSE.Core.Parts in 'delphi-jose-jwt-master\Source\JOSE\JOSE.Core.Parts.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm3, Form3);
  Application.Run;
end.
