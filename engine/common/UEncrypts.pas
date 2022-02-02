unit UEncrypts;

interface

uses
  System.SysUtils,
  IdSSLOpenSSL, IdHashSHA,
  IdGlobal, IdHMAC, IdHMACSHA1
  ;


function CalculateHMACSHA256(const value, salt: String): String;

implementation

function CalculateHMACSHA256(const value, salt: String): String;
var
  hmac: TIdHMACSHA256;
  hash: TIdBytes;
begin
  LoadOpenSSLLibrary;
  if not TIdHashSHA256.IsAvailable then
    raise Exception.Create('SHA256 hashing is not available!');
  hmac := TIdHMACSHA256.Create;
  try
    hmac.Key := IndyTextEncoding_UTF8.GetBytes(salt);
    hash := hmac.HashValue(IndyTextEncoding_UTF8.GetBytes(value));
    Result := ToHex(hash);
  finally
    hmac.Free;
  end;
end;

end.
