unit UEncrpyts;

interface

uses
  System.SysUtils,
  IdSSLOpenSSL, IdHashSHA,  IdURI,
  IdGlobal, IdHMAC, IdHMACSHA1;

  function GetSHA256Str(const str : String) : String;
  function CalculateHMACSHA256(const value, salt: String): String;
  function CalculateHMACSHA512(const value, salt: String): String;
  function EncodeURL(const value: String): String;

implementation


function GetSHA256Str(const str : String) : String;
begin
  IdSSLOpenSSL.LoadOpenSSLLibrary;

  with TIdHashSHA256.Create do
    try
      Result := LowerCase( HashStringAsHex(str) );
    finally
      Free;
  end;

  IdSSLOpenSSL.UnLoadOpenSSLLibrary;
end;


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
    Result := LowerCase(ToHex(hash));
  finally
    hmac.Free;
  end;
end;

 function CalculateHMACSHA512(const value, salt: String): String;
var
  hmac: TIdHMACSHA512;
  hash: TIdBytes;
begin
  LoadOpenSSLLibrary;
  if not TIdHashSHA512.IsAvailable then
    raise Exception.Create('SHA512 hashing is not available!');
  hmac := TIdHMACSHA512.Create;
  try
    hmac.Key := IndyTextEncoding_UTF8.GetBytes(salt);
    hash := hmac.HashValue(IndyTextEncoding_UTF8.GetBytes(value));
    Result := LowerCase(ToHex(hash));

//    hmac.Key := ToBytes(salt);
//    result := LowerCase(ToHex(hmac.HashValue(ToBytes(value))));
  finally
    hmac.Free;
  end;
//var
//  hash : TIdHashSHA512;
//  Return : String;
//begin
//
//  if IdSSLOpenSSL.LoadOpenSSLLibrary then
//  begin
//    hash := TIdHashSHA512.Create;
//    try
//
//      Return := hash.HashStringAsHex(value);
//      Result := Return;
//    finally
//      hash.Free;
//    end;
//  end;
 end;

 function EncodeURL(const value: String): String;
 begin
   result := TIdURI.URLEncode( value)
 end;

end.
