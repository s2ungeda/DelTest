unit Unit3;
interface
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.DateUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, REST.Types, Data.Bind.Components,
  Data.Bind.ObjectScope, REST.Client, Vcl.StdCtrls, System.Rtti,
  System.Bindings.Outputs, Vcl.Bind.Editors, Data.Bind.EngExt, Vcl.Bind.DBEngExt;
const
  sSecKey = '6e77f2451a4406fa12c2b8aca4f68aad61fc3acf2f48ae529c71f8a7386d2b16';
  sApiKey = 'ad318a99903cd611c5b6431f657fffdfa7e810bd2e0acfad6c1f876ceb67db2a';
type
  TDecimalHelper = record
    OrgVal : string;
    Jungsu : string;
    Sosu   : string;
    Precision : integer;
    Multiple  : int64;
    ConVal    : int64;
    constructor Create(sval : string );
    function print : string;
    procedure convert(sval:string);
    function ToDouble : double;
    function ToInt64  : int64;
  end;
  TForm3 = class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    Memo1: TMemo;
    restClient: TRESTClient;
    restReq: TRESTRequest;
    restRes: TRESTResponse;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Edit2: TEdit;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    Button10: TButton;
    Button11: TButton;
    Button12: TButton;
    Button13: TButton;
    Button14: TButton;
    Button15: TButton;
    Button16: TButton;
    Button17: TButton;
    edtUid: TEdit;
    Button18: TButton;
    Button19: TButton;
    Button20: TButton;
    Edit3: TEdit;
    Edit4: TEdit;
    Button21: TButton;
    Button22: TButton;
    Button23: TButton;
    Button24: TButton;
    Button25: TButton;
    상세: TButton;
    Button26: TButton;
    최근거래: TButton;
    procedure Button1Click(Sender: TObject);
    procedure restReqAfterExecute(Sender: TCustomRESTRequest);
    procedure restReqHTTPProtocolError(Sender: TCustomRESTRequest);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure Button10Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure Button12Click(Sender: TObject);
    procedure Button13Click(Sender: TObject);
    procedure Button14Click(Sender: TObject);
    procedure Button15Click(Sender: TObject);
    procedure Button16Click(Sender: TObject);
    procedure Button17Click(Sender: TObject);
    procedure Button18Click(Sender: TObject);
    procedure Button19Click(Sender: TObject);
    procedure Button20Click(Sender: TObject);
    procedure Edit4KeyPress(Sender: TObject; var Key: Char);
    procedure Button21Click(Sender: TObject);
    procedure Button22Click(Sender: TObject);
    procedure Button23Click(Sender: TObject);
    procedure Button24Click(Sender: TObject);
    procedure Button25Click(Sender: TObject);
    procedure 상세Click(Sender: TObject);
    procedure Button26Click(Sender: TObject);
    procedure 최근거래Click(Sender: TObject);
  private
    procedure DoLog(sData: string);
    procedure LogFileWrite( stData: string);
    procedure testparse;
    procedure test;
    { Private declarations }
  public
    { Public declarations }
  end;
var
  Form3: TForm3;
function TzSpecificLocalTimeToSystemTime(lpTimeZoneInformation: PTimeZoneInformation; var lpLocalTime, lpUniversalTime: TSystemTime): BOOL; stdcall;
function SystemTimeToTzSpecificLocalTime(lpTimeZoneInformation: PTimeZoneInformation; var lpUniversalTime,lpLocalTime: TSystemTime): BOOL; stdcall;
function GetUUID : string;
implementation


uses
  system.JSON  ,
  JOSE.Core.JWT,
  JOSE.Core.JWK,
  JOSE.Core.JWS,
  JOSE.Core.JWA,
  JOSE.Core.Builder,
  JOSE.Types.JSON,
  JOSE.Encoding.Base64,
  System.NetEncoding,
  IdGlobal,
   idcodermime,
  EncdDecd,
  Web.HTTPApp,
  IdMultipartFormData,
  System.Hash,
  Uencrpyts  ,
  System.Threading,
  Math
  ;
{$R *.dfm}
function TzSpecificLocalTimeToSystemTime; external kernel32 name 'TzSpecificLocalTimeToSystemTime';
function SystemTimeToTzSpecificLocalTime; external kernel32 name 'SystemTimeToTzSpecificLocalTime';

function GetUUID : string;
var
  guid : TGUID;
  sData: string;
begin
  CreateGUID(guid);
  sData  := GUIDToString(guid);
  Result := Copy( sData, 2, Length( sData) - 2);
end;
Function DateTime2UnivDateTime(d:TDateTime):TDateTime;
var
 TZI:TTimeZoneInformation;
 LocalTime, UniversalTime:TSystemTime;
begin
  GetTimeZoneInformation(tzi);
  DateTimeToSystemTime(d,LocalTime);
  TzSpecificLocalTimeToSystemTime(@tzi,LocalTime,UniversalTime);
  Result := SystemTimeToDateTime(UniversalTime);
end;
Function UnivDateTime2LocalDateTime(d:TDateTime):TDateTime;
var
 TZI:TTimeZoneInformation;
 LocalTime, UniversalTime:TSystemTime;
begin
  GetTimeZoneInformation(tzi);
  DateTimeToSystemTime(d,UniversalTime);
  SystemTimeToTzSpecificLocalTime(@tzi,UniversalTime,LocalTime);
  Result := SystemTimeToDateTime(LocalTime);
end;

function Gettamptime(vlen: Integer): string;
var
timen, time2: TDateTime;
ss2, ss3: Int64;
begin
timen := now;
time2 := EncodeDateTime(1970, 1, 1, 0, 0, 0, 0);
ss2 := 28800000;
ss3 := MilliSecondsBetween(timen, time2);
ss3 := ss3 - ss2;
Result := InttoStr(ss3);
if vlen = 13 then
Result := Result
else if vlen = 10 then
Result := Copy(Result, 1, 10);
end;
////////////////Standard correct //////////////////////
function Gettamptime2(vlen: Integer): string;
var
ss: string;
begin
if vlen = 13 then
begin
ss := DateTimeToTimeStamp(now).time .ToString;
Result := IntToStr(DateTimeToUnix(Now,false)) + Copy(ss,Length(ss) - 2,Length(ss) );
end
else if vlen = 10 then
begin
Result := IntToStr(DateTimeToUnix(Now,false));
end
end;
function gettamptotime(vtamp: string): string;
//1582688206607
var
ls10,lms: string;
begin
if Length(vtamp) = 10 then
Result := FormatDateTime('yyyy-MM-dd hh:mm:ss',UnixToDateTime(StrToInt64(vtamp),false))
else if Length(vtamp) = 13 then
begin
ls10 := Copy(vtamp,1,10);
lms := Copy(vtamp,11,13);
Result := FormatDateTime('yyyy-MM-dd hh:mm:ss',UnixToDateTime(StrToInt64(ls10),false));
Result := Result +'.' + lms;
end;
end;

function EncodeURIComponent(const ASrc: string): string;
const
  HexMap: string = '0123456789ABCDEF';
  function IsSafeChar(ch: Byte): Boolean;
  begin
    if (ch >= 48) and (ch <= 57) then Result := True    // 0-9
    else if (ch >= 65) and (ch <= 90) then Result := True  // A-Z
    else if (ch >= 97) and (ch <= 122) then Result := True  // a-z
    else if (ch = 33) then Result := True // !
    else if (ch >= 39) and (ch <= 42) then Result := True // '()*
    else if (ch >= 45) and (ch <= 46) then Result := True // -.
    else if (ch = 95) then Result := True // _
    else if (ch = 126) then Result := True // ~
    else Result := False;
  end;
var
  I, J: Integer;
  Bytes: TBytes;
begin
  Result := '';
  Bytes := TEncoding.UTF8.GetBytes(ASrc);
  I := 0;
  J := Low(Result);
  SetLength(Result, Length(Bytes) * 3); // space to %xx encode every byte
  while I < Length(Bytes) do
  begin
    if IsSafeChar(Bytes[I]) then
    begin
      Result[J] := Char(Bytes[I]);
      Inc(J);
    end
    else
    begin
      Result[J] := '%';
      Result[J+1] := HexMap[(Bytes[I] shr 4) + Low(ASrc)];
      Result[J+2] := HexMap[(Bytes[I] and 15) + Low(ASrc)];
      Inc(J,3);
    end;
    Inc(I);
  end;
  SetLength(Result, J-Low(ASrc));
end;
procedure TForm3.test;
begin
  memo1.Lines.Add('asdfad');
end;


procedure TForm3.Button1Click(Sender: TObject);
var
  stUri : string;
  req : TRESTRequest;
  con : TRestClient;
  res : TRestResponse;
begin
  req := TRestRequest.Create(nil);
  con := TRestClient.Create( 'https://api.bithumb.com');
  res := TRestResponse.Create( req );
  req.Method := rmGET;
  req.Resource := '/public/assetsstatus/BTC';
  req.ExecuteAsync(
    procedure
    begin
        memo1.Lines.Add('asdfad');
        req.Free;
        if con <> nil then con.free;
    end);

//  if res <> nil then res.Free;

end;


procedure TForm3.Button2Click(Sender: TObject);
begin

  var dd : int64;
  dd := DateTimeToMilliseconds(now) ;
  dd := Round((now - 25569) * 86400 * 1000.0);
  memo1.Lines.Add( Format('%u , %s, %s', [ dd, Gettamptime(13), Gettamptime2(10 )  ] ));
//  memo1.Lines.Add( Format('1638104275173, %d, %u', [ DateTimeToUnix( now, false ) , dd] ));

  exit;

  restReq.Method  := rmPOST;
  with restReq do
  begin
    Method   := rmPOST;
    Resource := '/info/balance';
    AddParameter('apiKey', '5a3d078609ef394c8ec5487bb66b282a', pkHTTPHEADER);
    AddParameter('secretKey', 'ab16921bf03ce7f32a0f3a7fdf6acabc', pkHTTPHEADER);
//    AddParameter('currency', 'BTC');
  end;

  restReq.Execute;
  memo1.Lines.Add( restRes.JSONValue.ToString );
end;
procedure TForm3.Button3Click(Sender: TObject);
var
  t,sig, data, key : string;
begin
  key := 'ZGzriTauLgOYQ35aNXlKJWFHf07CLxBLZ0i3yeIW7Fiht9gTJanPLGBRrp5FzFKf';
  //memo1.Lines.Add(CalculateHMACSHA256(data,key ) );
  restClient.BaseURL := 'https://api.binance.com';
  restReq.Resource := '/sapi/v1/margin/isolated/allPairs';
//  restReq.Resource := '/sapi/v1/margin/isolated/pair';
  t :=  Gettamptime2(13 );
  data := Format('timestamp=%s', [t]);
  sig  := CalculateHMACSHA256(data,key );
  memo1.Lines.Add( sig );
//  restReq.AddParameter('symbol', 'BTCUSDT' );
  restReq.AddParameter('timestamp', t );
  restReq.AddParameter('signature', sig );
  restReq.AddParameter('X-MBX-APIKEY', 'bzJPEfytBMVlyJNGBKmuJLuJGHJnpQ28lUhyaDOudMS9ZPPWfalu4hFb0HVt798H', pkHTTPHEADER );
  restReq.Method   := rmGET;
  restReq.Execute;
  memo1.Lines.Add( restRes.JSONValue.ToString );
end;
procedure TForm3.Button12Click(Sender: TObject);
var
  t,sig, data : string;
begin

  restClient.BaseURL := 'https://testnet.binancefuture.com';
  restReq.Resource := '/fapi/v2/balance';
  t :=  Gettamptime2(13 );
  data := Format('timestamp=%s', [t]);
  sig  := CalculateHMACSHA256(data,sSeckey );
//  restReq.AddParameter('symbol', 'BTCUSDT' );
	restReq.Params.Clear;
  
  restReq.AddParameter('timestamp', t );
  restReq.AddParameter('signature', sig );  
  restReq.AddParameter('X-MBX-APIKEY', sApiKey, pkHTTPHEADER );

  restReq.Method   := rmGET;     
  restReq.Execute;
  memo1.Lines.Add( restRes.JSONValue.ToString );

end;
procedure TForm3.Button13Click(Sender: TObject);
var
  t,sig, data : string;
begin
  restClient.BaseURL := 'https://testnet.binancefuture.com';
  restReq.Resource := '/fapi/v2/positionRisk';
  t :=  Gettamptime2(13 );
  data := Format('symbol=BTCUSDT&timestamp=%s', [t]);
  sig  := CalculateHMACSHA256(data,sSeckey );
  restReq.Params.Clear;
  restReq.AddParameter('symbol', 'BTCUSDT' );
  restReq.AddParameter('timestamp', t );
  restReq.AddParameter('signature', sig );
  restReq.AddParameter('X-MBX-APIKEY', sApiKey, pkHTTPHEADER );

  restReq.Method   := rmGET;     
  restReq.Execute;
  memo1.Lines.Add( restRes.JSONValue.ToString );
end;

procedure TForm3.Button19Click(Sender: TObject);
var
  sCode, sSide, sType, sQty, sPrice, sClientID  : string ;
  sData, sTime, sSig : string;
begin
//


  sCode := 'BTCUSDT';
  sSide := 'SELL';
  sType := 'LIMIT';
  sQty  := '0.001';
  sPrice:= '12345608';

  sTime := Gettamptime2(13 );

  sData := Format('symbol=%s&side=%s&type=%s&timeInForce=GTC&quantity=%s&price=%s&newOrderRespType=RESULT&timestamp=%s',
    [ sCode, sSide, sType, sQty, sPrice, sTime ]);

  sSig  := CalculateHMACSHA256(sData,sSecKey );

  sData := sData + Format('&signature=%s', [ sSig ]);

  restReq.Params.Clear;

  restReq.AddParameter('X-MBX-APIKEY', sApiKey, pkHTTPHEADER );

  restClient.BaseURL := 'https://testnet.binancefuture.com';
  restReq.Resource := '/fapi/v1/order?'+sData;

  restReq.Method := rmPOST;


  restReq.Execute;

  memo1.Lines.Add( restRes.JSONValue.ToString );
end;
procedure TForm3.Button20Click(Sender: TObject);
var
  sTime, sSig, sData, sKey : string;
begin

  sTime := Gettamptime2(13 );

  sData := Format('symbol=%s&orderId=%s&timestamp=%s',
    [ 'BTCUSDT', edit3.Text, sTime ]);

  sSig  := CalculateHMACSHA256(sData,sSecKey );

  sData := sData + Format('&signature=%s', [ sSig ]);

  restReq.AddParameter('X-MBX-APIKEY', sApiKey, pkHTTPHEADER );

  restClient.BaseURL := 'https://testnet.binancefuture.com';
  restReq.Resource := '/fapi/v1/order?'+sData;

  restReq.Method := rmDELETE;

  restReq.Execute;

  memo1.Lines.Add( restRes.JSONValue.ToString );
end;
procedure TForm3.Button21Click(Sender: TObject);
var
  s : TArray<string>;
  sTmp, sTmp2, sSosu, sjung : string;
  iLen, i, j, iCnt, iPre : integer;
  iPow, iCon : int64;
  dval : double;

  dh : TDecimalHelper;
begin


  dh.OrgVal := edit4.Text;// := TDecimalHelper.Create(edit4.Text);
  dh.convert( edit4.Text );

  doLog( dh.print );
  
  try

  finally
//    dh.Free;
  end;

  exit;

  // 2번
  sTmp := edit4.Text;
  s    := sTmp.Split(['.']);
  iLen := High(s);





  //  1번
  sTmp := edit4.Text;
  s    := sTmp.Split(['.']);
  iLen := High(s);
//  sTmp2:= sTmp.Replace('.','');
//  DoLog( Format('%d : %s %s ', [ iLen, sTmp, sTmp2   ]));

  sSosu:= '';    iPow := 1;    iPre := 0;
  if iLen >= 1 then  begin
    sjung := s[0];
    sSosu := s[1];
    iCnt := 0;

    for j := Length(sSosu) downto 1 do
    begin
      if sSosu[j] <> '0' then     break;
      if sSosu[j] = '0' then      inc( iCnt);
    end;

    iPre  := length(s[1]) - iCnt;
    if iCnt > 0 then
      sSosu := Copy( s[1], 1, iPre );

    iPow := 1;
    for I := 0 to iPre-1 do  iPow := iPow*10;

//    iPow := Round( Power( 10, iPre ) );
//
//    iPow := Power( 10, iPre );
  end else
    sjung := sTmp;

  iCon  :=  StrToInt64(sJung + sSosu);
  dval  :=  iCon/iPow;
  // 원본, 정수,  정수부, 소수부, 승수..
  DoLog(
    format('%s -> %s ( %d ), %s , %s, %d, %d, %.*f', [ sTmp,  sJung + sSosu, iCon ,
       sJung, sSosu, iPre, iPow,  iPre, dval

     ] )
    );


//  if iLen >= 1 then
//  begin
//    iPow :=  Round(Power(10,  iPre ));
//    DoLog( Format('%d %d', [ iPre, iPow ])  );
//  end;


end;



procedure TForm3.Button4Click(Sender: TObject);
var
  t,sig, data, key : string;
begin

  restClient.BaseURL := 'https://api.binance.com';
  restReq.Resource := '/sapi/v1/asset/assetDetail';
  t :=  Gettamptime2(13 );
  data := Format('timestamp=%s', [t]);
  sig  := CalculateHMACSHA256(data,key );
//  restReq.AddParameter('symbol', 'BTCUSDT' );
  restReq.AddParameter('timestamp', t );
  restReq.AddParameter('signature', sig );
//  restReq.AddParameter('request', 'assetDetail', pkHTTPHEADER);
  restReq.AddParameter('X-MBX-APIKEY', 'bzJPEfytBMVlyJNGBKmuJLuJGHJnpQ28lUhyaDOudMS9ZPPWfalu4hFb0HVt798H', pkHTTPHEADER );
  restReq.AddParameter('cookie', 'dnwstate', pkCOOKIE );
  restReq.Method   := rmGET;
//  restReq.Client.SetCookie('dnwstate', restClient.BaseURL);
  restReq.Execute;
  memo1.Lines.Add(  '1. threadid : ' + intTostr( GetCurrentThreadid ) );
  if ( restReq.ExecuteAsync( testparse ) = nil ) then
    memo1.Lines.Add( 'failed request async');
//  memo1.Lines.Add( restRes.JSONValue.ToString );
end;

procedure TForm3.LogFileWrite( stData: string);
  function IsFileUse(fName: String): Boolean;
  var
    HFile: THandle;
  begin
    Result := false;
    if not FileExists(fName) then exit;
    HFile := CreateFile(PChar(fName), GENERIC_READ or GENERIC_WRITE, 0, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
    Result := (HFile = INVALID_HANDLE_VALUE);
    if not Result then begin
      try
        //Memo_Log.Lines.Add('Value = ' + IntToStr(HFile));
      finally
        CloseHandle(HFile);
      end;
    end;
  end;
var
  OutFile: TextFile;
  stDate, LogFileName: String;
begin
  stDate := FormatDateTime('YYYYMMDD',Date);
  LogFileName := stDate + '.log';
  try
    if not IsFileUse(LogFileName) then begin
    {$I-}
      AssignFile(OutFile, LogFileName);
      try
        if Not FileExists(LogFileName) then
          ReWrite(OutFile)
        else Append(OutFile);
        Writeln(OutFile,stData);
      finally
        CloseFile(OutFile);
      end;
    {$I+}
    end;
  Except
  end;
end;
procedure TForm3.DoLog( sData : string );
begin
  memo1.Lines.Add( sData );
 // LogFileWrite( sData );
end;
procedure TForm3.Edit4KeyPress(Sender: TObject; var Key: Char);
begin
  if not (Key in ['0'..'9','.',#8]) then
    Key := #0;
end;

procedure TForm3.FormCreate(Sender: TObject);
var
  iPre : integer;
begin
  caption := Format('%.*n', [ 3 , 0.8011 + 0.0] );
  //caption := Format('%.*n', [ 3, 0.8011 + 0.0 ] );
end;

procedure TForm3.Button11Click(Sender: TObject);
var
  sValue, sEncode, sig, sData, sTime,  sTmp, sContent : string;
begin
  restClient.BaseURL := 'https://api.bithumb.com';
  sTmp    := '/info/account';
  restReq.Resource := sTmp;
  sTime    := Gettamptime2(13 );
  sValue := HTTPEncode(UTF8Encode('endPoint=/info/account&order_currency=TRX&payment_currency=KRW'));
  sValue := StringReplace(sValue, '+', '%20', [rfReplaceAll]);
  sValue := StringReplace(sValue, '%21', '!', [rfReplaceAll]);
  sValue := StringReplace(sValue, '%27', '''', [rfReplaceAll]);
  sValue := StringReplace(sValue, '%28', '(', [rfReplaceAll]);
  sValue := StringReplace(sValue, '%29', ')', [rfReplaceAll]);
  sValue := StringReplace(sValue, '%26', '&', [rfReplaceAll]);
  sValue := StringReplace(sValue, '%3D', '=', [rfReplaceAll]);
  sValue := StringReplace(sValue, '%7E', '~', [rfReplaceAll]);
  sValue := '/info/account' +  chr(0) + sValue +  chr(0 ) + sTime;
  sTmp:= CalculateHMACSHA512( sValue, 'ab16921bf03ce7f32a0f3a7fdf6acabc');
  sig := TIdEncoderMIME.EncodeString( sTmp, IndyTextEncoding_UTF8 );
  restReq.Params.Clear;
  restReq.AddParameter('Api-Key', '5a3d078609ef394c8ec5487bb66b282a', TRESTRequestParameterKind.pkHTTPHEADER );//, [poDoNotEncode]);
  restReq.AddParameter('Api-Sign', sig , TRESTRequestParameterKind.pkHTTPHEADER , [poDoNotEncode]);
  restReq.AddParameter('Api-Nonce', sTime , TRESTRequestParameterKind.pkHTTPHEADER );
  restReq.AddParameter('endPoint', '/info/account', TRESTRequestParameterKind.pkREQUESTBODY);
  restReq.AddParameter('order_currency', 'TRX', TRESTRequestParameterKind.pkREQUESTBODY);
  restReq.AddParameter('payment_currency', 'KRW', TRESTRequestParameterKind.pkREQUESTBODY);
  restReq.Method   := rmPOST;
  restReq.Execute;
  memo1.Lines.Add( restRes.JSONValue.ToString );
end;

procedure TForm3.Button22Click(Sender: TObject);
var
  sValue, sEncode, sig, sData, sTime,  sTmp, sContent : string;
begin
  restClient.BaseURL := 'https://api.bithumb.com';
  sTmp    := '/trade/place';
  restReq.Resource := sTmp;
  sTime    := Gettamptime2(13 );
  sValue := HTTPEncode(UTF8Encode('endPoint=/trade/place&order_currency=TRX&payment_currency=KRW&units=6.2&price=81&type=bid'));
  sValue := StringReplace(sValue, '+', '%20', [rfReplaceAll]);
  sValue := StringReplace(sValue, '%21', '!', [rfReplaceAll]);
  sValue := StringReplace(sValue, '%27', '''', [rfReplaceAll]);
  sValue := StringReplace(sValue, '%28', '(', [rfReplaceAll]);
  sValue := StringReplace(sValue, '%29', ')', [rfReplaceAll]);
  sValue := StringReplace(sValue, '%26', '&', [rfReplaceAll]);
  sValue := StringReplace(sValue, '%3D', '=', [rfReplaceAll]);
  sValue := StringReplace(sValue, '%7E', '~', [rfReplaceAll]);
  sValue := '/trade/place' +  chr(0) + sValue +  chr(0 ) + sTime;
  sTmp:= CalculateHMACSHA512( sValue, 'ab16921bf03ce7f32a0f3a7fdf6acabc');
  sig := TIdEncoderMIME.EncodeString( sTmp, IndyTextEncoding_UTF8 );
  restReq.Params.Clear;
  restReq.AddParameter('Api-Key', '5a3d078609ef394c8ec5487bb66b282a', TRESTRequestParameterKind.pkHTTPHEADER );//, [poDoNotEncode]);
  restReq.AddParameter('Api-Sign', sig , TRESTRequestParameterKind.pkHTTPHEADER , [poDoNotEncode]);
  restReq.AddParameter('Api-Nonce', sTime , TRESTRequestParameterKind.pkHTTPHEADER );
  restReq.AddParameter('endPoint', '/trade/place', TRESTRequestParameterKind.pkREQUESTBODY);

  restReq.AddParameter('order_currency', 'TRX', TRESTRequestParameterKind.pkREQUESTBODY);
  restReq.AddParameter('payment_currency', 'KRW', TRESTRequestParameterKind.pkREQUESTBODY);
  restReq.AddParameter('units', '6.2', TRESTRequestParameterKind.pkREQUESTBODY);
  restReq.AddParameter('price', '81', TRESTRequestParameterKind.pkREQUESTBODY);
  restReq.AddParameter('type', 'bid', TRESTRequestParameterKind.pkREQUESTBODY);

  restReq.Method   := rmPOST;
  restReq.Execute;
  memo1.Lines.Add( restRes.JSONValue.ToString );
end;

procedure TForm3.Button23Click(Sender: TObject);
var
  sValue, sEncode, sig, sData, sTime,  sTmp, sContent : string;
begin
  restClient.BaseURL := 'https://api.bithumb.com';
  sTmp    := '/trade/cancel';
  restReq.Resource := sTmp;
  sTime    := Gettamptime2(13 );
  sValue := HTTPEncode(UTF8Encode('endPoint=/trade/cancel&order_currency=TRX&payment_currency=KRW&order_id='+edtUid.Text+'&type=bid'));
  sValue := StringReplace(sValue, '+', '%20', [rfReplaceAll]);
  sValue := StringReplace(sValue, '%21', '!', [rfReplaceAll]);
  sValue := StringReplace(sValue, '%27', '''', [rfReplaceAll]);
  sValue := StringReplace(sValue, '%28', '(', [rfReplaceAll]);
  sValue := StringReplace(sValue, '%29', ')', [rfReplaceAll]);
  sValue := StringReplace(sValue, '%26', '&', [rfReplaceAll]);
  sValue := StringReplace(sValue, '%3D', '=', [rfReplaceAll]);
  sValue := StringReplace(sValue, '%7E', '~', [rfReplaceAll]);
  sValue := '/trade/cancel' +  chr(0) + sValue +  chr(0 ) + sTime;
  sTmp:= CalculateHMACSHA512( sValue, 'ab16921bf03ce7f32a0f3a7fdf6acabc');
  sig := TIdEncoderMIME.EncodeString( sTmp, IndyTextEncoding_UTF8 );
  restReq.Params.Clear;
  restReq.AddParameter('Api-Key', '5a3d078609ef394c8ec5487bb66b282a', TRESTRequestParameterKind.pkHTTPHEADER );//, [poDoNotEncode]);
  restReq.AddParameter('Api-Sign', sig , TRESTRequestParameterKind.pkHTTPHEADER , [poDoNotEncode]);
  restReq.AddParameter('Api-Nonce', sTime , TRESTRequestParameterKind.pkHTTPHEADER );
  restReq.AddParameter('endPoint', '/trade/cancel', TRESTRequestParameterKind.pkREQUESTBODY);

  restReq.AddParameter('order_currency', 'TRX', TRESTRequestParameterKind.pkREQUESTBODY);
  restReq.AddParameter('payment_currency', 'KRW', TRESTRequestParameterKind.pkREQUESTBODY);
  restReq.AddParameter('order_id', edtUid.Text, TRESTRequestParameterKind.pkREQUESTBODY);
  restReq.AddParameter('type', 'bid', TRESTRequestParameterKind.pkREQUESTBODY);

  restReq.Method   := rmPOST;
  restReq.Execute;
  memo1.Lines.Add( restRes.JSONValue.ToString );

end;

procedure TForm3.Button24Click(Sender: TObject);
var
  stmp : string;
  sts : TArray<string>;
  i : integer;
begin
  stmp := edit3.Text;
  sts  := stmp.Split(['|']);
  for I := 0 to High(sts) do
    memo1.Lines.Add( Format('%d : %s', [i, sts[i] ] ));
end;

procedure TForm3.Button25Click(Sender: TObject);
var
	sTime : string;
  iTime : Int64;
  dtTime : TDateTime;

  Result, ls10, lms : string;
begin
	sTime := edtUID.Text;
  iTime := StrToINt64(sTime); 
  dtTime:= UnixToDateTime( iTime );
  memo1.Lines.Add( FormatDateTime('yyyy-MM-dd hh:nn:ss.zzz', dtTime ) );

  dtTime := (iTime / 86400) + 25569;
  memo1.Lines.Add( FormatDateTime('yyyy-MM-dd hh:nn:ss.zzz', dtTime ) );


  ls10 := Copy(sTime,1,10);
  lms := Copy(sTime,11,3);
  Result := FormatDateTime('yyyy-MM-dd hh:mm:ss',UnixToDateTime(StrToInt64(ls10),false));
  Result := Result +'.' + lms;  

  memo1.Lines.Add(Result );

  dtTime := ( StrToINt64( Copy(sTime,1,10) ) / 86400) + 25569;
  memo1.Lines.Add( FormatDateTime('yyyy-MM-dd hh:nn:ss.zzz', dtTime ) );  

  dtTime:= UnixToDateTime(iTime div 1000000 , false);
  memo1.Lines.Add( FormatDateTime('yyyy-MM-dd hh:nn:ss.zzz', dtTime ) );      
end;

procedure TForm3.최근거래Click(Sender: TObject);
var
  sCode, sValue, sEncode, sig, sData, sTime,  sTmp, sContent : string;
begin
      //
    	sCode := 'AMO'; //aCodes[i];
      restClient.BaseURL := 'https://api.bithumb.com';
      sTmp    := '/info/ticker';
      restReq.Resource := sTmp;
      sTime    := Gettamptime2(13 );
      sValue := HTTPEncode(UTF8Encode('endPoint=/info/ticker&order_currency='+sCode));
      sValue := StringReplace(sValue, '+', '%20', [rfReplaceAll]);
      sValue := StringReplace(sValue, '%21', '!', [rfReplaceAll]);
      sValue := StringReplace(sValue, '%27', '''', [rfReplaceAll]);
      sValue := StringReplace(sValue, '%28', '(', [rfReplaceAll]);
      sValue := StringReplace(sValue, '%29', ')', [rfReplaceAll]);
      sValue := StringReplace(sValue, '%26', '&', [rfReplaceAll]);
      sValue := StringReplace(sValue, '%3D', '=', [rfReplaceAll]);
      sValue := StringReplace(sValue, '%7E', '~', [rfReplaceAll]);
      sValue := '/info/ticker' +  chr(0) + sValue +  chr(0 ) + sTime;
      sTmp:= CalculateHMACSHA512( sValue, 'ab16921bf03ce7f32a0f3a7fdf6acabc');
      sig := TIdEncoderMIME.EncodeString( sTmp, IndyTextEncoding_UTF8 );
      restReq.Params.Clear;
      restReq.AddParameter('Api-Key', '5a3d078609ef394c8ec5487bb66b282a', TRESTRequestParameterKind.pkHTTPHEADER );//, [poDoNotEncode]);
      restReq.AddParameter('Api-Sign', sig , TRESTRequestParameterKind.pkHTTPHEADER , [poDoNotEncode]);
      restReq.AddParameter('Api-Nonce', sTime , TRESTRequestParameterKind.pkHTTPHEADER );
      restReq.AddParameter('endPoint', '/info/ticker', TRESTRequestParameterKind.pkREQUESTBODY);
      restReq.AddParameter('order_currency', sCode, TRESTRequestParameterKind.pkREQUESTBODY);


      restReq.Method   := rmPOST;
      restReq.Execute;
      memo1.Lines.Add( restRes.JSONValue.ToString );

end;

procedure TForm3.Button18Click(Sender: TObject);
var
  sCode, sValue, sEncode, sig, sData, sTime,  sTmp, sContent : string;
begin

  try
   //	TParallel.For( 0, aCodes.Count-1, procedure(i:integer)      

    	sCode := 'TRX'; //aCodes[i];
      restClient.BaseURL := 'https://api.bithumb.com';
      sTmp    := '/info/orders';
      restReq.Resource := sTmp;
      sTime    := Gettamptime2(13 );
      sValue := HTTPEncode(UTF8Encode('endPoint=/info/orders&order_currency='+sCode));
      sValue := StringReplace(sValue, '+', '%20', [rfReplaceAll]);
      sValue := StringReplace(sValue, '%21', '!', [rfReplaceAll]);
      sValue := StringReplace(sValue, '%27', '''', [rfReplaceAll]);
      sValue := StringReplace(sValue, '%28', '(', [rfReplaceAll]);
      sValue := StringReplace(sValue, '%29', ')', [rfReplaceAll]);
      sValue := StringReplace(sValue, '%26', '&', [rfReplaceAll]);
      sValue := StringReplace(sValue, '%3D', '=', [rfReplaceAll]);
      sValue := StringReplace(sValue, '%7E', '~', [rfReplaceAll]);  
      sValue := '/info/orders' +  chr(0) + sValue +  chr(0 ) + sTime;
      sTmp:= CalculateHMACSHA512( sValue, 'ab16921bf03ce7f32a0f3a7fdf6acabc');
      sig := TIdEncoderMIME.EncodeString( sTmp, IndyTextEncoding_UTF8 );
      restReq.Params.Clear;
      restReq.AddParameter('Api-Key', '5a3d078609ef394c8ec5487bb66b282a', TRESTRequestParameterKind.pkHTTPHEADER );//, [poDoNotEncode]);
      restReq.AddParameter('Api-Sign', sig , TRESTRequestParameterKind.pkHTTPHEADER , [poDoNotEncode]);
      restReq.AddParameter('Api-Nonce', sTime , TRESTRequestParameterKind.pkHTTPHEADER );
      restReq.AddParameter('endPoint', '/info/orders', TRESTRequestParameterKind.pkREQUESTBODY);
      restReq.AddParameter('order_currency', sCode, TRESTRequestParameterKind.pkREQUESTBODY);

      restReq.Method   := rmPOST;
      restReq.Execute;  
      memo1.Lines.Add( restRes.JSONValue.ToString );


      //
    	sCode := 'TRX'; //aCodes[i];
      restClient.BaseURL := 'https://api.bithumb.com';
      sTmp    := '/info/user_transactions';
      restReq.Resource := sTmp;
      sTime    := Gettamptime2(13 );
      sValue := HTTPEncode(UTF8Encode('endPoint=/info/user_transactions&order_currency='+sCode+'&payment_currency=KRW'));
      sValue := StringReplace(sValue, '+', '%20', [rfReplaceAll]);
      sValue := StringReplace(sValue, '%21', '!', [rfReplaceAll]);
      sValue := StringReplace(sValue, '%27', '''', [rfReplaceAll]);
      sValue := StringReplace(sValue, '%28', '(', [rfReplaceAll]);
      sValue := StringReplace(sValue, '%29', ')', [rfReplaceAll]);
      sValue := StringReplace(sValue, '%26', '&', [rfReplaceAll]);
      sValue := StringReplace(sValue, '%3D', '=', [rfReplaceAll]);
      sValue := StringReplace(sValue, '%7E', '~', [rfReplaceAll]);  
      sValue := '/info/user_transactions' +  chr(0) + sValue +  chr(0 ) + sTime;
      sTmp:= CalculateHMACSHA512( sValue, 'ab16921bf03ce7f32a0f3a7fdf6acabc');
      sig := TIdEncoderMIME.EncodeString( sTmp, IndyTextEncoding_UTF8 );
      restReq.Params.Clear;
      restReq.AddParameter('Api-Key', '5a3d078609ef394c8ec5487bb66b282a', TRESTRequestParameterKind.pkHTTPHEADER );//, [poDoNotEncode]);
      restReq.AddParameter('Api-Sign', sig , TRESTRequestParameterKind.pkHTTPHEADER , [poDoNotEncode]);
      restReq.AddParameter('Api-Nonce', sTime , TRESTRequestParameterKind.pkHTTPHEADER );
      restReq.AddParameter('endPoint', '/info/user_transactions', TRESTRequestParameterKind.pkREQUESTBODY);
      restReq.AddParameter('order_currency', sCode, TRESTRequestParameterKind.pkREQUESTBODY);
      restReq.AddParameter('payment_currency', 'KRW', TRESTRequestParameterKind.pkREQUESTBODY);

      restReq.Method   := rmPOST;
      restReq.Execute;  
      memo1.Lines.Add( restRes.JSONValue.ToString );             
  
 
  finally
  end;
end;


procedure TForm3.상세Click(Sender: TObject);
var
  sCode, sValue, sEncode, sig, sData, sTime,  sTmp, sContent : string;
begin
      //
    	sCode := 'TRX'; //aCodes[i];
      restClient.BaseURL := 'https://api.bithumb.com';
      sTmp    := '/info/order_detail';
      restReq.Resource := sTmp;
      sTime    := Gettamptime2(13 );
      sValue := HTTPEncode(UTF8Encode('endPoint=/info/order_detail&order_id=C0117000000197655899&order_currency='+sCode));
      sValue := StringReplace(sValue, '+', '%20', [rfReplaceAll]);
      sValue := StringReplace(sValue, '%21', '!', [rfReplaceAll]);
      sValue := StringReplace(sValue, '%27', '''', [rfReplaceAll]);
      sValue := StringReplace(sValue, '%28', '(', [rfReplaceAll]);
      sValue := StringReplace(sValue, '%29', ')', [rfReplaceAll]);
      sValue := StringReplace(sValue, '%26', '&', [rfReplaceAll]);
      sValue := StringReplace(sValue, '%3D', '=', [rfReplaceAll]);
      sValue := StringReplace(sValue, '%7E', '~', [rfReplaceAll]);
      sValue := '/info/order_detail' +  chr(0) + sValue +  chr(0 ) + sTime;
      sTmp:= CalculateHMACSHA512( sValue, 'ab16921bf03ce7f32a0f3a7fdf6acabc');
      sig := TIdEncoderMIME.EncodeString( sTmp, IndyTextEncoding_UTF8 );
      restReq.Params.Clear;
      restReq.AddParameter('Api-Key', '5a3d078609ef394c8ec5487bb66b282a', TRESTRequestParameterKind.pkHTTPHEADER );//, [poDoNotEncode]);
      restReq.AddParameter('Api-Sign', sig , TRESTRequestParameterKind.pkHTTPHEADER , [poDoNotEncode]);
      restReq.AddParameter('Api-Nonce', sTime , TRESTRequestParameterKind.pkHTTPHEADER );
      restReq.AddParameter('endPoint', '/info/order_detail', TRESTRequestParameterKind.pkREQUESTBODY);
      restReq.AddParameter('order_id', 'C0117000000197655899', TRESTRequestParameterKind.pkREQUESTBODY);
      restReq.AddParameter('order_currency', sCode, TRESTRequestParameterKind.pkREQUESTBODY);


      restReq.Method   := rmPOST;
      restReq.Execute;
      memo1.Lines.Add( restRes.JSONValue.ToString );
  
end;



procedure TForm3.Button8Click(Sender: TObject);
var
  sValue, sEncode, sig, sData, sTime,  sTmp, sContent : string;
begin
  restClient.BaseURL := 'https://api.bithumb.com';
  restReq.Resource := '/info/balance';
  sTmp    := '/info/balance';
  //sEncode := Format('endpoint=%s&currency=BTC', [ EncodeURIComponent(sTmp) ]);
//  sEncode  := TBase64.URLDecode( format('endpoint=%s&currency=ALL', [ sEncode ] )  ).AsString;
  sTime    := Gettamptime2(13 );
  sValue := HTTPEncode(UTF8Encode('endPoint=/info/balance&currency=BTC'));
  sValue := StringReplace(sValue, '+', '%20', [rfReplaceAll]);
  sValue := StringReplace(sValue, '%21', '!', [rfReplaceAll]);
  sValue := StringReplace(sValue, '%27', '''', [rfReplaceAll]);
  sValue := StringReplace(sValue, '%28', '(', [rfReplaceAll]);
  sValue := StringReplace(sValue, '%29', ')', [rfReplaceAll]);
  sValue := StringReplace(sValue, '%26', '&', [rfReplaceAll]);
  sValue := StringReplace(sValue, '%3D', '=', [rfReplaceAll]);
  sValue := StringReplace(sValue, '%7E', '~', [rfReplaceAll]);
//  sValue := '/info/balance' + chr(1)+ sValue + chr(1) + sTime;
  sValue := '/info/balance' +  chr(0) + sValue +  chr(0 ) + sTime;
//  DoLog( sValue );
//  DoLog(  CalculateHMACSHA256( sValue, 'ab16921bf03ce7f32a0f3a7fdf6acabc') );
//  sValue := '/info/balanceendPoint=%2Finfo%2Fbalance&currency=BTC';
//  DoLog( sValue );
//  DoLog(  CalculateHMACSHA256( sValue, 'ab16921bf03ce7f32a0f3a7fdf6acabc') );
  //sData :=  Format('%s %s %s', [ sTmp, sEncode, sTime ]) ;
//  DoLog(  sValue );
  sTmp:= CalculateHMACSHA512( sValue, 'ab16921bf03ce7f32a0f3a7fdf6acabc');

//  DoLog(  CalculateHMACSHA512( sValue, 'ab16921bf03ce7f32a0f3a7fdf6acabc') );
  //DoLog(  sTmp );
  sig := TIdEncoderMIME.EncodeString( sTmp, IndyTextEncoding_UTF8 );
  restReq.Params.Clear;
  restReq.AddParameter('Api-Key', '5a3d078609ef394c8ec5487bb66b282a', TRESTRequestParameterKind.pkHTTPHEADER );//, [poDoNotEncode]);
  restReq.AddParameter('Api-Sign', sig , TRESTRequestParameterKind.pkHTTPHEADER , [poDoNotEncode]);
  restReq.AddParameter('Api-Nonce', sTime , TRESTRequestParameterKind.pkHTTPHEADER );
//  restReq.AddParameter('Content-Type', 'multipart/form-data' , TRESTRequestParameterKind.pkHTTPHEADER );
//  restReq.Accept := '*/*';
//  restReq.AcceptEncoding := 'gzip, deflate, br';
//
  restReq.AddParameter('endPoint', '/info/balance', TRESTRequestParameterKind.pkREQUESTBODY);
  restReq.AddParameter('currency', 'BTC', TRESTRequestParameterKind.pkREQUESTBODY);
  restReq.Method   := rmPOST;
  restReq.Execute;
//  vFormData.Free;
//  var i : integer;
//  for i := 0 to restReq.Params.Count-1 do
//  begin
//    DoLog( Format('%d. %s : %s %d, %d', [ i,  restReq.Params.Items[i].Name,  restReq.Params.Items[i].Value
//      ,  integer(restReq.Params.Items[i].ContentType)  , integer( restReq.Params.Items[i].Kind) ]));
//  end;


  memo1.Lines.Add( restRes.JSONValue.ToString );
end;


function UNIXTimeInMilliseconds: Int64;
var
  DateTime: TDateTime;
  SystemTime: TSystemTime;
begin
  GetSystemTime(SystemTime);
  DateTime := System.SysUtils.EncodeDate(SystemTime.wYear, SystemTime.wMonth, SystemTime.wDay) +
        System.SysUtils.EncodeTime(SystemTime.wHour, SystemTime.wMinute, SystemTime.wSecond, SystemTime.wMilliseconds);
  Result := System.DateUtils.MilliSecondsBetween(DateTime, UnixDateDelta);
end;
procedure TForm3.Button5Click(Sender: TObject);
var
  iTime : int64;
  dtTime, d2, d3: TDateTime;
  sTmp : string;
  ts : TTimeStamp;
begin
  iTime := StrToInt64(edit2.Text);
//  ts.Time := iTime mod 1000;
//  d3 := TimeStampToDateTime(ts);

  dtTime:= UnixToDateTime(iTime div 1000, false );
  d2 := (iTime / 86400) + 25569;
  sTmp := Format( '%s.%03d', [ FormatDateTime('yyyy-mm-dd hh:nn:ss', dtTime ),
    iTime mod 1000 ]);

  d2:= EncodeDate(  StrToInt(copy(sTmp, 1, 4 ) ) , StrToInt( copy(sTmp, 6, 2 )) , StrToInt( copy(sTmp, 9,2)) )
          + EnCodeTime(StrToInt(copy(sTmp, 12,2))
                    ,StrToInt(copy(sTmp, 15,2))
                    ,StrToInt(copy(sTmp, 18,2))
                    ,StrToInt(copy(sTmp, 21,3)) )  ;
  memo1.Lines.Add( Format('%s,%.10f,  %.10f, %.10f -> %s' ,[ sTmp, d2-dtTime,  dtTime,  d2, FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', d2 ) ] ) );
end;

procedure TForm3.Button6Click(Sender: TObject);
var
  guid : TGUID;
begin
  CreateGUID(guid);
  memo1.Lines.Add( GUIDToString(guid)  )   ;
end;
// 업비트 계좌 조회
procedure TForm3.Button9Click(Sender: TObject);
var
  LToken: TJWT;
  guid : TGUID;
  sSig, sID, sToken, sOut, apikey, sKey ,sJson : string;
begin
  LToken:= TJWT.Create(TJWTClaims);
  try
    sID := GetUUID;
  	apikey := 'pPutaXMQMoY3wzyhe2B4ZxNBKd0Fbb4DyaVDQrNN';
	  sKey   := 'vKE178MrOBDu5CsjoNtEW7N6Kg4qYK8BiqNsxoux';

//  	apikey := 'Bru2m8dUJLhk9t6OvR0LJMeRLad4BiGGZuVe0wKD';
//	  sKey   := 'EC70nhGg2PJE4XqgkMxMJXkXm0f1SxBYgyhYxOxx';
    LToken.Claims.SetClaimOfType<string>('access_key', apikey);
    LToken.Claims.SetClaimOfType<string>('nonce', sID );
    sSig := TJOSE.SerializeCompact(sKey,  TJOSEAlgorithmId.HS256, LToken);
    sToken := Format('Bearer %s', [sSig ]);
    restReq.AddParameter('Authorization', sToken, TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode] );
	  restClient.BaseURL := 'https://api.upbit.com';
  	restReq.Resource := '/v1/accounts';

    restReq.Method   := rmGET;    
    restReq.Execute;

    memo1.Lines.Add( restRes.JSONValue.ToString );
  finally
    LToken.Free;
  end;
end;
procedure TForm3.Button10Click(Sender: TObject);
var
  LToken: TJWT;
  guid : TGUID;
  sSig, sID, sToken, sOut, apikey, sKey ,sJson : string;
  vHash : THashSHA2;
begin        
  LToken:= TJWT.Create(TJWTClaims);
  try
    sID := GetUUID;
  	apikey := 'pPutaXMQMoY3wzyhe2B4ZxNBKd0Fbb4DyaVDQrNN';
	  sKey   := 'vKE178MrOBDu5CsjoNtEW7N6Kg4qYK8BiqNsxoux';
//  	apikey := 'Bru2m8dUJLhk9t6OvR0LJMeRLad4BiGGZuVe0wKD';
//	  sKey   := 'EC70nhGg2PJE4XqgkMxMJXkXm0f1SxBYgyhYxOxx';
        
    sJson  := 'market=KRW-XRP';
    sOut  := vHash.gethashstring( sJson, SHA512 );

    LToken.Claims.SetClaimOfType<string>('access_key', apikey);
    LToken.Claims.SetClaimOfType<string>('nonce', sID );    
    LToken.Claims.SetClaimOfType<string>('query_hash', sOut );
    LToken.Claims.SetClaimOfType<string>('query_hash_alg', 'SHA512' );
    restReq.Params.Clear;         
    
    sSig := TJOSE.SerializeCompact(sKey,  TJOSEAlgorithmId.HS512, LToken);
    sToken := Format('Bearer %s', [sSig ]);
    restReq.AddParameter('Authorization', sToken, TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode] );
	  restClient.BaseURL := 'https://api.upbit.com';
  	restReq.Resource := '/v1/orders/chance?'+sJson;

    restReq.Method   := rmGET;    
    restReq.Execute;
        
    memo1.Lines.Add( restRes.JSONValue.ToString );
  finally
    LToken.Free;
  end;
end;
procedure TForm3.Button14Click(Sender: TObject);
var
  LToken: TJWT;
  guid : TGUID;
  sSig, sID, sToken, sOut, apikey, sKey ,sJson : string;
  vHash : THashSHA2;
begin        
  LToken:= TJWT.Create(TJWTClaims);
  try
    sID := GetUUID;
  	apikey := 'pPutaXMQMoY3wzyhe2B4ZxNBKd0Fbb4DyaVDQrNN';
	  sKey   := 'vKE178MrOBDu5CsjoNtEW7N6Kg4qYK8BiqNsxoux';
//  	apikey := 'Bru2m8dUJLhk9t6OvR0LJMeRLad4BiGGZuVe0wKD';
//	  sKey   := 'EC70nhGg2PJE4XqgkMxMJXkXm0f1SxBYgyhYxOxx';
         
//    sJson  := 'state=wait&page=1';
    sJson  := 'state=done';
    sOut  := vHash.gethashstring( sJson, SHA512 );

    LToken.Claims.SetClaimOfType<string>('access_key', apikey);
    LToken.Claims.SetClaimOfType<string>('nonce', sID );    
    LToken.Claims.SetClaimOfType<string>('query_hash', sOut );    
    LToken.Claims.SetClaimOfType<string>('query_hash_alg', 'SHA512' );    
    restReq.Params.Clear;         
    
    sSig := TJOSE.SerializeCompact(sKey,  TJOSEAlgorithmId.HS512, LToken);
    sToken := Format('Bearer %s', [sSig ]);
    restReq.AddParameter('Authorization', sToken, TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode] );
	  restClient.BaseURL := 'https://api.upbit.com';
  	restReq.Resource := '/v1/orders?'+sJson;
      
    restReq.Method   := rmGET;    
    restReq.Execute;
        
    memo1.Lines.Add( restRes.JSONValue.ToString );
  finally
    LToken.Free;
  end;
end;

procedure TForm3.Button15Click(Sender: TObject);
var
  LToken: TJWT;
  guid : TGUID;
  sSig, sID, sToken, sOut, apikey, sKey ,sJson : string;
  vHash : THashSHA2;
begin
  LToken:= TJWT.Create(TJWTClaims);
  try
    sID := GetUUID;
  	apikey := 'pPutaXMQMoY3wzyhe2B4ZxNBKd0Fbb4DyaVDQrNN';
	  sKey   := 'vKE178MrOBDu5CsjoNtEW7N6Kg4qYK8BiqNsxoux';
//  	apikey := 'Bru2m8dUJLhk9t6OvR0LJMeRLad4BiGGZuVe0wKD';
//	  sKey   := 'EC70nhGg2PJE4XqgkMxMJXkXm0f1SxBYgyhYxOxx';
    
    sJson  := 'uuids[]=40ed4d6a-2986-44d9-a6c5-6a6f598944eb&uuids[]=c50ddf49-736e-4e1d-bffa-f28b83084363';
    sOut  := vHash.gethashstring( sJson, SHA512 );      

    LToken.Claims.SetClaimOfType<string>('access_key', apikey);
    LToken.Claims.SetClaimOfType<string>('nonce', sID );    
    LToken.Claims.SetClaimOfType<string>('query_hash', sOut );
    LToken.Claims.SetClaimOfType<string>('query_hash_alg', 'SHA512' );
    restReq.Params.Clear;
    
    sSig := TJOSE.SerializeCompact(sKey,  TJOSEAlgorithmId.HS512, LToken);
    sToken := Format('Bearer %s', [sSig ]);
    restReq.AddParameter('Authorization', sToken, TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode] );
	  restClient.BaseURL := 'https://api.upbit.com';
  	restReq.Resource := '/v1/orders?'+sJson;
      
    restReq.Method   := rmGET;
    restReq.Execute;
        
    memo1.Lines.Add( restRes.JSONValue.ToString );
  finally
    LToken.Free;
  end;

end;
procedure TForm3.Button16Click(Sender: TObject);
var
  LToken: TJWT;
  guid : TGUID;
  sSig, sID, sToken, sOut, apikey, sKey ,sJson : string;
  vHash : THashSHA2;
begin
  LToken:= TJWT.Create(TJWTClaims);
  try
    sID := GetUUID;
  	apikey := 'Bru2m8dUJLhk9t6OvR0LJMeRLad4BiGGZuVe0wKD';
	  sKey   := 'EC70nhGg2PJE4XqgkMxMJXkXm0f1SxBYgyhYxOxx';

//  	apikey := 'pPutaXMQMoY3wzyhe2B4ZxNBKd0Fbb4DyaVDQrNN';
//	  sKey   := 'vKE178MrOBDu5CsjoNtEW7N6Kg4qYK8BiqNsxoux';
    sJson  := 'uuid='+edtUid.Text;//-2986-44d9-a6c5-6a6f598944eb';
    sOut  := vHash.gethashstring( sJson, SHA512 );
    LToken.Claims.SetClaimOfType<string>('access_key', apikey);
    LToken.Claims.SetClaimOfType<string>('nonce', sID );
    LToken.Claims.SetClaimOfType<string>('query_hash', sOut );
    LToken.Claims.SetClaimOfType<string>('query_hash_alg', 'SHA512' );
    restReq.Params.Clear;

    sSig := TJOSE.SerializeCompact(sKey,  TJOSEAlgorithmId.HS512, LToken);
    sToken := Format('Bearer %s', [sSig ]);
    restReq.AddParameter('Authorization', sToken, TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode] );
	  restClient.BaseURL := 'https://api.upbit.com';
  	restReq.Resource := '/v1/order?'+sJson;

    restReq.Method   := rmDELETE;
    restReq.Execute;

    memo1.Lines.Add( restRes.JSONValue.ToString );
  finally
    LToken.Free;
  end;
end;

procedure TForm3.Button26Click(Sender: TObject);
var
  LToken: TJWT;
  guid : TGUID;
  sSig, sID, sToken, sOut, apikey, sKey ,sJson : string;
  vHash : THashSHA2;
begin
  LToken:= TJWT.Create(TJWTClaims);
  try
    sID := GetUUID;
  	apikey := 'Bru2m8dUJLhk9t6OvR0LJMeRLad4BiGGZuVe0wKD';
	  sKey   := 'EC70nhGg2PJE4XqgkMxMJXkXm0f1SxBYgyhYxOxx';

//  	apikey := 'pPutaXMQMoY3wzyhe2B4ZxNBKd0Fbb4DyaVDQrNN';
//	  sKey   := 'vKE178MrOBDu5CsjoNtEW7N6Kg4qYK8BiqNsxoux';
    sJson  := 'uuid='+edtUid.Text;//-2986-44d9-a6c5-6a6f598944eb';
    sOut  := vHash.gethashstring( sJson, SHA512 );
    LToken.Claims.SetClaimOfType<string>('access_key', apikey);
    LToken.Claims.SetClaimOfType<string>('nonce', sID );
    LToken.Claims.SetClaimOfType<string>('query_hash', sOut );
    LToken.Claims.SetClaimOfType<string>('query_hash_alg', 'SHA512' );
    restReq.Params.Clear;

    sSig := TJOSE.SerializeCompact(sKey,  TJOSEAlgorithmId.HS512, LToken);
    sToken := Format('Bearer %s', [sSig ]);
    restReq.AddParameter('Authorization', sToken, TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode] );
	  restClient.BaseURL := 'https://api.upbit.com';
  	restReq.Resource := '/v1/order?'+sJson;

    restReq.Method   := rmGET;
    restReq.Execute;

    memo1.Lines.Add( restRes.JSONValue.ToString );
  finally
    LToken.Free;
  end;

end;
procedure TForm3.Button17Click(Sender: TObject);
var
  LToken: TJWT;
  guid : TGUID;
  sSig, sID, sToken, sOut, apikey, sKey ,sJson : string;
  vHash : THashSHA2;
  aParam : TRESTRequestParameter;
  aObj : TJsonObject;
begin        
  LToken:= TJWT.Create(TJWTClaims);
  aObj 	:= TJsonObject.Create;
  aObj.AddPair('market','KRW-TRX');
  aObj.AddPair('side','bid');
  aObj.AddPair('volume','69.39388235');
  aObj.AddPair('price','85');
  aObj.AddPair('order_type','limit');
  
  try
    sID := GetUUID;
  	apikey := 'Bru2m8dUJLhk9t6OvR0LJMeRLad4BiGGZuVe0wKD';
	  sKey   := 'EC70nhGg2PJE4XqgkMxMJXkXm0f1SxBYgyhYxOxx';
    sJson  := 'market=KRW-TRX&side=bid&volume=69.39388235&price=85&order_type=limit';
    sOut  := vHash.gethashstring( sJson, SHA512 );
    LToken.Claims.SetClaimOfType<string>('access_key', apikey);
    LToken.Claims.SetClaimOfType<string>('nonce', sID );
    LToken.Claims.SetClaimOfType<string>('query_hash', sOut );
    LToken.Claims.SetClaimOfType<string>('query_hash_alg', 'SHA512' );
    restReq.Params.Clear;
    
    sSig := TJOSE.SerializeCompact(sKey,  TJOSEAlgorithmId.HS512, LToken);
    sToken := Format('Bearer %s', [sSig ]);
    restReq.AddParameter('Authorization', sToken, TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode] );
	  restClient.BaseURL := 'https://api.upbit.com';
  	restReq.Resource := '/v1/orders';
		//restClient.Accept	:= 'application/json';
//    memo1.Lines.Add( aObj.ToString );
    
//    aParam := restReq.Params.AddItem;
//    aParam.Value := aObj.ToString;
//    aParam.ContentType := ctAPPLICATION_JSON;  

    restReq.Body.Add(aObj);
      
    restReq.Method   := rmPOST;
    restReq.Execute;
        
    memo1.Lines.Add( restRes.JSONValue.ToString );
  finally
    LToken.Free;
   	aObj.Free;
  end;
end;

procedure TForm3.Button7Click(Sender: TObject);
var
  sData, apikey, sSig, sID, sKey, sToken, sKey2 : string;
  guid : TGUID;
  LToken: TJWT;
  LKey: TJWK;
  LSigner: TJWS;
  i : Integer;
begin
  restClient.BaseURL := 'https://api.upbit.com';
  restReq.Resource := '/v1/status/wallet';
  apikey := 'pPutaXMQMoY3wzyhe2B4ZxNBKd0Fbb4DyaVDQrNN';
  sKey   := 'vKE178MrOBDu5CsjoNtEW7N6Kg4qYK8BiqNsxoux';
  CreateGUID(guid);
  sData  := format('{"access_key":"%s","nonce":"%s"}', [ apikey, GUIDToString(guid) ]);
  sSig   := CalculateHMACSHA256(sData,sKey );

  LToken:= TJWT.Create(TJWTClaims);
  try

    sData := GUIDToString(guid);
    sID := Copy( sData, 2, Length( sData) - 2);
//    sData  := format('{"access_key":"%s","nonce":"%s"}', [ apikey, sID ]);
//    sSig   := CalculateHMACSHA256(sData,sKey );
//    restReq.Method   := rmGET;
//    restReq.AddParameter('Authorization', sToken, pkHTTPHEADER );
//    restReq.Execute;
    memo1.Lines.Add( sData );
    LToken.Claims.SetClaimOfType<string>('access_key', apikey);
    LToken.Claims.SetClaimOfType<string>('nonce', sID );

    sKey2 :=  TBase64.Encode(sKey).AsString;
//    sSig := TJOSE.SHA256CompactToken( sKey, LToken);
    sSig := TJOSE.SerializeCompact(sKey,  TJOSEAlgorithmId.HS256, LToken);
    var jwt: TJWT := TJOSE.Verify(sKey, sSig);
    if jwt.Verified then
      memo1.Lines.Add( 'verified');
    //sSig := 'eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJhY2Nlc3Nfa2V5IjoicFB1dGFYTVFNb1kzd3p5aGUyQjRaeE5CS2QwRmJiNER5YVZEUXJOTiIsIm5vbmNlIjoie2U3OTg2ZWMyLTk5MWUtNDYzMS04OTNhLTViODRhNTJjMTUyMH0iLCJpYXQiOjE2NDY4MDM3OTV9.ClIFNF-bsoYM8rJdJMraKKf9gMBoFQTGtfiYykFxlmQ';
    sToken := Format('Bearer %s', [sSig ]);
    memo1.Lines.Add( TJSONUtils.ToJSON(LToken.Header.JSON) );
    memo1.Lines.Add( TJSONUtils.ToJSON(LToken.Claims.JSON) );
    memo1.Lines.Add( sToken );
    restReq.Params.Clear;
//    LKey := TJWK.Create(sKey);
//    LSigner := TJWS.Create(LToken);
//
//    memo1.Lines.Add( TJSONUtils.ToJSON(LToken.Header.JSON) );
//    memo1.Lines.Add( TJSONUtils.ToJSON(LToken.Claims.JSON) );
//    LSigner.Sign(LKey,  TJOSEAlgorithmId.HS256);
//    sToken := Format('Bearer %s', [ LSigner.Signature.AsString ]);
//     Content-Type: application/json; charset=utf-8
    restReq.Method   := rmGET;
    restReq.AddParameter('Authorization', sToken, TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode] );
    restReq.Execute;
  finally
    LToken.Free;
 // LSigner.Free;
  end;
for i:=0 to  restRes.Headers.Count-1 do
begin
  memo1.Lines.Add( format('%d : %s',  [ i, restRes.Headers[i]  ] ) );
end;
  memo1.Lines.Add( restRes.JSONValue.ToString );
end;

procedure TForm3.restReqAfterExecute(Sender: TCustomRESTRequest);
begin
  //
  //memo1.Lines.Add('a');
end;
procedure TForm3.restReqHTTPProtocolError(Sender: TCustomRESTRequest);
begin
 memo1.Lines.Add('error : ' + Sender.Response.Content);
end;


procedure TForm3.testparse;
var
  i : integer;
  sTmp : string;
  sts   : TArray<string>;
begin
  memo1.Lines.Add(  '2. threadid : ' + intTostr( GetCurrentThreadid ) );
  memo1.Lines.Add('===================');
  sTmp := restRes.FullRequestURI;
  sTmp := 'https://api.bithumb.com/info/balance';
  sts  := sTmp.Split(['/']);

//  for I := 0 to restRes.Headers.Count-1 do
//  begin
//
//    memo1.Lines.Add( Format('%d.th %s : %s', [ i, restRes.Headers.Names[i], restRes.Headers.Values[ restRes.Headers.Names[i]] ])  );
//  end;
  for i:=0 to high(sts) do
  begin
    memo1.Lines.Add( Format('%d.th %s ', [ i, sts[i]])  );
  end;
end;


{ TDecimalHelper }


{ TDecimalHelper }

procedure TDecimalHelper.convert(sval:string);
var
  sts : TArray<string>;
  iCnt, iLen, j: integer;
begin
  if sval = '' then Exit;

  OrgVal := sval;

  sts  := OrgVal.Split(['.']);
  iLen := High(sts);


  if iLen >= 1 then  begin
    Jungsu  := sts[0];
    Sosu    := sts[1];
    iCnt    := 0;

    for j := Length(Sosu) downto 1 do
    begin
      if Sosu[j] <> '0' then     break;
      if Sosu[j] = '0' then      inc( iCnt);
    end;

    Precision  := length(sts[1]) - iCnt;
    if iCnt > 0 then
      Sosu := Copy( sts[1], 1, Precision );

    for j := 0 to Precision-1 do  Multiple := Multiple*10;

  end else
    Jungsu := OrgVal;

  ConVal  :=  StrToInt64(Jungsu + Sosu);
end;

constructor TDecimalHelper.Create(sval: string);
begin
  //
  OrgVal  := sval;
  Sosu    := '';
  Multiple  := 1;
  Precision := 0;
end;

function TDecimalHelper.print : string;
begin
  Result := Format('O:%s Cnv:%d %d , %.*f', [ OrgVal, ConVal, Precision, Precision, ConVal / Multiple ]);
end;

function TDecimalHelper.ToDouble: double;
begin
  Result  := ConVal / Multiple;
end;

function TDecimalHelper.ToInt64: int64;
begin
  Result  := ConVal;
end;

end.
