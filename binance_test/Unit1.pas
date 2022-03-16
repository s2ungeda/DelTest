unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, REST.Types, Vcl.StdCtrls, REST.Client,
  Data.Bind.Components, Data.Bind.ObjectScope,

  System.JSON,
  Rest.Json
    ;

const

  Bin = 0;
  Bit = 2;
  Upb = 1;

  Sum = 3;
  Cnt = 4;

type
  TTestType = ( tt1, tt2, tt3 );

  TForm1 = class(TForm)
    Memo1: TMemo;
    btnMaster: TButton;
    Button2: TButton;
    conn: TRESTClient;
    req: TRESTRequest;
    res: TRESTResponse;
    Edit1: TEdit;
    Button1: TButton;
    Button3: TButton;
    procedure btnMasterClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
    Symbols : array [0..Cnt-1] of TStringList;

    procedure SetBaseUrl(stData : string );
    procedure DoLog( sLog : string );
  public
    { Public declarations }
    procedure parseBithumbJson( value : TJsonObject );
    procedure parseUpbitJson( value : TJSONArray );
    procedure parseJson( value : TJsonObject ); overload;
    procedure parseJson( value : string ); overload;
  end;

var
  Form1: TForm1;

implementation


{$R *.dfm}

procedure TForm1.btnMasterClick(Sender: TObject);
var
  sTmp : string;
begin
  SetBaseUrl( 'https://api.binance.com' );
  req.Resource := '/api/v3/exchangeInfo';
  req.Execute;

//  if res.Status.Success then
//    sTmp := 'suc'
//  else sTmp := 'failed';
//
//  DoLog( Format(' %d, %s, %s ', [
//  res.StatusCode, res.StatusText
//  , sTmp
//  ] ));

  parseJson(  res.Content );
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  SetBaseUrl('https://api.upbit.com');
  req.Resource := '/v1/market/all';
  req.Execute;

  parseUpbitJson( res.JSONValue as TJSONArray );
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  SetBaseUrl('https://api.bithumb.com');
  req.Resource := '/public/ticker/ALL_KRW';
  req.Execute;

  parseBithumbJson( res.JSONValue as TJsonObject );
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  s , sTmp: string;
  idx, iTmp : integer;
begin
  // upbit 와 binance 교집합..
  for s in Symbols[Upb] do
  begin
    idx := Symbols[Bin].IndexOf(s);
    if idx >=0 then
      Symbols[Sum].Add(s);
  end;

  iTmp := Symbols[Sum].Count;

  for s in Symbols[Bit] do
  begin
    idx := Symbols[Bin].IndexOf(s);
    if idx >=0 then
      Symbols[Sum].Add(s)
    else
      sTmp := sTmp + ',' + s;
  end;

  DoLog( Format('Tot %d = u(%d) + b(%d)', [ Symbols[Sum].Count , iTmp, Symbols[Sum].Count-iTmp ] ));
  DoLog( sTmp );
end;

procedure TForm1.DoLog(sLog: string);
begin
  memo1.Lines.Add( sLog );
end;



procedure TForm1.FormCreate(Sender: TObject);
var
  I: Integer;
  tt : TTestType;
begin
  tt := tt1;
  caption := intTostr( integer( tt ) );
  for I := 0 to Cnt-1 do
  begin
    Symbols[i] := TSTringList.Create;
    Symbols[i].Sorted := true;
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to Cnt-1 do
    Symbols[i].Free;
end;

procedure TForm1.parseJson(value: string);
begin
  ParseJson (
    TJsonObject.ParseJSONValue( Value) as TJsonObject
  );
end;

procedure TForm1.parseUpbitJson(value: TJSONArray);
var
  aObj  : TJsonObject;
  prObj : TJSONPair;
  stTmp, ss : string;
  sts   : TArray<string>;
  iLen, I: Integer;

begin
  if value = nil then Exit;

  Symbols[Upb].Clear;

  for I := 0 to value.Size-1 do
  begin
    aObj := value.Get(i) as TJsonObject;
    stTmp:= aObj.GetValue('market').Value;

    sts := stTmp.Split(['-']);
    iLen:= Length(sts);
    if iLen <= 1 then continue;

    if sts[0] = 'KRW' then begin
      Symbols[Upb].Add( sts[1] );
    end;
//      DoLog( Format('%d. %s : %s', [ i,  stTmp , aObj.GetValue('korean_name').Value ]));
//    DoLog( Format('%d. %s : %s', [ i,  aVal.GetValue<string>('market'), aVal.GetValue<string>('korean_name') ]));
  end;

  DoLog( Format('업비트 %d  ', [ Symbols[Upb].Count ]));



end;

procedure TForm1.parseJson(value: TJsonObject);
var
  aObj : TJsonObject;
  jsArr : TJsonArray;
  prObj : TJSONPair;
  I: Integer;
  ss, stTmp : string;

begin

  for I := 0 to value.Size-1 do
  begin
    prObj := value.get(i);
    DoLog( Format('%d. %s : %s', [ i, prObj.JsonString.Value, prObj.JsonValue.Value]));
  end;

  if value.get('symbols') = nil then exit;

  jsArr := value.get('symbols').JsonValue as TJsonArray;

  Symbols[Bin].Clear;

  for I := 0 to jsArr.Size-1 do
  begin
    aObj := jsArr.get(i) as TJsonObject;
    prObj:= aObj.Get('symbol');

    stTmp := aObj.GetValue('quoteAsset').Value;

    if stTmp = 'USDT' then
      Symbols[Bin].Add( aObj.GetValue('baseAsset').Value);

    Continue;

    DoLog( Format('%d. %s, %s , %s, %s, %s ', [ i,
       aObj.Get('symbol').ToString,             // key : value 전체 출력
       prObj.ToString,                          // key : value 전체 출력
       prObj.JsonString.Value ,                 // key 만 출력
       prObj.JsonValue.Value,                   // value 만 출력
       aObj.GetValue('symbol').Value   ]  ));   // value 만 출력
  end;

  DoLog( Format('바이낸스  %d ', [ Symbols[Bin].Count ] ));

end;

procedure TForm1.parseBithumbJson(value: TJsonObject);
var
  aObj : TJsonObject;
  aVal : TJsonValue;
  aPair: TJSONPair;
  I: Integer;
  ss,  sTmp : string;

begin
  aVal  := value.GetValue('data');
  if aVal = nil then Exit;

  aObj := aVal as TJsonObject;

  Symbols[Bit].Clear;

  for I := 0 to aObj.Size-1 do
  begin
    aPair := aObj.Get(i);
    //aPair.JsonValue.ClassType.ClassName = TJsonValue then
    if aPair.JsonValue.ClassType = TJSONObject then
    begin
      aVal := aPair.JsonValue;
//      DoLog( Format('%d. %s : %s ', [ i, aPair.JsonString.Value, aPair.JsonValue.Value]));
      Symbols[Bit].Add( aPair.JsonString.Value );
    end;
  end;


  DoLog( Format('빗썸  %d ', [ Symbols[Bit].Count ] ));


end;

procedure TForm1.SetBaseUrl(stData : string );
begin
  conn.BaseURL := stData;
end;

end.
