unit UExchangeRate;

interface

uses
  System.Classes, System.SysUtils, System.DateUtils,

  System.JSON,  Rest.Json , Rest.Types ,

  UExchange,  { UQueryExRate, }

  UApiTypes

  ;

type
  TExchangeRate = class( TExchange )
  private
    FExRate : double;
//    FQueryExRate: TPhtnToDlph;
    FLastTime: TDateTime;
//    procedure ParseExchangeRate( sData : string );
    function GetValue: double;
    procedure SetValue(const val: double);
  public
    Constructor Create( aObj : TObject; aMarketType : TMarketType );
    Destructor  Destroy; override;

//    procedure RequestData ;
    function GetExRate : double;

    property Value : double read GetValue write SetValue;
    property LastTime : TDateTime read FLastTime write FLastTime;
//    exReate �� ��ü
//    property QueryExRate : TPhtnToDlph read FQueryExRate write FQueryExRate;
  end;


implementation

uses
  GApp   ,
  UConsts

  ;

{ TExchangeRate }

constructor TExchangeRate.Create(aObj: TObject; aMarketType: TMarketType);
begin
  inherited Create( aObj, aMarketType );
  FExRate := 0;
  FLastTime := 0;
//  FQueryExRate := nil;
end;

destructor TExchangeRate.Destroy;
begin
//  if FQueryExRate <> nil then
//  begin
//    FQueryExRate.Fin;
//    FQueryExRate.Free;
//  end;

  inherited;
end;



function TExchangeRate.GetExRate: double;
begin
//  if QueryExRate.LastValue <> '0' then
//    FExRate := StrToFloatDef( QueryExRate.LastValue, 0.0 );
  Result  := FExRate;
end;

function TExchangeRate.GetValue: double;
begin
  if FExRate <= PRICE_EPSILON then
    Result := 1
  else
    Result := FExRate;
end;

procedure TExchangeRate.SetValue(const val : double);
begin
  FExRate := val;
end;

{
procedure TExchangeRate.ParseExchangeRate(sData: string);
var
  aArr : TJsonArray;
  aVal : TJsonValue;
  iVal, I: Integer;
  sTmp, sVal : string;
  dEx : double;
begin
  if sData = '' then
  begin
    App.Log(llError, 'ParseExchangeRate data is empty') ;
    Exit;
  end;

  try

    aArr := TJsonObject.ParseJSONValue( sData) as TJsonArray;

    for I := 0 to aArr.Size-1 do
    begin
      aVal := aArr.Get(i);
      iVal := aVal.GetValue<integer>('result');
      sTmp := aVal.GetValue<string>('cur_unit');

      if (iVal = 1) and ( sTmp = 'USD')  then
      begin
        sTmp := trim( aVal.GetValue<string>('deal_bas_r') );
        sVal := sTmp.Replace(',', '');
        dEx  := StrToFloat( sVal );
        if dEx > 0 then begin
          Value  := dEx;
          App.SaveData(ldExRate, sVal );
        end;
//        :"1,194.7","bkpr":"1,194","yy_efee_r":"0","ten_dd_efee_r":"0","kftc_bkpr":"1,194","kftc_deal_bas_r":"1,194.7"
      end;
    end;

  except
  end;
end;

procedure TExchangeRate.RequestData;
var
  sJson, sOut : string;
begin

//  Value := 1900;
//  Exit;

  SetBaseUrl('https://koreaexim.go.kr');
  SetParam('authkey', 'mEN1OkANsoKalML5mAiPMn9h8aGUeCcZ' );
  SetParam('searchdate', FormatDateTime('yyyymmdd', now) );
  SetParam('data', 'AP01' );

  if Request( rmGET, '/site/program/financial/exchangeJSON', '', sJson, sOut ) then
  begin
    App.DebugLog('ex rate : %s', [sJson]);
    ParseExchangeRate( sJson );
  end else
  begin
    App.Log( llError, '', 'Failed ExRate RequestData (%s, %s)',  [  sOut, sJson] );
    Exit;
  end;
end;
}


end.
