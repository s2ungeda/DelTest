unit UExchangeRate;

interface

uses
  System.Classes, System.SysUtils, System.DateUtils,

  System.JSON,  Rest.Json , Rest.Types ,

  UExchange,

  UApiTypes

  ;

type
  TExchangeRate = class( TExchange )
  private
    FValue: double;
    procedure ParseExchangeRate( sData : string );
  public
    Constructor Create( aObj : TObject; aMarketType : TMarketType );
    Destructor  Destroy; override;

    procedure RequestData ;

    property Value : double read FValue;
  end;


implementation

uses
  GApp

  ;

{ TExchangeRate }

constructor TExchangeRate.Create(aObj: TObject; aMarketType: TMarketType);
begin
  inherited Create( aObj, aMarketType );
end;

destructor TExchangeRate.Destroy;
begin

  inherited;
end;



procedure TExchangeRate.ParseExchangeRate(sData: string);
var
  aArr : TJsonArray;
  aVal : TJsonValue;
  iVal, I: Integer;
  sTmp : string;
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
        sTmp := aVal.GetValue<string>('bkpr');
        FValue  := StrToFloat( sTmp );
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

  SetBaseUrl('https://koreaexim.go.kr');
  SetParam('authkey', 'mEN1OkANsoKalML5mAiPMn9h8aGUeCcZ' );
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

end.
