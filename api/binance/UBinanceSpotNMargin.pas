unit UBinanceSpotNMargin;

interface

uses
  System.Classes, System.SysUtils, System.DateUtils,

  System.JSON,  Rest.Json ,   Rest.Types,
  UExchange,

  UApiTypes

  ;

type
  TBinanceSpotNMargin = class( TExchange )
  private

    FCodes: TStrings;

  public
    Constructor Create( aObj : TObject; aMarketType : TMarketType );
    Destructor  Destroy; override;

    procedure ParsePrepareMaster; override;

    property Codes  : TStrings read FCodes;
  end;

implementation

uses
  GApp
  ;

{ TBinanceSpotNMargin }

constructor TBinanceSpotNMargin.Create(aObj: TObject; aMarketType: TMarketType);
begin
  inherited Create( aObj, aMarketType );

  FCodes:= TStringList.Create;
end;

destructor TBinanceSpotNMargin.Destroy;
begin
  FCodes.Free;
  inherited;
end;


procedure TBinanceSpotNMargin.ParsePrepareMaster;
var
  master : TJsonObject;
  aObj  : TJsonObject;
  aArr  : TJsonArray;
  aPair : TJSONPair;
  I: Integer;
begin

  master := TJsonObject.ParseJSONValue( MasterData ) as TJsonObject;
  if master = nil then Exit;
  aPair := master.Get('symbols');
  if aPair = nil then Exit;

  aArr  := aPair.JsonValue as TJsonArray;

  var sTmp : string;

  for I := 0 to aArr.Size-1 do
  begin
    aObj  := aArr.Get(i) as TJsonObject;
    sTmp  := aObj.GetValue('quoteAsset').Value;

    if sTmp = 'USDT' then
      FCodes.Add( aObj.GetValue('baseAsset').Value );
  end;

end;
end.
