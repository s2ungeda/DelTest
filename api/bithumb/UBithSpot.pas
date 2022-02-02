unit UBithSpot;

interface

uses
  System.Classes, System.SysUtils, System.DateUtils,

  System.JSON,  Rest.Json , Rest.Types ,

  UExchange,

  UApiTypes

  ;

type
  TBithSpot = class( TExchange )
  private

  public
    Constructor Create( aObj : TObject; aMarketType : TMarketType );
    Destructor  Destroy; override;

    function ParsePrepareMaster : integer  ; override;

  end;

implementation

uses
  GApp
  ;

{ TBinanceSpotNMargin }

constructor TBithSpot.Create(aObj: TObject; aMarketType: TMarketType);
begin
  inherited Create( aObj, aMarketType );

end;

destructor TBithSpot.Destroy;
begin

  inherited;
end;



function TBithSpot.ParsePrepareMaster : integer;
var
  aObj, master : TJsonObject;
  aVal : TJsonValue;
  aPair : TJsonPair;
  i : Integer;
begin
  master := TJsonObject.ParseJSONValue( MasterData ) as TJsonObject;
  if master = nil then Exit;
  aObj := master.GetValue('data') as TJsonObject;

  for I := 0 to aObj.Size-1 do
  begin
    aPair := aObj.Get(i);
    if aPair.JsonValue.ClassType = TJSONObject then
    begin
      aVal := aPair.JsonValue;
//      DoLog( Format('%d. %s : %s ', [ i, aPair.JsonString.Value, aPair.JsonValue.Value]));
      Codes.Add( aPair.JsonString.Value );
    end;
  end;
end;


end.
