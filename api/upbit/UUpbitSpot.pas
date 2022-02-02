unit UUpbitSpot;

interface

uses
  System.Classes, System.SysUtils, System.DateUtils,

  System.JSON,  Rest.Json , Rest.Types ,

  UExchange,

  UApiTypes

  ;

type
  TUpbitSpot = class( TExchange )
  public
    Constructor Create( aObj : TObject; aMarketType : TMarketType );
    Destructor  Destroy; override;

    function ParsePrepareMaster : integer; override;
  end;

implementation

{ TBinanceSpotNMargin }

constructor TUpbitSpot.Create(aObj: TObject; aMarketType: TMarketType);
begin
  inherited Create( aObj, aMarketType );

end;

destructor TUpbitSpot.Destroy;
begin

  inherited;
end;



function TUpbitSpot.ParsePrepareMaster: integer;
var
  master : TJsonArray;
  aObj : TJsonObject;
  i, iLen : Integer;
  stTmp : string;
  sts   : TArray<string>;
begin
  master :=  TJsonObject.ParseJSONValue( MasterData) as TJsonArray;

  for I := 0 to master.Size-1 do
  begin
    aObj := master.Get(i) as TJsonObject;
    stTmp:= aObj.GetValue('market').Value;

    sts := stTmp.Split(['-']);
    iLen:= Length(sts);
    if iLen <= 1 then continue;

    if sts[0] = 'KRW' then begin
      Codes.Add( sts[1] );
    end;
  end;
end;

end.
