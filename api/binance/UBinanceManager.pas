unit UBinanceManager;

interface

uses
  System.Classes, System.SysUtils, System.DateUtils,

  UExchangeManager,

  UApiTypes

  ;

type

  TBinanceManager = class( TExchangeManager )
  public
    Constructor  Create( aExType : TExchangeKind );
    Destructor  Destroy; override;

    function RequestMaster : boolean; override;
  end;

implementation

uses
  GApp

  ;

{ TBinanceManager }

constructor TBinanceManager.Create(aExType: TExchangeKind);
begin
  inherited Create( aExType );
end;

destructor TBinanceManager.Destroy;
begin
  inherited;
end;

function TBinanceManager.RequestMaster: boolean;
var
  I: TMarketType;
begin
  for I := mtSpot to High(TMarketType) do
    if not Exchanges[i].RequestMaster then
      Exit (false);

  Result := true;
end;



end.
