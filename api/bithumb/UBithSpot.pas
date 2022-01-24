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
begin

end;


end.
