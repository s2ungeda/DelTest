unit UUpbitSpot;

interface

uses
  System.Classes, System.SysUtils, System.DateUtils,

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
begin

end;

end.
