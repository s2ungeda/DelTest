unit UBinanceFutures;

interface

uses
  System.Classes, System.SysUtils, System.DateUtils,

  UExchange,

  UApiTypes

  ;

type
  TBinanceFutures = class( TExchange )
  public
    Constructor Create( aObj : TObject; aMarketType : TMarketType );
    Destructor  Destroy; override;
  end;

implementation

{ TBinanceSpotNMargin }

constructor TBinanceFutures.Create(aObj: TObject; aMarketType: TMarketType);
begin
  inherited Create( aObj, aMarketType );

end;

destructor TBinanceFutures.Destroy;
begin

  inherited;
end;

end.
