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
    FMasterData: string;
    FCodes: TStrings;
    procedure ParsePrepareMaster;
  public
    Constructor Create( aObj : TObject; aMarketType : TMarketType );
    Destructor  Destroy; override;

    function PrepareMaster : boolean;

    property MasterData : string read FMasterData;
    property Codes  : TStrings read FCodes;
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



procedure TBithSpot.ParsePrepareMaster;
begin

end;

function TBithSpot.PrepareMaster: boolean;
var
  sUrl, sOut, sJson : string;
begin
  sUrl  := App.Engine.ApiConfig.GetBaseUrl( etBitthumb , emSpot );
  SetBaseUrl( sUrl );
  if Request( rmGET, '/public/ticker/ALL_KRW' , '', sJson, sOut ) then
  begin
    FMasterData := sJson ;
    ParsePrepareMaster;
  end else
  begin
    App.Log( llError, '', 'Failed Binance spot PreparMaster (%s, %s)',
      [sOut, sJson] );
    Exit( false );
  end;

  Result := true;
end;

end.
