unit UUpbitManager;

interface

uses
  System.Classes, System.SysUtils, System.DateUtils,

  UExchangeManager,

  UApiTypes

  ;

type

  TUpbitManager = class( TExchangeManager )
  private

  public
    Constructor  Create( aExType : TExchangeKind );
    Destructor  Destroy; override;
//    function RequestMaster : boolean; override;
    function InitMarketWebSockets : boolean ; override;
    function SubscribeAll : boolean; override;
    procedure UnSubscribeAll ; override;
  end;

implementation

uses
  GApp
  ;

{ TBinanceManager }

constructor TUpbitManager.Create(aExType: TExchangeKind);
begin
  inherited Create( aExType );

end;

destructor TUpbitManager.Destroy;
begin

  inherited;
end;



function TUpbitManager.InitMarketWebSockets: boolean;
begin
  Result := true;
end;

function TUpbitManager.SubscribeAll: boolean;
begin
  Result := true;
end;

procedure TUpbitManager.UnSubscribeAll;
begin
  inherited;

end;

//function TUpbitManager.RequestMaster: boolean;
//var
//  I: TMarketType;
//begin
//  for I := mtSpot to High(TMarketType) do
//  begin
//    if Exchanges[i] = nil then continue;
//    if not Exchanges[i].RequestMaster then
//      Exit (false);
//  end;
//
//  Result := true;
//
//end;



end.
