unit UBithManager;

interface

uses
  System.Classes, System.SysUtils, System.DateUtils,

  UExchangeManager,

  UApiTypes

  ;

type

  TBithManager = class( TExchangeManager )
  public
    Constructor  Create( aExType : TExchangeKind );
    Destructor  Destroy; override;

//    function RequestMaster : boolean; override;
  end;

implementation

uses
  GApp
  ;
{ TBinanceManager }

constructor TBithManager.Create(aExType: TExchangeKind);
begin
  inherited Create( aExType );

end;

destructor TBithManager.Destroy;
begin

  inherited;
end;



//function TBithManager.RequestMaster: boolean;
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
