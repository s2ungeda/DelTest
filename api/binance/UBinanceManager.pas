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

    function PrepareMaster : boolean;  override;
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

function TBinanceManager.PrepareMaster: boolean;
begin

  Result := Exchanges[emSpot].PrepareMaster;


//  App.Log( llDebug, '', 'TBinanceManager.PrepareMaster');


end;

end.
