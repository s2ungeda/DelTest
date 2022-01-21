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

    function PrepareMaster : boolean; override;
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

function TBithManager.PrepareMaster: boolean;
begin
  App.Log( llDebug, '', 'TBithManager.PrepareMaster');
end;

end.
