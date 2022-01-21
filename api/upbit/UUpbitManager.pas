unit UUpbitManager;

interface

uses
  System.Classes, System.SysUtils, System.DateUtils,

  UExchangeManager,

  UApiTypes

  ;

type

  TUpbitManager = class( TExchangeManager )
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

constructor TUpbitManager.Create(aExType: TExchangeKind);
begin
  inherited Create( aExType );

end;

destructor TUpbitManager.Destroy;
begin

  inherited;
end;

function TUpbitManager.PrepareMaster: boolean;
begin
  App.Log( llDebug, '', 'TUpbitManager.PrepareMaster');
end;

end.
