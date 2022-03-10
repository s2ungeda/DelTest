unit UUpbitManager;

interface

uses
  System.Classes, System.SysUtils, System.DateUtils,

  UExchangeManager,  USymbols, UUpbitParse,

  UApiTypes

  ;

type

  TUpbitManager = class( TExchangeManager )
  private
    FParse: TUpbitParse;
    procedure OnTimer( Sender : TObject );
  public
    Constructor  Create( aExType : TExchangeKind );
    Destructor  Destroy; override;
//    function RequestMaster : boolean; override;
    function InitMarketWebSockets : boolean ; override;
    function SubscribeAll : boolean; override;
    procedure UnSubscribeAll ; override;

    function Subscrib( aSymbol : TSymbol ) : boolean; override;
    function UnSubscrib( aSymbol : TSymbol ) : boolean; override;

    property Parse : TUpbitParse read FParse;
  end;

implementation

uses
  GApp
  , UApiConsts
  , UUpbitSpot
  , UUpbitWebSockets
  ;

{ TBinanceManager }

constructor TUpbitManager.Create(aExType: TExchangeKind);
begin
  inherited Create( aExType );
  FParse:= TUpbitParse.Create( self );

end;

destructor TUpbitManager.Destroy;
begin
  FParse.Free;
  inherited;
end;



function TUpbitManager.InitMarketWebSockets: boolean;
var
  i, iCount : integer;
begin
  iCount := 1;
  SetLength( QuoteSock, iCount );

  for I := 0 to iCount-1 do begin
    QuoteSock[i]  := TUpbitWebSocket.Create(QOUTE_SOCK,i, mtSpot ) ;
    QuoteSock[i].init( 'api.upbit.com/websocket/v1' );
  end;

  Timer.OnTimer := OnTimer;
  Result := true;
end;

procedure TUpbitManager.OnTimer(Sender: TObject);
begin
  Exchanges[mtSpot].RequestDNWState;
end;

function TUpbitManager.Subscrib(aSymbol: TSymbol): boolean;
begin
  Result := false;
  try
    (QuoteSock[QOUTE_SOCK] as TUpbitWebSocket).SubScribe( aSymbol);
    Result := true;
  except

  end;
end;


function TUpbitManager.UnSubscrib(aSymbol: TSymbol): boolean;
begin

end;

procedure TUpbitManager.UnSubscribeAll;
begin
  inherited;

end;
function TUpbitManager.SubscribeAll: boolean;
begin
  Timer.Enabled := true;
  Result := true;
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
