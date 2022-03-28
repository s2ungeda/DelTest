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
var
  i, iState : integer;
begin
  Exchanges[mtSpot].RequestDNWState;


  for I := 0 to High(QuoteSock) do
  begin
    iState := integer( QuoteSock[i].WebSocket.State );
    if  iState = 4 then
    begin
      QuoteSock[i].DoConnect;
 //     QuoteSock[i].
      App.DebugLog('%s, %d %s reconnect ', [ TExchangeKindDesc[ExchangeKind],i, QuoteSock[i].GetSockType ] );

      if QuoteSock[i].GetSockState = 'Open' then
      begin
        QuoteSock[i].SubscribeAll;
        App.Log(llInfo, '%s %.dth %s SubscribeAll ', [ TExchangeKindDesc[ExchangeKind],i, QuoteSock[i].GetSockType ] );
      end;
    end;
  end;
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
var
  i : Integer;
begin
  for I := 0 to High(QuoteSock) do
    QuoteSock[i].SubscribeAll;

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
