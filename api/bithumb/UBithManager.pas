unit UBithManager;

interface

uses
  System.Classes, System.SysUtils, System.DateUtils,

  UExchangeManager, UBithParse , USymbols,  UQuoteTimers,

  UApiTypes

  ;

type

  TBithManager = class( TExchangeManager )
  private
    FParse: TBithParse;
    FDepTimer: TQuoteTimer;

    procedure OnTimer( Sender : TObject );
  public

    Constructor  Create( aExType : TExchangeKind );
    Destructor  Destroy; override;

    function InitMarketWebSockets : boolean ; override;
    function SubscribeAll : boolean; override;
    procedure UnSubscribeAll ; override;

    function Subscrib( aSymbol : TSymbol ) : boolean; override;
    function UnSubscrib( aSymbol : TSymbol ) : boolean; override;

    procedure OnDepthTimer(Sender: TObject);
    procedure OnSendDoneEvent(  Sender : TObject );

    property Parse : TBithParse read FParse;
    property DepTimer : TQuoteTimer read FDepTimer;
  end;

implementation

uses
  GApp
  , UApiConsts
  , UBithSpot
  , UBithWebSockets
  ;
{ TBinanceManager }

constructor TBithManager.Create(aExType: TExchangeKind);
begin
  inherited Create( aExType );
  FParse  := TBithParse.Create( self );
  FParse.OnSendDone := OnSendDoneEvent;

end;

destructor TBithManager.Destroy;
begin
  if FDepTimer <> nil then
    FDepTimer.Enabled := false;
  FParse.Free;
  inherited;
end;


const divCnt = 40;
function TBithManager.InitMarketWebSockets: boolean;
var
  i, j, iCount : integer;
//  aList : TStringList;
//  aSymbol : TSymbol;
begin

  iCount := 1;
  SetLength( QuoteSock, iCount );

  for I := 0 to iCount-1 do begin
    QuoteSock[i]  := TBithWebSocket.Create(QOUTE_SOCK, i, mtSpot ) ;
    QuoteSock[i].init('pubwss.bithumb.com/pub/ws' );
  end;

  Result := true;

  // orderbook 을 위한 조회 타이머.
  FDepTimer  := App.Engine.QuoteBroker.Timers.New;
  FDepTimer.Enabled  := false;
  FDepTimer.Interval := 500;
  FDepTimer.OnTimer  := OnDepthTimer;

  Timer.OnTimer   := OnTimer;
end;

procedure TBithManager.OnDepthTimer(Sender: TObject);
begin

  if Done then
  begin
    FDepTimer.Enabled := false;
    Exit;
  end;

  (Exchanges[mtSpot] as TBithSpot).RequestOrderBook('1');
end;


procedure TBithManager.OnTimer(Sender: TObject);
begin
  Exchanges[mtSpot].RequestDNWState;
end;


procedure TBithManager.OnSendDoneEvent(Sender: TObject);
begin
  if Sender = FParse then
    (QuoteSock[QOUTE_SOCK] as TBithWebSocket).Send;
end;


function TBithManager.Subscrib(aSymbol: TSymbol): boolean;
begin
//  self.Exchanges[ aSymbol.Spec.Market ].
  Result := false;
  try
    (QuoteSock[QOUTE_SOCK] as TBithWebSocket).SubScribe( aSymbol);
    Result := true;
  except

  end;

end;

function TBithManager.UnSubscrib(aSymbol: TSymbol): boolean;
begin
  Result := true;
end;


function TBithManager.SubscribeAll: boolean;
var
  i, j : integer;
begin
  //Exit (true);

  for I := 0 to High(QuoteSock) do
    QuoteSock[i].SubscribeAll;
  result := true;

  FDepTimer.Enabled := true;
end;


procedure TBithManager.UnSubscribeAll;
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
