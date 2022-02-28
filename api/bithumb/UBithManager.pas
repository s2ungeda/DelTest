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
    FTimer: TQuoteTimer;
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
    property Timer : TQuoteTimer read FTimer;
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
  FParse  := TBithParse.Create( aExtype );
  FParse.OnSendDone := OnSendDoneEvent;
end;

destructor TBithManager.Destroy;
begin
  FTimer.Enabled := false;
  FParse.Free;
  inherited;
end;


const divCnt = 40;
function TBithManager.InitMarketWebSockets: boolean;
var
  iCount , iMode, i, j : integer;
  aList : TStringList;
  aSymbol : TSymbol;
begin
//  iCount := App.Engine.SymbolCore.Spots[ExchangeType].Count div divCnt;
//  iMode  := App.Engine.SymbolCore.Spots[ExchangeType].Count mod divCnt;
////
//  if iMode > 0 then inc( iCount );
//  if iCount <= 0 then Exit (false);
////
//  SetLength( QuoteSock, iCount );
//  App.DebugLog('Bithumb Quote Sock %d.th Created', [ iCount ]);
////
//  for I := 0 to iCount-1 do begin
//    QuoteSock[i]  := TBithWebSocket.Create(QOUTE_SOCK, mtSpot ) ;
//    QuoteSock[i].init(i, 'pubwss.bithumb.com/pub/ws' );
//  end;
//
//  aList := TStringList.Create;
//  try
//    J := 0;
//    for i:=0 to App.Engine.SymbolCore.Spots[ExchangeType].Count-1 do
//    begin
//      aSymbol := App.Engine.SymbolCore.Spots[ExchangeType].Spots[i];
//      aList.Add( aSymbol.OrgCode  );
//      if aList.Count = divCnt then
//      begin
//        (QuoteSock[j] as TBithWebSocket).SetSubList( aList );
//        aList.Clear;
//        inc(j);
//      end;
//    end;
//
//    if aList.Count > 0 then
//      (QuoteSock[j] as TBithWebSocket).SetSubList( aList );
//
//    App.DebugLog('Bithumb Spot SetSubList (%d/%d)', [ j, High(QuoteSock)] );
//  finally
//    aList.Free;
//  end;

  iCount := 1;
  SetLength( QuoteSock, iCount );

  for I := 0 to iCount-1 do begin
    QuoteSock[i]  := TBithWebSocket.Create(QOUTE_SOCK, mtSpot ) ;
    QuoteSock[i].init(i, 'pubwss.bithumb.com/pub/ws' );
  end;

  Result := true;

  // orderbook 을 위한 조회 타이머.
  FTimer  := App.Engine.QuoteBroker.Timers.New;
  FTimer.Enabled  := false;
  FTimer.Interval := 300;
  FTimer.OnTimer  := OnDepthTimer;
end;

procedure TBithManager.OnDepthTimer(Sender: TObject);
begin
//  App.DebugLog('-------------  Depth Timer -----------');
  (Exchanges[mtSpot] as TBithSpot).RequestOrderBook('1');
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
    QuoteSock[i].DoConnect;
  result := true;

  FTimer.Enabled := true;
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
