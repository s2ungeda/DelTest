unit UUpbitManager;

interface

uses
  System.Classes, System.SysUtils, System.DateUtils,

  UExchangeManager,  USymbols, UOrders, UUpbitParse,

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
    function MakeCloseData : boolean ; override;

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
  , URestRequests
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
  i, iTot, iCount, iQty : integer;
begin
  iTot:= 1;
  SetLength( QuoteSock, iTot );

  for I := 0 to iTot-1 do begin
    QuoteSock[i]  := TUpbitWebSocket.Create(QOUTE_SOCK,i, mtSpot ) ;
    QuoteSock[i].init( 'api.upbit.com/websocket/v1' );
  end;

//  iCount := App.Engine.SymbolCore.Symbols[ekUpbit].Count;
//
//  var iCnt, iMod, j : integer;
//  iCnt := iCount div iTot;
//  iMod := iCount mod iTot;
//
//
//
//  j := 0;   iQty := iCnt;
//  for I := 0 to iTot-1 do begin
//    if i = iTot -1 then
//      iQty := iQty + iMod;
//    QuoteSock[i].SetIndex(j, iQty);
//    App.DebugLog('setindex : %d, %d, %d',[ i, j, iQty]);
//    QuoteSock[i].init( 'api.upbit.com/websocket/v1' );
//    inc( j, iCnt );
//    inc( iQty, iCnt);
//  end;

  Timer.OnTimer := OnTimer;
  Timer.Interval:= 1000;
  Result := true;
end;

function TUpbitManager.MakeCloseData: boolean;
var
  sTmp : string;
begin
  stmp := App.Engine.SymbolCore.MainSymbols[msBTC][ExchangeKind].OrgCode;
  Result := Exchanges[mtSpot].RequestCandleData('240', sTmp )
    and Exchanges[mtSpot].RequestCandleData('30', sTmp )    ;

  stmp := App.Engine.SymbolCore.MainSymbols[msETH][ExchangeKind].OrgCode;
  Result := Exchanges[mtSpot].RequestCandleData('240', sTmp )
    and Exchanges[mtSpot].RequestCandleData('30', sTmp )    ;
end;

procedure TUpbitManager.OnTimer(Sender: TObject);
var
  i, j : integer;
  a : TRequest;
begin
  //(Exchanges[mtSpot] as TBithSpot).RequestData(1);
  //inc( FCount);
//  j := 0;
//  for i:=0 to Exchanges[mtSpot].ReqItems.Count-1 do
//  begin
//    a := TRequest(Exchanges[mtSpot].ReqItems.Items[i]);
//    j := j +  a.SndCnt ;
//    a.SndCnt  := 0;
//  end;
//
//  App.Log(llInfo,'rate', '%s req cnt : %d', [  TExchangeKindDesc[ExchangeKind] , j ] )  ;

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
  (QuoteSock[QOUTE_SOCK] as TUpbitWebSocket).UnSubScribe( aSymbol);
  Result := true;
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
