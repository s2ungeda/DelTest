unit UBinanceManager;

interface

uses
  System.Classes, System.SysUtils, System.DateUtils,

  UExchangeManager, USymbols, UBinanceParse

  ,UApiTypes

  ;

type

  TBinanceManager = class( TExchangeManager )
  private
    FParse: TBinanceParse;
  public

    Constructor  Create( aExType : TExchangeKind );
    Destructor  Destroy; override;

    function InitMarketWebSockets : boolean ; override;
    function SubscribeAll : boolean; override;
    procedure UnSubscribeAll ; override;

    property Parse : TBinanceParse read FParse;
  end;

implementation

uses
  GApp
  , UApiConsts
  , UBinanceWebSockets
  ;

{ TBinanceManager }

constructor TBinanceManager.Create(aExType: TExchangeKind);
begin
  inherited Create( aExType );

  FParse:= TBinanceParse.Create;
end;

destructor TBinanceManager.Destroy;
//var
//  i : integer;
begin
//
//  for I := 0 to High(QuoteSock) do
//    QuoteSock[i].Free;

  FParse.Free;
  inherited;
end;

function TBinanceManager.InitMarketWebSockets: boolean;
var
  iCount , iMode, i, j : integer;
  aList : TStringList;
  aSymbol : TSymbol;
begin
  iCount := App.Engine.SymbolCore.Spots[ekBinance].Count div 10;
  iMode  := App.Engine.SymbolCore.Spots[ekBinance].Count mod 10;

  if iMode > 0 then inc( iCount );
  if iCount <= 0 then Exit (false);

  inc(iCount); // 바이낸스는 선물꺼 하나..추가.
  SetLength( QuoteSock, iCount );
  App.DebugLog('Binance Quote Sock %d.th Created', [ iCount ]);

  for I := 0 to iCount-1 do
    if i = iCount-1 then
    begin
      QuoteSock[i]  := TBinanceWebSocket.Create(QOUTE_SOCK, mtFutures ) ;
      QuoteSock[i].init(i, 'fstream.binance.com/ws' );
    end else
    begin
      QuoteSock[i]  := TBinanceWebSocket.Create(QOUTE_SOCK, mtSpot ) ;
      QuoteSock[i].init(i, 'stream.binance.com:9443/ws' );
    end;


  aList := TStringList.Create;
  try
    J := 0;
    for i:=0 to App.Engine.SymbolCore.Spots[ekBinance].Count-1 do
    begin
      aSymbol := App.Engine.SymbolCore.Spots[ekBinance].Spots[i];
      aList.Add( aSymbol.OrgCode  );
      if aList.Count = 10 then
      begin
        (QuoteSock[j] as TBinanceWebSocket).SetSubList( aList );
        aList.Clear;
        inc(j);
      end;
    end;

    if aList.Count > 0 then
    begin
      (QuoteSock[j] as TBinanceWebSocket).SetSubList( aList );
       inc(j);
    end;

    aList.Clear;

    App.DebugLog('Binance Spot SetSubList (%d/%d)', [ j-1, High(QuoteSock)] );

    for i:=0 to App.Engine.SymbolCore.Futures[ekBinance].Count-1 do
    begin
      aSymbol := App.Engine.SymbolCore.Futures[ekBinance].Futures[i];
      aList.Add( aSymbol.OrgCode );
    end;

    if aList.Count > 0 then
    begin
      (QuoteSock[j] as TBinanceWebSocket).SetSubList( aList );
       App.DebugLog('Binance Futures SetSubList (%d/%d)', [ j, High(QuoteSock)] );
    end;

  finally
    aList.Free;
  end;

  Result := true;
end;

function TBinanceManager.SubscribeAll: boolean;
var
  i, j : integer;
begin
//  Exit (true);
//  QuoteSock[0].DoConnect;
  for I := 0 to High(QuoteSock) do
    QuoteSock[i].DoConnect;
  result := true;
end;

procedure TBinanceManager.UnSubscribeAll;
begin
  inherited;

end;

//function TBinanceManager.RequestMaster: boolean;
//var
//  I: TMarketType;
//begin
//  for I := mtSpot to High(TMarketType) do
//    if not Exchanges[i].RequestMaster then
//      Exit (false);
//
//  Result := true;
//end;



end.
