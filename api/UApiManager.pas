unit UApiManager;

interface

uses
  System.Classes, System.SysUtils, System.DateUtils,
  UExchangeManager, UExchangeRate, UQuoteBroker,
  UApiTypes
  ;
type
  TExchangeArray = array [ TExchangeKind ] of TExchangeManager;
  TApiManager = class
  private
    FExManagers: TExchangeArray;
    FCommCodes: TStrings;
    FExRate: TExchangeRate;
    FSockState: TStrings;
    function GetExManager: integer;
    function RequestMaster : boolean;
    function LoadMaster( sMasterFile : string ) : boolean;
    function GetCodesIntersection : boolean;

    procedure MasterLog;
  public
    Constructor Create;
    Destructor  Destroy; override;

    function GetMaster : boolean;
    function PrepareMaster : boolean;
    function InitMarketWebSocket : boolean;
    function SubscribeAll : boolean;
    function ConnectAll: boolean;    
    function DisConnectAll : boolean;

    function RequestBalance : boolean; 
    function RequestPositons : boolean;
    function RequestOrders: boolean;

    procedure StartRequest;

    procedure MakeCloseData;
    procedure RequestExRate;
    procedure CheckCount;


    function Sub(aQuote: TQuote) : boolean;
    function UnSub(aQuote:TQuote): boolean;

    property ExManagers  : TExchangeArray read FExManagers;
    property ExManagerCount : integer read GetExManager;
    //
    property ExRate : TExchangeRate read FExRate;
    // 국내 거래소간 공통 코드
    property CommCodes : TStrings read FCommCodes;
    //
    property  SockState : TStrings read FSockState;

  end;

implementation

uses
  GApp,
  UApiConsts,
  UBinanceManager ,  UUpbitManager, UBithManager
  ;
{ TApiManager }
procedure TApiManager.CheckCount;
var
  I : TExchangeKind;
  j : integer;
begin
  FSockState.Clear;
  for I := ekBinance to High(TExchangeKind) do
    for j := 0 to high(FExManagers[i].QuoteSock) do
    begin
      if FExManagers[i].QuoteSock[j].GetSockState <> 'Open' then
      begin
        FSockState.Add( Format('%s-%d(X)', [ TExchangeKindDesc[i], j ] ));
      end;
    end;
end;


constructor TApiManager.Create;
var
  I: TExchangeKind;
begin
  for I := ekBinance to High(TExchangeKind) do
    case i of
      ekBinance: FExManagers[i] := TBinanceManager.Create(i) as TExchangeManager ;
      ekUpbit: FExManagers[i]   := TUpbitManager.Create(i) as TExchangeManager;
      ekBithumb: FExManagers[i]:= TBithManager.Create(i) as TExchangeManager;
    end;

  FCommCodes:= TStringList.Create;
  FSockState:= TStringList.Create;
  FExRate:= TExchangeRate.Create(self, mtSpot );
end;
destructor TApiManager.Destroy;
var
  I: TExchangeKind;
begin
  FCommCodes.Free;
  FExRate.Free;
  FSockState.Free;

  for I := ekBinance to High(TExchangeKind) do
  begin
    FExManagers[i].Free;
  end;

  inherited;
end;



function TApiManager.GetCodesIntersection: boolean;
var
  s : string;
  iRes : integer;
  aList: TStrings;
  I: TExchangeKind;
begin

  aList := TStringList.Create;
  try
    // binance spot future 교집합..
    for s in FExManagers[ekBinance].Exchanges[mtSpot].Codes do
    begin
      iRes := FExManagers[ekBinance].Exchanges[mtFutures].Codes.IndexOf(s);
      if iRes >= 0 then
        aList.Add(s);
    end;
    FExManagers[ekBinance].Exchanges[mtSpot].Codes.Clear;
    FExManagers[ekBinance].Exchanges[mtSpot].Codes.Assign( aList );
    aList.Clear;

    // upbit 와 binance 교집합
    for s in FExManagers[ekUpbit].Exchanges[mtSpot].Codes do
    begin
      iRes := FExManagers[ekBinance].Exchanges[mtSpot].Codes.IndexOf(s);
      if iRes >= 0 then
        aList.Add(s);
    end;
    // 업비트 와 바이낸스에 있는 공통 종목을 다시 담는다.
    FExManagers[ekUpbit].Codes.Assign(aList);

    aList.Clear;
    // bithumb 와 binance 교집합
    for s in FExManagers[ekBithumb].Exchanges[mtSpot].Codes do
    begin
      iRes := FExManagers[ekBinance].Exchanges[mtSpot].Codes.IndexOf(s);
      if iRes >= 0 then
        aList.Add(s);
    end;
    // 빗썸 과 바이낸스에 있는 공통 종목을 다시 담는다.
    FExManagers[ekBithumb].Codes.Assign( aList );
    FExManagers[ekBinance].Codes.Assign( aList );
    //  alist 에는 빗썸과 바이낸스의 교집합..
    for s in FExManagers[ekUpbit].Codes do
    begin
      iRes := ExManagers[ekBinance].Codes.IndexOf(s);
      if iRes < 0 then
        FExManagers[ekBinance].Codes.Add(s)
      else
        FCommCodes.Add(s);
    end;

    s := '';
    for I := ekBinance to High(TExchangeKind) do
      s := Format('%s %s:%d', [ s, TExchangeKindDesc[i], FExManagers[i].Codes.Count ] );

    s := Format('%s Common:%d', [ s, FCommCodes.Count ] );

    App.Log(llDebug, '', 'merge result : %s', [ s ] );

    Result := true;

  finally
    aList.Free;
  end;

end;

function TApiManager.GetExManager: integer;
begin
  Result := Integer(high(  TExchangeKind ));
end;

function TApiManager.GetMaster: boolean;
var
  sFileName : string;
begin
  // 오늘자 파일 체크..
  sFileName := Format('%s_Master.txt', [ FormatDateTime('yyyymmdd', date) ] );
  if FileExists( sFileName ) then
    Result := LoadMaster( sFileName )
  else begin
    if PrepareMaster then
      Result := RequestMaster ;
  end;

  if Result  then
  begin
    App.Engine.SymbolCore.SymbolArrange;
    MasterLog;
  end;
end;


function TApiManager.LoadMaster(sMasterFile: string): boolean;
begin

end;

procedure TApiManager.MakeCloseData;
  // 거래소별 과거 분봉 데이터를 통해  데이터 생성..
var
  i : TExchangeKind;
begin
  for I := ekBinance to High(TExchangeKind) do
  begin
    if not FExManagers[i].MakeCloseData then
      Exit;
  end;

  App.Engine.SymbolCore.MakePrevData;

end;

procedure TApiManager.MasterLog;
begin
  App.Engine.SymbolCore.Log;
end;

function TApiManager.PrepareMaster: boolean;
var
  i : TExchangeKind;
begin
  for I := ekBinance to High(TExchangeKind) do
  begin
    if not FExManagers[i].PrepareMaster then
      Exit (false);
  end;

  Result := GetCodesIntersection;
end;


{
TApiManager.RequestMaster
	↓
TBinanceManager.RequestMaster
	↓
TBinanceSpotNMargin.RequestMaster
	↓
TBinanceFutures.RequestMaster
}


procedure TApiManager.RequestExRate;
begin
  FExRate.RequestData;
end;

function TApiManager.RequestMaster: boolean;
var
  i :  TExchangeKind;
begin
  for I := ekBinance to High(TExchangeKind) do
  begin
    if not FExManagers[i].RequestMaster then
      Exit (false);
  end;

  Result := true;
end;



procedure TApiManager.StartRequest;
var
  i :  TExchangeKind;
begin
  //FExManagers[ekUpbit].StartRequest;
  //FExManagers[ekBithumb].StartRequest;
  //FExManagers[ekBinance].StartRequest;
  //exit;
  for I := ekBinance to High(TExchangeKind) do
  	FExManagers[i].StartRequest;
end;

function TApiManager.Sub(aQuote: TQuote): boolean;
begin
  if aQuote = nil then Exit (false);
  Result := ExManagers[aQuote.Symbol.Spec.ExchangeType].Subscrib(aQuote.Symbol);
end;

function TApiManager.UnSub(aQuote: TQuote): boolean;
begin
  if aQuote = nil then Exit (false);
  Result := ExManagers[aQuote.Symbol.Spec.ExchangeType].UnSubscrib(aQuote.Symbol);
end;

function TApiManager.SubscribeAll: boolean;
var
  i :  TExchangeKind;
begin

  for I := ekBinance to High(TExchangeKind) do
  begin
    if not FExManagers[i].SubscribeAll then
      Exit (false);
    sleep(100);
  end;      
  Result := true;

end;



function TApiManager.InitMarketWebSocket: boolean;
var
  i :  TExchangeKind;
begin

  for I := ekBinance to High(TExchangeKind) do
  begin
    FExManagers[i].init;
    if not FExManagers[i].InitMarketWebSockets then
      Exit (false);
  end;

  Result := true;

end;

function TApiManager.ConnectAll: boolean;
var
  i :  TExchangeKind;
begin

  for I := ekBinance to High(TExchangeKind) do
  begin
    if not FExManagers[i].ConnectAll then
      Exit (false);
    //sleep(200);
  end;

  Result := true;

end;


function TApiManager.DisConnectAll: boolean;
var
  i :  TExchangeKind;
begin

  for I := ekBinance to High(TExchangeKind) do
  begin
    if FExManagers[i] <> nil then begin
      if i = ekBinance then
        FExManagers[i].UnSubscribeAll;
      if not FExManagers[i].DissConnectAll then
        Exit (false);
    end;
  end;

  Result := true;
end;

function TApiManager.RequestBalance: boolean;
var
  i :  TExchangeKind;
begin

	Result := FExManagers[ekBinance].RequestBalance and
            FExManagers[ekBithumb].RequestBalance;
  Exit;
  

  for I := ekBinance to High(TExchangeKind) do
  begin
    if not FExManagers[i].RequestBalance then
      Exit (false);    
  end;

  Result := true;

end;

function TApiManager.RequestOrders;
var
  i :  TExchangeKind;
begin

	Result := FExManagers[ekBithumb].RequestOrders;
  Exit;

  for I := ekBinance to High(TExchangeKind) do
  begin
    if not FExManagers[i].ConnectAll then
      Exit (false);        
  end;

  Result := true;

end;

function TApiManager.RequestPositons;
var
  i :  TExchangeKind;
begin

  Exit;
	Result := FExManagers[ekBithumb].RequestBalance;


  for I := ekBinance to High(TExchangeKind) do
  begin
    if not FExManagers[i].ConnectAll then
      Exit (false);    
  end;

  Result := true;

end;


end.
