unit UExchangeManager;
interface
uses
  system.Classes, system.SysUtils,
  UExchange     ,
  UApiTypes
  ;
type
  TMarketArray = array [ TMarketType ] of TExchange;

  TExchangeManager = class
  private
    FExchanges: TMarketArray;
    FExchangeType: TExchangeKind;
    FExchangeIdx: integer;
    FCodes: TStrings;
    function GetMarketCount: integer;
    function CreateMarket( aMarket : TMarketType ) :  TExchange;
  public
    Constructor Create( aExType : TExchangeKind ); overload;
    Destructor  Destroy; override;

    // 공통으로 같이 처리할수 있는것은 상속 처리 하지 않음..
    // 1. preparemaster
    // 2. requestmaster
    function PrepareMaster : boolean; //virtual; abstract;
    function RequestMaster : boolean;

    // 상속 받아서 각 거래소 매니저에서 처리

    //  TExchangeMarketType
    property Exchanges   : TMarketArray read FExchanges write FExchanges;
    property MarketCount : integer read GetMarketCount;
    property ExchangeType: TExchangeKind read FExchangeType;
    property ExchangeIdx : integer read FExchangeIdx;
    // 교집합 코드를 담을 리스트..
    property Codes       : TStrings read FCodes;
  end;

implementation
uses
  GApp,
  UBinanceSpotNMargin, UBinanceFutures ,
  UBithSpot, UUpbitSpot
  ;

{ TExchangeManaager }
constructor TExchangeManager.Create(aExType: TExchangeKind);
var
  i : TMarketType;
begin
  FExchangeType := aExType;
  FExchangeIdx  := integer( FExchangeIdx );
  
  for I := mtSpot to High(TMarketType) do
  begin
    FExchanges[i] :=  CreateMarket( i);
  end;

  FCodes:= TStringList.Create;
end;

function TExchangeManager.CreateMarket(aMarket: TMarketType): TExchange;
begin
  Result := nil;  
  case FExchangeType of
    ekBinance:
      begin
        case aMarket of
          mtSpot  : Result := TBinanceSpotNMargin.Create( self, aMarket) ;
          mtFutures: Result := TBinanceFutures.Create( self, aMarket) ;
        end;
      end;                 
    ekUpbit:
      begin
        case aMarket of
          mtSpot  : Result := TUpbitSpot.Create( self, aMarket) ;
        end;

      end;
    ekBithumb:
      begin
        case aMarket of
          mtSpot  : Result := TBithSpot.Create( self, aMarket) ;
        end;     
      end;                       
  end;
end;

destructor TExchangeManager.Destroy;
var
  i : TMarketType;
begin

  FCodes.Free;
  for I := mtSpot to High(TMarketType) do
  begin
    if FExchanges[i] <> nil then
      FExchanges[i].Free;
  end;
  inherited;
end;

function TExchangeManager.GetMarketCount: integer;
begin
  Result := Integer(High(TMarketType));
end;

function TExchangeManager.PrepareMaster: boolean;
var
  I: Integer;
  sTmp : string;
begin
  Result := Exchanges[mtSpot].PrepareMaster;
  App.Log(llDebug, '', ' ---------- %s codes count %d -----------',
    [ TExchangeKindDesc[FExchangeType], Exchanges[mtSpot].Codes.Count ] );

end;

function TExchangeManager.RequestMaster: boolean;
var
  I: TMarketType;
begin
  for I := mtSpot to High(TMarketType) do
  begin
    if Exchanges[i] = nil then continue;
    if not Exchanges[i].RequestMaster then
      Exit (false);
  end;

  Result := true;

end;

end.
