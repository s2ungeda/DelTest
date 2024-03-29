unit UApiTypes;

interface

type


  TExchangeKind   = ( ekBinance , ekUpbit, ekBithumb );
  TMarketType     = ( mtSpot, mtFutures );
  TAccountMarketType  = (amAll, amSpot, {amMargin,} amFuture );
  TMajorSymbolKind = (msBTC, msETH, msXRP );
  TRequestType    = ( rtNewOrder , rtCnlOrder, rtOrderList, rtPosition,
  	rtBalance, rtAbleOrder, rtOrdDetail, rtTradeAmt,
    rtOrderBook
     );

  // account settle currency
  TSettleCurType = ( scNone, scKRW, scUSDT, scBTC );


	// 소켓, Rest 구분을 위해..
  TDivInfo = record
 		Kind 		: TExchangeKind;
    Market 	: TMarketType;
    //Division: integer;    	// 0 : public   1 : 주문   2 : 주문외 privatge
    Index		: integer;     
    WaitTime: integer;
  end;
  

  TApiInfo = record
    BaseUrl    : string;
    Prepare    : string;
    Port       : integer;
    MarketType : TMarketType;
    Key, Secret : string;
  end;

  TExchangeInfo = record
    Name : string;
    Code : string;
    IsMargin : boolean;
    IsFuture : boolean;
    IsDomestic : boolean;
    Index   : integer;

    MarketInfo : array [ TMarketType ] of TApiInfo;

    procedure SetInfo( i:integer; stName : string ; isMar,isFut, isDome : boolean );
  end;


//  TExAccountInfo = record
//    Code, Name, Key, Secret : string;
//    ExType : TExchangeKind;
//  end;

  function GetSettleType( sCur : string): TSettleCurType;
  function ExKindToStr( aKind : TExchangeKind ) : string;
  function MarketToStr( aType : TMarketType ) : string;
  function ExistExType( aExStr : string; var aExKind : TExchangeKind   ) : boolean;

implementation

uses
  UApiConsts
  ;
{ TExchangeInfo }

procedure TExchangeInfo.SetInfo(i:integer; stName: string; isMar, isFut, isDome: boolean);
begin
  Index := i;
  Name  := stName  ;
  IsMargin := isMar;
  IsFuture := isFut;
  IsDomestic := isDome;
end;            


function GetSettleType( sCur : string): TSettleCurType;
begin
  if sCur = 'KRW' then
    Result := scKRW
  else if sCur = 'USDT' then
    Result := scUSDT
  else if sCur = 'BTC' then
    Result := scBTC
  else Result := scNone;
end;

function ExKindToStr( aKind : TExchangeKind ) : string;
begin
  Result := TExchangeKindDesc[ aKind ];
end;

function MarketToStr( aType : TMarketType ) : string;
begin
  Result := TMarketTypeDesc[ aType ];
end;

function ExistExType( aExStr : string; var aExKind : TExchangeKind   ) : boolean;
var
  i : TExchangeKind;
begin
  Result := false;

  for I := ekBinance to High(TExchangeKindShortDesc) do
    if TExchangeKindShortDesc[i] = aExStr then
    begin
      aExKind := i;
      Result  := true;
      break;
    end;
end;

end.
