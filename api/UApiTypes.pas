unit UApiTypes;

interface

type


  TExchangeKind   = ( ekBinance , ekUpbit, ekBithumb );
  TMarketType     = ( mtSpot, mtFutures );
  TAccountMarketType  = (amAll, amSpot, amMargin, amFuture );

  TMajorSymbolKind = (msBTC, msETH, msXRP );

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

    Key : string;
    Secret  : string;    
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


implementation


{ TExchangeInfo }

procedure TExchangeInfo.SetInfo(i:integer; stName: string; isMar, isFut, isDome: boolean);
begin
  Index := i;
  Name  := stName  ;
  IsMargin := isMar;
  IsFuture := isFut;
  IsDomestic := isDome;
end;            


end.
