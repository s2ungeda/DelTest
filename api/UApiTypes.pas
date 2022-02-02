unit UApiTypes;

interface

type


  TExchangeKind   = ( ekBinance , ekUpbit, ekBithumb );
  TMarketType     = ( mtSpot, mtFutures );
  TAccountMarketType  = ( amSpot, amMargin, amFuture );

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

const

  TExchangeKindDesc : array [ TExchangeKind ] of string = ('Binace', 'Upbit', 'Bithumb');
  TMarketTypeDesc : array [ TMarketType ] of string = ('Spot', 'Future');
  TAccountMarketTypeDesc : array [ TAccountMarketType ] of string = ('Spot', 'Margin', 'Future');

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
