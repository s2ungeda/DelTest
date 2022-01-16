unit UApiTypes;

interface

type


  TExchangeKind       = ( ekBinance , ekUpbit, etBitthumb );
  TExchangeMarketType = ( emtSpot, emtMargin, emtFuture );


  TExchangeInfo = record
    Name : string;
    Code : string;
    IsMargin : boolean;
    IsFuture : boolean;
    IsDomestic : boolean;

    BaseUrl    : string;
    Port       : integer;
    MarketType : TExchangeMarketType;

    Key : string;
    Secret  : string;

    procedure SetInfo( stCode : string ; isMar,isFut, isDome : boolean );
  end;

implementation

{ TExchangeInfo }

procedure TExchangeInfo.SetInfo(stCode: string; isMar, isFut, isDome: boolean);
begin
  Code := stCode;
  IsMargin := isMar;
  IsFuture := isFut;
  IsDomestic := isDome;
end;

end.
