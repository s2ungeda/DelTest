unit UApiTypes;

interface

type
  TExchangeInfo = record
    Name : string;
    Code : string;
    IsMargin : boolean;
    IsFuture : boolean;
    IsDomestic : boolean;
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
