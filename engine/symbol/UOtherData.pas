unit UOtherData;

interface

uses
  System.Classes, System.SysUtils

  , UApiTypes

  ;

type

  TWCDData = class( TCollectionItem )
  private
    FTimStr  : string;
    FName: string;

  public
    Price : array [TMajorSymbolKind, TExchangeKind ] of double;
    Amount: array [TMajorSymbolKind, TExchangeKind ] of double;
    WCDPrice: array [TExchangeKind ] of double;
    property Name : string read FName write FName;
    property TimStr : string read FTimStr write FTimStr;
  end;

  TWCDDataList = class( TCollection )
  private
    function GetWCDS(i: integer): TWCDData;
  public
    Constructor Create;
    function New( sTime : string ): TWCDData;
    function Find( sTime : string ) : TWCDData;
    property WCDs[ i : integer] : TWCDData read GetWCDS;
  end;

implementation

uses
  GApp
  ;

{ TOtherData }


{ TTWCDDataList }

constructor TWCDDataList.Create;
begin
  inherited Create( TWCDData );
end;

function TWCDDataList.Find(sTime: string): TWCDData;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count-1 do
    if GetWCDS(i).FTimStr = sTime then
    begin
      Result := GetWCDS(i);
      break;
    end;
end;

function TWCDDataList.GetWCDS(i: integer): TWCDData;
begin
  if ( i < 0 ) or ( i >= count ) then
    Result := nil
  else
    Result := Items[i] as TWCDData;
end;

function TWCDDataList.New(sTime: string): TWCDData;
begin
  Result := Add as TWCDData;
  Result.TimStr := sTime;
end;

end.
