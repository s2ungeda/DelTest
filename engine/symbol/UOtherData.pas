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
    FValue : array [0..1] of double;
    FName: string;
    function GetPrice(ekKind: TExchangeKind): double;
    procedure SetPrice(ekKind: TExchangeKind; const Value: double);
    function GetWCDPrice(ekKind: TExchangeKind): double;
    function GetAmount(ekKind: TExchangeKind): double;
    procedure SetAmount(ekKind: TExchangeKind; const Value: double);
  public
    property Name : string read FName write FName;
    property TimStr : string read FTimStr write FTimStr;
    property Price[ ekKind : TExchangeKind ] : double read GetPrice write SetPrice;
    property Amount[ ekKind : TExchangeKind ] : double read GetAmount write SetAmount;
    property WCDPrice[ ekKind : TExchangeKind ] : double read GetWCDPrice;
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

function TWCDData.GetAmount(ekKind: TExchangeKind): double;
begin

end;

function TWCDData.GetPrice(ekKind: TExchangeKind): double;
begin
  Result := FValue[ integer( ekKind ) - 1 ] ;
end;

procedure TWCDData.SetAmount(ekKind: TExchangeKind; const Value: double);
begin

end;

procedure TWCDData.SetPrice(ekKind: TExchangeKind; const Value: double);
begin
  FValue[ integer( ekKind ) - 1 ] := Value;
end;

function TWCDData.GetWCDPrice(ekKind: TExchangeKind): double;
begin

end;

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
