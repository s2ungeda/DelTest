unit UDecimalHelper;

interface

uses
  System.SysUtils
  ;

type

  TDecimalHelper = record
    OrgVal : string;
    Jungsu : string;
    Sosu   : string;
    Precision : integer;
    Multiple  : int64;
    ConVal    : int64;
    constructor Create(sval : string );
    function print : string;
    procedure init;
    procedure convert(sval:string);
    function ToDouble : double;
    function ToInt64  : int64;
  end;

  TBigInt = TDecimalHelper;

implementation

{ TDecimalHelper }

procedure TDecimalHelper.convert(sval:string);
var
  sts : TArray<string>;
  iCnt, iLen, j: integer;
begin
  if sval = '' then Exit;

  OrgVal := sval;
  init;
  
  sts  := OrgVal.Split(['.']);
  iLen := High(sts);


  if iLen >= 1 then  begin
    Jungsu  := sts[0];
    Sosu    := sts[1];
    iCnt    := 0;

    for j := Length(Sosu) downto 1 do
    begin
      if Sosu[j] <> '0' then     break;
      if Sosu[j] = '0' then      inc( iCnt);
    end;

    Precision  := length(sts[1]) - iCnt;
    if iCnt > 0 then
      Sosu := Copy( sts[1], 1, Precision );

    for j := 0 to Precision-1 do  Multiple := Multiple*10;

  end else
    Jungsu := OrgVal;

  ConVal  :=  StrToInt64(Jungsu + Sosu);
end;

constructor TDecimalHelper.Create(sval: string);
begin
  //
  OrgVal  := sval;
  init;
end;

procedure TDecimalHelper.init;
begin
  Sosu    := '';
  Multiple  := 1;
  Precision := 0;
end;

function TDecimalHelper.print : string;
begin
  Result := Format('O:%s Cnv:%d %d , %.*f', [ OrgVal, ConVal, Precision, Precision, ConVal / Multiple ]);
end;

function TDecimalHelper.ToDouble: double;
begin
	if Multiple = 0 then
  	Result := ConVal
  else  
	  Result  := ConVal / Multiple;
end;

function TDecimalHelper.ToInt64: int64;
begin
  Result  := ConVal;
end;

end.
