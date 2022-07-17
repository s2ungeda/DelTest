unit UFills;

interface

uses
	System.Classes, System.SysUtils,

	UAccounts, USymbols, UDecimalHelper, 

  UApiTypes

  ;  
  
type

  TFill = class(TCollectionItem)
  private
    FSymbol: TSymbol;
    FPrice: Double;
    FEventTime: TDateTime;
    FFillTime: TDateTime;
    FOrderNo: string;
    FVolume: Double;
    FAccount: TAccount;
    FFillNo: string;
    FSide: integer;
    //
    FPriceBI: TBigint;
    FVolumeBI: TBigint;
    FFee: double;
    FFeeBI: TBigint;

     //
  public
    constructor Create(Coll: TCollection); override;

    function Represent : string;
    procedure Assign( aFill : TFill );

    procedure SetPrice(const Value: string);
    procedure SetVolume(const Value: string);    
    procedure SetFee(const Value: string);    

    property FillNo: string read FFillNo;
    property FillTime	: TDateTime read FFillTime;
    property EventTime: TDateTime read FEventTime;
    property OrderNo: string read FOrderNo;
    property Account: TAccount read FAccount;
    property Symbol: TSymbol read FSymbol;
    property Volume: Double read FVolume;
    property Price: Double read FPrice;
    property Side : integer read FSide;    

    property VolumeBI : TBigint read FVolumeBI;
    property PriceBI	: TBigint read FPriceBI;   
    property FeeBI	: TBigint read FFeeBI;   

    property Fee	: double read FFee;

  end;

  TFills = class(TCollection)
  private
    function Represent: String;
    function GetFill(i: Integer): TFill;
  public
    constructor Create;

    function New(sFillNo: string; dtFillTime, dtEventTime: TDateTime; sOrderNo: string;
      aAccount: TAccount; aSymbol: TSymbol; dVolume: double; iSide : integer; dPrice: Double ): TFill; overload;
    function New(sFillNo: string; dtFillTime, dtEventTime: TDateTime; sOrderNo: string;
      aAccount: TAccount; aSymbol: TSymbol; sVolume: string; iSide : integer; sPrice: string ): TFill; overload;

    function Find( aAccount : TAccount; aSymbol : TSymbol; sFillNo : string ): TFill;

    property Fills[i:Integer]: TFill read GetFill; default;
  end;

  TFillList = class(TList)
  private
    function GetFill(i: Integer): TFill;
  public
    procedure AddFill(aFill: TFill);
    // order �� �ִ� fills �� ã����
    function Find( sFillNo : string ): TFill;    
    property Fills[i:Integer]: TFill read GetFill; default;
  end;  

implementation

{ TFill }

procedure TFill.Assign(aFill: TFill);
begin
  FFillNo := aFill.FillNo;
  FFillTime := aFill.FFillTime;
  FOrderNo := aFill.FOrderNo;
    //
  FAccount := aFill.FAccount;
  FSymbol := aFill.FSymbol;
  FVolume := aFill.FVolume;
  FPrice := aFill.FPrice;
end;

constructor TFill.Create(Coll: TCollection);
begin
  inherited Create(Coll);

  FFillNo := '';
  FFillTime := 0.0;
  FEventTime:= 0.0;
  FOrderNo := '';
    //
  FAccount := nil;
  FSymbol := nil;
  FVolume := 0;
  FPrice := 0;
    //           
end;

function TFill.Represent: String;
var
  stAccount, stSymbol: String;
begin
    // account code
  if FAccount <> nil then
    stAccount := FAccount.Name
  else
    stAccount := 'nil';

    // symbol code
  if FSymbol <> nil then
    stSymbol := FSymbol.Code
  else
    stSymbol := 'nil';

    // represent
  Result := Format('(%s,%s,%s,%s,%s,%f,%.4n)',
                   [FFillNo, FormatDateTime('hh:nn:ss', FFillTime), FOrderNo,
                    stAccount, stSymbol, FVolume, FPrice]);

end;

procedure TFill.SetFee(const Value: string);
begin
	FFeeBI.convert( Value);
  FFee	:= FFeeBI.ToDouble;
end;

procedure TFill.SetPrice(const Value: string);
begin
	FPriceBI.convert( Value);
  FPrice	:= FPriceBI.ToDouble;
end;


procedure TFill.SetVolume(const Value: string);
begin
	FVolumeBI.convert( Value);
  FVolume	:= FVolumeBI.ToDouble;
end;      

{ TFills }

constructor TFills.Create;
begin
  inherited Create(TFill);
end;

function TFills.Find(aAccount: TAccount; aSymbol: TSymbol;
  sFillNo: string): TFill;
var
  i : integer;
begin
  result := nil;
  for i := Count-1 downto 0 do
    if (Fills[i].Account = aAccount) and
      ( Fills[i].Symbol  = aSymbol)  and
      ( Fills[i].FFillNo = sFillNo ) then
      begin
        Result := Fills[i];
        break;
      end; 

end;

function TFills.GetFill(i: Integer): TFill;
begin
  if (i >= 0) and (i <= Count-1) then
    Result := Items[i] as TFill
  else
    Result := nil;
end;

function TFills.New(sFillNo: string; dtFillTime, dtEventTime: TDateTime;
  sOrderNo: string; aAccount: TAccount; aSymbol: TSymbol; dVolume : double; iSide : integer;
  dPrice: Double): TFill;
begin
  Result := Add as TFill;

  Result.FFillNo  	:= sFillNo;
  Result.FFillTime 	:= dtFillTime;
  Result.FOrderNo   := sOrderNo;
  Result.FAccount   := aAccount;
  Result.FSymbol  	:= aSymbol;
  Result.FVolume  	:= dVolume;
  Result.FPrice   	:= dPrice;
  Result.FEventTime := dtEventTime;
  Result.FSide       := iSide;
end;

function TFills.New(sFillNo: string; dtFillTime, dtEventTime: TDateTime;
  sOrderNo: string; aAccount: TAccount; aSymbol: TSymbol; sVolume : string; iSide : integer;
  sPrice: string): TFill;
begin
  Result := Add as TFill;

  Result.FFillNo  	:= sFillNo;
  Result.FFillTime 	:= dtFillTime;
  Result.FOrderNo   := sOrderNo;
  Result.FAccount   := aAccount;
  Result.FSymbol  	:= aSymbol;
  Result.SetVolume( sVolume );
  Result.SetPrice( sPrice );
  Result.FEventTime := dtEventTime;
  Result.FSide       := iSide;
end;

function TFills.Represent: String;
begin

end;

{ TFillList }

procedure TFillList.AddFill(aFill: TFill);
begin
  if IndexOf(aFill) < 0 then Add(aFill);
end;

function TFillList.Find(sFillNo: string): TFill;
var
  i : integer;
begin
  result := nil;
  for i := Count-1 downto 0 do
    if ( Fills[i].FFillNo = sFillNo ) then
    begin
      Result := Fills[i];
      break;
    end; 
end;

function TFillList.GetFill(i: Integer): TFill;
begin
  if (i >= 0) and (i <= Count-1) then
    Result := TFill(Items[i])
  else
    Result := nil;
end;

end.
