unit UPositions;
interface
uses
	System.Classes, System.SysUtils, System.DateUtils, 
  UAccounts, USymbols,  UFills,
  UApiTypes, UApiConsts 
  ;
type
  TPosition = class(TCollectionItem)
  private
    FSymbol: TSymbol;
    FLastPL: Double;
    FTradeAmt: Double;
    FEntryOTE: Double;
    FFee: double;
    FEntryPL: Double;
    FVolume: double;
    FEntryDate: TDateTime;
    FAvgPrice: Double;
    FAccount: TAccount;
    FFills: TFillList;
    FLastFill: TFill;
    function GetFee: double;
    function GetLastPL: Double;
    function GetSide: integer;
    procedure DoCalcFut;
    procedure DoCalcPosition;
  public
    constructor Create(aColl: TCollection); override;
    destructor Destroy; override;

    procedure AddFill(aFill: TFill);
    procedure CalcOpenPL ;

    procedure DoOrder( iSide : integer; dQty : double );
    property Account: TAccount read FAccount write FAccount;
    property Symbol: TSymbol read FSymbol write FSymbol;    
		property Volume: double read FVolume write FVolume;
    property Side  : integer read GetSide;
    property AvgPrice: Double read FAvgPrice write FAvgPrice;
    property EntryDate: TDateTime read FEntryDate;    
	  property EntryPL: Double read FEntryPL;			 // 실현손익
    property EntryOTE: Double read FEntryOTE;    // 평가금액
    property LastPL : Double read GetLastPL write FLastPL;
    property TradeAmt : Double read FTradeAmt write FTradeAmt;
    property Fee      : double read GetFee  write FFee;

    property Fills: TFillList read FFills;
    property LastFill: TFill  read FLastFill;

  end;
  TPositions = class(TCollection)
  private
    function GetPosition(i: Integer): TPosition;       
  public
    constructor Create;
    destructor Destroy; override;
    function GetLastPosition: string;
    function Find( aSymbol : TSymbol ) : TPosition; overload;
    function Find( aAccount : TAccount ) : TPosition; overload;
    function Find(aAccount: TAccount; aSymbol: TSymbol): TPosition; overload;
    function FindOrNew(aAccount: TAccount; aSymbol: TSymbol): TPosition;
    function New(aAccount: TAccount; aSymbol: TSymbol;
      dVolume: double = 0.0; dAvgPrice: Double = 0.0;
      dtEntry: Double = 0.0): TPosition;


    procedure UpdatePosition( aSymbol : TSymbol );
    function GetSymbolPL( aAccount: TAccount; aSymbol : TSymbol ) : Double;
    function GetPL( aAccount : TAccount ) : double;
    function GetOpenPL( aAccount : TAccount ) : double;

    // 주문 제한 ( 신규
    function CheckOrderLimit( aAcnt : TAccount; aSymbol : TSymbol; iSide : integer;
        dPrice : double;  bNewOrder : boolean = true ) : boolean;
    procedure CheckOrderLimitDec( aAcnt : TAccount; aSymbol : TSymbol; iSide : integer );
    property Positions[i: Integer]: TPosition read GetPosition; default;
  end;  
  
implementation
uses
	GLibs
	;  
{ TPosition }
procedure TPosition.AddFill(aFill: TFill);
begin
  FFills.AddFill( aFill );
  FLastFill := aFill;

//  case FSymbol.Spec.Market of
//    mtSpot:     DoCalcSpot;
//    mtFutures:  DoCalcFut;
//  end;
  
	DoCalcPosition;
end;

procedure TPosition.DoCalcPosition;
var
  dFilledQty , dNewQty : double;
begin

  dFilledQty  := FLastFill.Volume * FLastFill.Side;
  dNewQty     := FVolume + dFilledQty;

  if CheckZero( FVolume ) then
    FAvgPrice := FLastFill.Price
  else if ( FVolume * dNewQty ) < 0 then // 반대포지션
  begin
    FEntryPL  := FEntryPL + (FLastFill.Price - FAvgPrice) * FVolume * FSymbol.Spec.PointValue;
    FAvgPrice := FLastFill.Price;
  end else
  if CheckZero( dNewQty ) then  // 청산
  begin
    FEntryPL  := FEntryPL + (FLastFill.Price - FAvgPrice) * FVolume * FSymbol.Spec.PointValue;
    FAvgPrice := 0;
  end else
  if abs(dNewQty) > abs(FVolume) then
  begin
    FAvgPrice := ( FAvgPrice * FVolume + FLastFill.Price * dFilledQty ) / dNewQty;
  end else
  begin
    FEntryPL := FEntryPL
                  + (FLastFill.Price - FAvgPrice) * -dFilledQty * FSymbol.Spec.PointValue;
  end;

  FTradeAmt   := FTradeAmt + FLastFill.Volume * FLastFill.Price * FSymbol.Spec.PointValue;
  FVolume     := dNewQty;

  CalcOpenPL;

end;

procedure TPosition.DoCalcFut;
var
  dFilledQty , dNewQty : double;
begin

  dFilledQty  := FLastFill.Volume * FLastFill.Side;
  dNewQty     := FVolume + dFilledQty;

  if CheckZero( FVolume ) then
    FAvgPrice := FLastFill.Price
  else if ( FVolume * dNewQty ) < 0 then // 반대포지션
  begin
    FEntryPL  := FEntryPL + (FLastFill.Price - FAvgPrice) * FVolume * FSymbol.Spec.PointValue;
    FAvgPrice := FLastFill.Price;
  end else
  if CheckZero( dNewQty ) then  // 청산
  begin
    FEntryPL  := FEntryPL + (FLastFill.Price - FAvgPrice) * FVolume * FSymbol.Spec.PointValue;
    FAvgPrice := 0;
  end else
  if abs(dNewQty) > abs(FVolume) then
  begin
    FAvgPrice := ( FAvgPrice * FVolume + FLastFill.Price * dFilledQty ) / dNewQty;
  end else
  begin
    FEntryPL := FEntryPL
                  + (FLastFill.Price - FAvgPrice) * -dFilledQty * FSymbol.Spec.PointValue;
  end;

  FTradeAmt   := FTradeAmt + FLastFill.Volume * FLastFill.Price * FSymbol.Spec.PointValue;
  FVolume     := dNewQty;

  CalcOpenPL;
end;

procedure TPosition.CalcOpenPL;
begin
  FEntryOTE := FVolume * (FSymbol.Last - FAvgPrice) * FSymbol.Spec.PointValue;
	case FSymbol.Spec.Market of
    mtSpot		: FEntryOTE := FVolume  * FSymbol.Last;
    mtFutures	: FEntryOTE := FVolume * (FSymbol.Last - FAvgPrice) * FSymbol.Spec.PointValue;
  end;
end;

constructor TPosition.Create(aColl: TCollection);
begin
  inherited Create( aColl );
  FAccount := nil;
  FSymbol := nil;
  FVolume := 0;
  FAvgPrice := 0.0;
  FEntryDate := 0.0;
  FEntryPL := 0.0;
  FEntryOTE := 0.0;

  FFills:= TFillList.Create;
  FLastFill := nil;
end;

destructor TPosition.Destroy;
begin
  FFills.Free;
  inherited;
end;

procedure TPosition.DoOrder(iSide: integer; dQty: double);
begin

end;

function TPosition.GetFee: double;
begin
  Result := FFee;
end;

function TPosition.GetLastPL: Double;
begin
  Result := FEntryPL + FEntryOTE;
end;

function TPosition.GetSide: integer;
begin
  if Volume > 0 then
    Result := 1
  else if Volume < 0 then
    Result := -1
  else
    Result := 0;
end;




{ TPositions }
function TPositions.CheckOrderLimit(aAcnt: TAccount; aSymbol: TSymbol;
  iSide: integer; dPrice: double; bNewOrder: boolean): boolean;
begin
end;
procedure TPositions.CheckOrderLimitDec(aAcnt: TAccount; aSymbol: TSymbol;
  iSide: integer);
begin
end;
constructor TPositions.Create;
begin
  inherited Create(TPosition);
end;
destructor TPositions.Destroy;
begin
  inherited;
end;
function TPositions.Find(aAccount: TAccount): TPosition;
var
  i: Integer;
  aPosition: TPosition;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    aPosition := GetPosition(i);
    if (aPosition.Account = aAccount) then
    begin
      Result := aPosition;
      Break;
    end;
  end;

end;
function TPositions.Find(aAccount: TAccount; aSymbol: TSymbol): TPosition;
var
  i: Integer;
  aPosition: TPosition;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    aPosition := GetPosition(i);
    if (aPosition.Account = aAccount) and ( aPosition.Symbol = aSymbol ) then
    begin
      Result := aPosition;
      Break;
    end;
  end;

end;
function TPositions.Find(aSymbol: TSymbol): TPosition;
var
  i: Integer;
  aPosition: TPosition;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    aPosition := GetPosition(i);
    if (aPosition.Symbol = aSymbol) then
    begin
      Result := aPosition;
      Break;
    end;
  end;
end;
function TPositions.FindOrNew(aAccount: TAccount; aSymbol: TSymbol): TPosition;
begin
  Result := Find(aAccount, aSymbol);
  if Result = nil then
    Result := New(aAccount, aSymbol);
end;

function TPositions.GetLastPosition: string;
begin
end;

function TPositions.GetOpenPL(aAccount: TAccount): double;
var
  i: Integer;
  aPosition: TPosition;
begin
  Result := 0;
  for i := 0 to Count - 1 do
  begin
    aPosition := GetPosition(i);
    if aPosition = nil then
      Continue;
          
    if (aPosition.Account = aAccount) then
      Result := Result + aPosition.EntryOTE;
  end;

end;
function TPositions.GetPL(aAccount: TAccount): double;
var
  i: Integer;
  aPosition: TPosition;
begin
  Result := 0;
  for i := 0 to Count - 1 do
  begin
    aPosition := GetPosition(i);
    if aPosition = nil then
      Continue;
    if (aPosition.Account = aAccount) then
      Result := Result + aPosition.LastPL;
  end;
end;
function TPositions.GetPosition(i: Integer): TPosition;
begin
  if (i >= 0) and (i <= Count-1) then
    Result := Items[i] as TPosition
  else
    Result := nil;
end;
function TPositions.GetSymbolPL(aAccount: TAccount; aSymbol: TSymbol): Double;
var
  i: Integer;
  aPosition: TPosition;
  dS, dF, dOpt, dTot : double;
begin
  dTot := 0;
  dS :=0;
  dF :=0;
  dOpt:=0;
//  for i := 0 to Count - 1 do
//  begin
//    aPosition := GetPosition(i);
//    if (aPosition.Account = aAccount) and ( aPosition.Symbol = aSymbol ) then
//    begin
//      case aPosition.Symbol.Spec.Market of
//        mtStock, mtBond, mtETF, mtELW : dS  := dS + aPosition.EntryOTE + aPosition.EntryPL;
//        mtFutures : dF := dF + aPosition.EntryOTE + aPosition.EntryPL;
//        mtOption : dOpt := dOpt + aPosition.EntryOTE + aPosition.EntryPL ;
//      end;
//      break;
//    end;
//  end;
  dTot := dS + dOpt + dF ;// dFee;
  Result := dTot;
end;
function TPositions.New(aAccount: TAccount; aSymbol: TSymbol; dVolume,
  dAvgPrice: Double; dtEntry: Double): TPosition;
begin
  Result := nil;
  if (aAccount = nil) or (aSymbol = nil) then Exit;
  Result := Find(aAccount, aSymbol);
  if Result = nil then
  begin
    Result := Add as TPosition;
    Result.FAccount := aAccount;
    Result.FSymbol := aSymbol;   
    if not CheckZero(dVolume) then
    begin
      Result.FEntryDate := dtEntry;
      Result.FVolume 		:= dVolume;
      Result.FAvgPrice 	:= dAvgPrice;
    end;
  end;
end;

procedure TPositions.UpdatePosition( aSymbol : TSymbol );
var
  i: Integer;
  aPosition: TPosition;
begin

  for i := 0 to Count - 1 do
  begin
    aPosition := GetPosition(i);
    if (aPosition.Symbol = aSymbol ) then
      aPosition.CalcOpenPL;
  end;

end;
end.
