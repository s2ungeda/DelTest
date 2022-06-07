unit UAccounts;
interface
uses
  system.Classes, system.SysUtils
  , UTypes, UApiTypes
  ;
type
  TAcntAmtArray = array [TSettleCurType] of double;
	TAccount = class( TCollectionItem )
  private
    FAccountType: TAccountMarketType;
    FExchangeKind: TExchangeKind;
    FApiKey: string;
    FPriKey: string;
    function GetName: string;
  public
    AvailableAmt :  array [TSettleCurType] of double;            // 주문가능금액
    TradeAmt     :  array [TSettleCurType] of double;            // 약정금액 ( 보유자산 )
    constructor Create( aColl : TCollection ); override;
    destructor Destroy; override;
    property AccountType  : TAccountMarketType read FAccountType write FAccountType;
    property ExchangeKind : TExchangeKind read FExchangeKind write FExchangeKind;

    property ApiKey : string read FApiKey;
    property PriKey : string read FPriKey;
    property Name	  : string read GetName;
  end;
  TAccounts = class(TCollection)
  private
    FPresentName: string;
    FPassword: string;
    function GetAccount(i: Integer): TAccount;
  public
    constructor Create;
    Destructor Destroy ; override;
    function New( sKey, sPri: String; aKind: TExchangeKind ): TAccount; overload;  
    function New( sKey, sPri: String; aKind: TExchangeKind; aMarket : TAccountMarketType ): TAccount; overload;  
    function Find( sKey: string) : TAccount; overload;
    function Find( aKind: TExchangeKind) : TAccount; overload;
    function Find( aKind: TExchangeKind; aMarket : TAccountMarketType) : TAccount; overload;
    function Represent: String;
    
    property Accounts[i: Integer]: TAccount read GetAccount; default;
  end;  
  TAccountList = class(TStringList)
  private
    function GetAccount(i: Integer): TAccount;
  public
    function Represent: String;
    procedure AddAccount(aAccount: TAccount);
    procedure GetList(aList: TStrings);
    property Accounts[i: Integer]: TAccount read GetAccount; default;
    function Find( sKey: String): TAccount; overload;
    function Find( aKind: TExchangeKind) : TAccount; overload;
  end;  
	
implementation
uses
	UApiConsts
  ;
{ TAccount }
constructor TAccount.Create(aColl: TCollection);
var
  i : TSettleCurType;
begin
  inherited create( aColl );
  for I := scKRW to High(TSettleCurType) do
  begin
    TradeAmt[i]			:= 0.0;
    AvailableAmt[i]	:= 0.0;
  end;
end;
destructor TAccount.Destroy;
begin
  inherited;
end;
function TAccount.GetName: string;
begin       
	Result := TExchangeKindShortDesc[ FExchangeKind ]+'.'+ TAccountMarketTypeDesc[ FAccountType];
end;
{procedure TAccount.SetAvailableAmt(const Value: TAcntAmtArray);
begin
  FAvailableAmt := Value;
end;

 TAccounts }
constructor TAccounts.Create;
begin
  inherited Create(TAccount);
end;
destructor TAccounts.Destroy;
begin
  inherited;
end;
function TAccounts.Find(aKind: TExchangeKind; aMarket: TAccountMarketType): TAccount;
var
  I: Integer;
  aAcnt : TAccount;
begin
	Result := nil;
  for I := 0 to Count-1 do
	begin
		aAcnt := GetAccount(i);
    if (aAcnt.AccountType = aMarket ) and ( aAcnt.ExchangeKind = aKind ) then
    begin  
      Result := aAcnt;
      Break;
    end;
  end;
end;
function TAccounts.Find(sKey: string): TAccount;
var
  I: Integer;
  aAcnt : TAccount;
begin
	Result := nil;
  for I := 0 to Count-1 do
	begin
		aAcnt := GetAccount(i);
    if ( aAcnt.ApiKey = sKey ) then
    begin  
      Result := aAcnt;
      Break;
    end;
  end;

end;
function TAccounts.Find(aKind: TExchangeKind): TAccount;
var
  I: Integer;
  aAcnt : TAccount;
begin
	Result := nil;
  for I := 0 to Count-1 do
	begin
		aAcnt := GetAccount(i);
    if ( aAcnt.ExchangeKind = aKind ) then
    begin  
      Result := aAcnt;
      Break;
    end;
  end;
end;
function TAccounts.GetAccount(i: Integer): TAccount;
begin
  if (i >= 0) and (i <= Count-1) then
    Result := TAccount(Items[i])
  else
    Result := nil;
end;
function TAccounts.New(sKey, sPri: String; aKind: TExchangeKind;
  aMarket: TAccountMarketType): TAccount;
begin
  if (sKey = '') or ( sPri = '' ) then begin
    Result := nil;
    Exit;
  end;
  Result := Find(sKey);
  if Result = nil then
  begin
    Result := Add as TAccount;
    Result.FApiKey := sKey;
    Result.FPriKey := sPri;
		Result.AccountType	:= aMarket;
    Result.ExchangeKind	:= aKind;
  end;
end;
function TAccounts.New(sKey, sPri: String; aKind: TExchangeKind): TAccount;
begin
  if (sKey = '') or ( sPri = '' ) then begin
    Result := nil;
    Exit;
  end;
  Result := Find(sKey);
  if Result = nil then
  begin
    Result := Add as TAccount;
    Result.FApiKey := sKey;
    Result.FPriKey := sPri;
    Result.ExchangeKind	:= aKind;
    Result.AccountType	:= amAll;
  end;
end;
function TAccounts.Represent: String;
begin
end;
{ TAccountList }
procedure TAccountList.AddAccount(aAccount: TAccount);
begin
  if (aAccount <> nil) and (IndexOfObject(aAccount) < 0) then
    AddObject(aAccount.Name, aAccount);
end;
function TAccountList.Find(aKind: TExchangeKind): TAccount;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if GetAccount(i).ExchangeKind = aKind then
    begin
      Result := Objects[i] as TAccount;
      Break;
    end;   
end;
function TAccountList.Find(sKey: String): TAccount;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if GetAccount(i).FApiKey = sKey then
    begin
      Result := Objects[i] as TAccount;
      Break;
    end;   
end;
function TAccountList.GetAccount(i: Integer): TAccount;
begin
  if (i >= 0) and (i <= Count-1) then
    Result := TAccount(Objects[i])
  else
    Result := nil;
end;
procedure TAccountList.GetList(aList: TStrings);
var
  i: Integer;
  aAccount: TAccount;
begin
  if aList = nil then Exit;
  for i := 0 to Count - 1 do
  begin
    aAccount := GetAccount(i);
    aList.AddObject(aAccount.Name, aAccount);
  end;
end;
function TAccountList.Represent: String;
begin
end;
end.
