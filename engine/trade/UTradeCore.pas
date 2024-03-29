unit UTradeCore;
interface
uses
	System.Classes, System.SysUtils, 
  UAccounts, UFills, UOrders, UPositions , USymbols,
  UApiTypes, UApiConsts        
  ;
type
	TAccountArray = array [TExchangeKind] of TAccounts;
  TOrderArray		= array [TExchangeKind] of TOrders;
  TFillArray		= array [TExchangeKind] of TFills;
  TPositionArray= array [TExchangeKind] of TPositions;

	TTradeCore	= class
  private
    FOrders: TOrderArray;
    FFills: TFillArray;
    FAccounts: TAccountArray;
    FPositions: TPositionArray;
    FTotalOrders: TOrderList;
  public
    constructor Create;
    destructor Destroy; override;      
    procedure AccountLoad;

    function FindAccount( ekType : TExchangeKind ) : TAccount;  overload;
    function FindAccount( ekType : TExchangeKind; aMarket : TAccountMarketType) : TAccount; overload;

    function FindPosition( aAcnt : TAccount;  aSymbol : TSymbol ) : TPosition;
    function NewPosition( aAcnt : TAccount;  aSymbol : TSymbol ) : TPosition;

    function FindOrder( ekType : TExchangeKind; aAcnt : TAccount;
      aSymbol : TSymbol; sID : string ) : TOrder; overload;
    // only bithumb   // 0 : localNo  , 1 : OrderNo;
    function FindOrder( ekType : TExchangeKind;  sNo : string; iDiv : integer = 0 ) : TOrder;  overload;
    //

    property Accounts: TAccountArray read FAccounts;
    property Orders: TOrderArray read FOrders;
    property Fills: TFillArray read FFills;
    property Positions: TPositionArray read FPositions;

    property TotalOrders : TOrderList read FTotalOrders;
  end;
implementation
uses
	GApp
  , UConsts
  ;
{ TTradeCore }
procedure TTradeCore.AccountLoad;
//var
//  I: Integer;
var
  I: TExchangeKind;
  aAcnt : TAccount;
begin
  // account 라 표현했지만.. 거래소랑 비교하면 wallets 라 할수 잇다!!??.
  // spot 과 future 마켓은 다르지만 api 는 같을수도 있고 다를수도 잇음..
  for I := ekBinance to High( TExchangeKind ) do
  begin
    if i = ekBinance then
    begin
    	aAcnt := FAccounts[i].New( App.Engine.ApiConfig.GetApiKey( I, mtSpot )
         	,App.Engine.ApiConfig.GetSceretKey( I, mtSpot )   , I,  amSpot   );
    	aAcnt := FAccounts[i].New( App.Engine.ApiConfig.GetApiKey( I, mtFutures )
        	,App.Engine.ApiConfig.GetSceretKey( I, mtFutures )   , I  , amFuture  );
    end else
    	aAcnt := FAccounts[i].New( App.Engine.ApiConfig.GetApiKey( I, mtSpot )
      	,App.Engine.ApiConfig.GetSceretKey( I, mtSpot )   , I , amSpot   );
  end;
//begin
//
//  for I := 0 to High(App.Engine.ApiConfig.ExAccountInfo) do
//  begin
//    var exType : TExchangeKind;
//    if ExistExType( App.Engine.ApiConfig.ExAccountInfo[i].Code, exType ) then
//    begin
//      var aCnt : TAccount;
//
//      App.Engine.ApiConfig.ExAccountInfo[i].ExType := exType;
//      aCnt := FAccounts[exType].New( App.Engine.ApiConfig.ExAccountInfo[i].Key,
//        App.Engine.ApiConfig.ExAccountInfo[i].Secret, exType );
//
//      if aCnt <> nil then
//        aCnt.Name := App.Engine.ApiConfig.ExAccountInfo[i].Name;
//    end;
//  end;

end;
constructor TTradeCore.Create;
var
  I: TExchangeKind;
begin  
  for I := ekBinance to High( TExchangeKind ) do
  begin
    FOrders[i]	:= TOrders.Create;
    FFills[i]		:= TFills.Create;
    FAccounts[i]:= TAccounts.Create;
    FPositions[i]:= TPositions.Create;
  end;
  FTotalOrders  := TOrderList.Create;
end;
destructor TTradeCore.Destroy;
var
  I: TExchangeKind;
begin  
  for I := ekBinance to High( TExchangeKind ) do
  begin
    FPositions[i].Free;
    FOrders[i].Free;
    FFills[i].Free;
    FAccounts[i].Free;
  end;
  FTotalOrders.Free;
  inherited;
end;
function TTradeCore.FindAccount(ekType: TExchangeKind;
  aMarket: TAccountMarketType): TAccount;
begin
	Result := nil;
	if FAccounts[ekType] <> nil then
  	Result := FAccounts[ekType].Find(ektype, aMarket);
end;

function TTradeCore.FindOrder(ekType: TExchangeKind; sNo: string;
  iDiv: integer): TOrder;
begin
	if iDiv = DIV_LOC_NO then
  	Result := Orders[ekType].FindLocalNo( sNo)
  else
  	Result := Orders[ekType].Find( sNo);
end;

function TTradeCore.FindOrder(ekType: TExchangeKind; aAcnt: TAccount;
  aSymbol: TSymbol; sID: string): TOrder;
begin
  Result := Orders[ekType].Find( aAcnt, aSymbol, sID );
end;

function TTradeCore.FindPosition( aAcnt: TAccount;
  aSymbol: TSymbol): TPosition;
begin
  Result := Positions[aAcnt.ExchangeKind].Find( aAcnt, aSymbol);
end;

function TTradeCore.NewPosition(aAcnt: TAccount; aSymbol: TSymbol): TPosition;
begin
  Result := Positions[aAcnt.ExchangeKind].New( aAcnt, aSymbol);
end;

function TTradeCore.FindAccount(ekType: TExchangeKind): TAccount;
begin
	Result := nil;
	if FAccounts[ekType] <> nil then
  	Result := FAccounts[ekType].Find(ektype);
end;


end.

