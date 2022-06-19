unit UTradeCore;

interface

uses
	System.Classes, System.SysUtils, 

  UAccounts, UFills, UOrders, UPositions ,

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
  public
    constructor Create;
    destructor Destroy; override;      

    procedure AccountLoad;

    function FindAccount( ekType : TExchangeKind ) : TAccount;

    property Accounts: TAccountArray read FAccounts;
    property Orders: TOrderArray read FOrders;
    property Fills: TFillArray read FFills;
    property Positions: TPositionArray read FPositions;
  end;

implementation

uses
	GApp
  ;

{ TTradeCore }

procedure TTradeCore.AccountLoad;
var
  I: TExchangeKind;
  aAcnt : TAccount;
begin  

  for I := ekBinance to High( TExchangeKind ) do
  begin
  	aAcnt := FAccounts[i].New( App.Engine.ApiConfig.GetApiKey( I, mtSpot )
    	,App.Engine.ApiConfig.GetSceretKey( I, mtSpot )   , I    );
  end;
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

end;

destructor TTradeCore.Destroy;
var
  I: TExchangeKind;
begin  

  for I := ekBinance to High( TExchangeKind ) do
  begin
    FOrders[i].Free;
    FFills[i].Free;
    FAccounts[i].Free;
    FPositions[i].Free;
  end;

  inherited;
end;

function TTradeCore.FindAccount(ekType: TExchangeKind): TAccount;
begin
	Result := nil;
	if FAccounts[ekType] <> nil then
  	Result := FAccounts[ekType].Find(ektype);
end;

end.
