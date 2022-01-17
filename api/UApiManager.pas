unit UApiManager;

interface

uses
  System.Classes, System.SysUtils, System.DateUtils,

  UExchangeManager,

  UApiTypes

  ;

type

  TExchangeArray = array [ TExchangeKind ] of TExchangeManaager;

  TApiManager = class
  private
    FExManagers: TExchangeArray;
    function GetExManager: integer;
  public
    Constructor Create;
    Destructor  Destroy; override;

    property ExManagers  : TExchangeArray read FExManagers;
    property ExManagerCount : integer read GetExManager;

  end;

implementation

{ TApiManager }

constructor TApiManager.Create;
var
  I: TExchangeKind;
begin
  for I := ekBinance to High(TExchangeKind) do
  begin
    FExManagers[i]  := TExchangeManaager.Create;
  end;

end;

destructor TApiManager.Destroy;
var
  I: TExchangeKind;
begin
  for I := ekBinance to High(TExchangeKind) do
  begin
    FExManagers[i].Free;
  end;

  inherited;
end;

function TApiManager.GetExManager: integer;
begin
  Result := Integer(high(  TExchangeKind ));
end;


end.
