unit UApiManager;
interface
uses
  System.Classes, System.SysUtils, System.DateUtils,
  UExchangeManager,
  UApiTypes
  ;
type
  TExchangeArray = array [ TExchangeKind ] of TExchangeManager;
  TApiManager = class
  private
    FExManagers: TExchangeArray;
    function GetExManager: integer;
    function RequestMaster : boolean;
    function LoadMaster( sMasterFile : string ) : boolean;
  public
    Constructor Create;
    Destructor  Destroy; override;
    function GetMaster : boolean;
    function PrepareMaster : boolean;
    property ExManagers  : TExchangeArray read FExManagers;
    property ExManagerCount : integer read GetExManager;
  end;
implementation
uses
  UBinanceManager ,  UUpbitManager, UBithManager
  ;
{ TApiManager }
constructor TApiManager.Create;
var
  I: TExchangeKind;
begin
  for I := ekBinance to High(TExchangeKind) do
    case i of
      ekBinance: FExManagers[i] := TBinanceManager.Create(i) as TExchangeManager ;
      ekUpbit: FExManagers[i]   := TUpbitManager.Create(i) as TExchangeManager;
      etBitthumb: FExManagers[i]:= TBithManager.Create(i) as TExchangeManager;
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

function TApiManager.GetMaster: boolean;
var
  sFileName : string;
begin
  // 오늘자 파일 체크..
  sFileName := Format('%s_Master.txt', [ FormatDateTime('yyyymmdd', date) ] );
  if FileExists( sFileName ) then
    LoadMaster( sFileName )
  else begin
    PrepareMaster;
  end;

end;

function TApiManager.LoadMaster(sMasterFile: string): boolean;
begin

end;

function TApiManager.PrepareMaster: boolean;
var
  i :  TExchangeKind;
begin
  for I := ekBinance to High(TExchangeKind) do
  begin
    FExManagers[i].PrepareMaster;
  end;
end;

function TApiManager.RequestMaster: boolean;
var
  i :  TExchangeKind;
begin
  for I := ekBinance to High(TExchangeKind) do
  begin
//    FExManagers[i]  := TExchangeManaager.Create(i);
  end;
end;

end.
