unit UApiConfigManager;

interface

uses
  system.Classes,  system.SysUtils , system.IniFiles
  ;

type

  TExchangeInfo = record
    Name : string;
    Code : string;
    IsMargin : boolean;
    IsFuture : boolean;
    IsDomestic : boolean;
    procedure SetInfo( stCode : string ; isMar,isFut, isDome : boolean );
  end;


  TApiConfigManager = class
  private
    FDomesticCnt: integer;
    FOverseasCnt: integer;
    function GetTotal: integer;
  public

    ExchangeInfo : array of TExchangeInfo;

    constructor Create;
    destructor Destroy; override;
    function LoadExchangeConfig : boolean;

    property ToTalCnt : integer read GetTotal;
    property DomesticCnt : integer read FDomesticCnt;
    property OverseasCnt : integer read FOverseasCnt;
  end;

implementation

{ TApiConfigManasger }

constructor TApiConfigManager.Create;
begin
  FDomesticCnt:= 0;
  FOverseasCnt:= 0;
end;

destructor TApiConfigManager.Destroy;
begin
  ExchangeInfo := nil;
  inherited;
end;

function TApiConfigManager.GetTotal: integer;
begin
  result := FDomesticCnt + FOverseasCnt;
end;

function TApiConfigManager.LoadExchangeConfig: boolean;
var
  pIniFile : TIniFile;
  stDir : string;
  iCnt : integer;
  I: Integer;
begin
  result := true;

  try
    try
      stDir := ExtractFilePath( paramstr(0) )+'Config\';
      pIniFile := TIniFile.Create(stDir + 'exchange.ini' );

      /////////////////////////////////////////////////////////////

      iCnt := pIniFile.ReadInteger('Exchange', 'Count', 0);

      SetLength( ExchangeInfo, iCnt );

      for I := 1 to iCnt do
      begin
        stDir := Format('Exchange_%d', [i]);
        ExchangeInfo[i-1].SetInfo(
          pIniFile.ReadString(stDir, 'name', 'Sauri')
          , pIniFile.ReadInteger(stDir, 'domestic',0 ) = 1
          , pIniFile.ReadInteger(stDir, 'margin', 0) = 1
          , pIniFile.ReadInteger(stDir, 'fut', 0 ) = 1
        ) ;

        if ExchangeInfo[i-1].IsDomestic then inc( FDomesticCnt )
        else inc( FOverseasCnt );

      end;

      /////////////////////////////////////////////////////////////

    except
      result := false;
    end;
  finally
    pIniFile.Free;
  end;
end;

{ TExchangeInfo }



{ TExchangeInfo }

procedure TExchangeInfo.SetInfo(stCode: string; isMar, isFut, isDome: boolean);
begin
  Code := stCode;
  IsMargin := isMar;
  IsFuture := isFut;
  IsDomestic := isDome;
end;

end.
