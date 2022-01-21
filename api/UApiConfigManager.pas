unit UApiConfigManager;

interface

uses
  system.Classes,  system.SysUtils , system.IniFiles,

  UApiTypes
  ;

type


  TApiConfigManager = class
  private
    FDomesticCnt: integer;
    FOverseasCnt: integer;
    FVerbose: boolean;
    function GetTotal: integer;
  public

    ExchangeInfo : array of TExchangeInfo;

    constructor Create;
    destructor Destroy; override;
    function LoadExchangeConfig : boolean;

    function GetBaseUrl( aExKind : TExchangeKind;  aMarket : TMarketType ) : string;
    function GetPrepare( aExKind : TExchangeKind;  aMarket : TMarketType ) : string;


    property ToTalCnt : integer read GetTotal;
    property DomesticCnt : integer read FDomesticCnt;
    property OverseasCnt : integer read FOverseasCnt;
    property Verbose     : boolean read FVerbose;
  end;

implementation

uses
  GApp, GLibs
  ;

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

function TApiConfigManager.GetBaseUrl(aExKind: TExchangeKind;
  aMarket: TMarketType): string;
begin
  Result := ExchangeInfo[ integer( aExKind )].MarketInfo[ aMarket].BaseUrl;
end;

function TApiConfigManager.GetPrepare(aExKind: TExchangeKind;
  aMarket: TMarketType): string;
begin
  Result := ExchangeInfo[ integer( aExKind )].MarketInfo[ aMarket].Prepare;
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
  j : TMarketType;
begin
  result := true;

  try
    try
      stDir := ExtractFilePath( paramstr(0) )+'Config\';
      pIniFile := TIniFile.Create(stDir + 'exchange.ini' );

      /////////////////////////////////////////////////////////////

      iCnt := pIniFile.ReadInteger('Exchange', 'Count', 0);

      SetLength( ExchangeInfo, iCnt );

      for I := 0 to iCnt-1 do
      begin
        stDir := Format('Exchange_%d', [i]);
        ExchangeInfo[i].SetInfo(  i,
          pIniFile.ReadString(stDir, 'name', 'Sauri')
          , pIniFile.ReadInteger(stDir, 'domestic',0 ) = 1
          , pIniFile.ReadInteger(stDir, 'margin', 0) = 1
          , pIniFile.ReadInteger(stDir, 'fut', 0 ) = 1
        ) ;

        if ExchangeInfo[i-1].IsDomestic then inc( FDomesticCnt )
        else inc( FOverseasCnt );
      end;

      for I := 0 to iCnt-1 do
      begin


        for j := emSpot to High(TMarketType) do
          if ( j = emSpot ) or ((  j = emFuture ) and ( ExchangeInfo[i].IsFuture ))  then
          begin
            stDir := Format( '%s_%s', [ ExchangeInfo[i].Name, ifThenStr( j = emSpot,'spot', 'future')  ] );

            ExchangeInfo[i].MarketInfo[j].BaseUrl  :=  pIniFile.ReadString( stDir, 'Url', 'Sauri');
            ExchangeInfo[i].MarketInfo[j].Prepare  :=  pIniFile.ReadString( stDir, 'PrePare', 'Sauri');
            ExchangeInfo[i].MarketInfo[j].Port     :=  pIniFile.ReadInteger( stDir, 'Port', 443 );
            ExchangeInfo[i].MarketInfo[j].Key      :=  pIniFile.ReadString( stDir, 'ApiKey', 'Sauri' );
            ExchangeInfo[i].MarketInfo[j].Secret   :=  pIniFile.ReadString( stDir, 'SecretKey', 'Sauri' );
          end;
      end;

      //App.Log(llInfo, '', '---start---');
      /////////////////////////////////////////////////////////////
      if App.Config.VERBOSE then
        for I := 0 to iCnt-1 do
          for j := emSpot to emFuture do
          begin
            App.Log( llDebug, '', '%d %s[%s] %s, %s, %s', [i, ExchangeInfo[i].Name
                , TMarketTypeDesc[j]
                , ExchangeInfo[i].MarketInfo[j].BaseUrl
                , ExchangeInfo[i].MarketInfo[j].Key
                , ExchangeInfo[i].MarketInfo[j].Secret ]   );
          end;


    except
      result := false;
    end;
  finally
    pIniFile.Free;
  end;
end;







end.