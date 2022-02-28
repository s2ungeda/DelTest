unit UUpbitSpot;

interface

uses
  System.Classes, System.SysUtils, System.DateUtils,

  System.JSON,  Rest.Json , Rest.Types ,

  UExchange,

  UApiTypes

  ;

type
  TUpbitSpot = class( TExchange )
  public
    Constructor Create( aObj : TObject; aMarketType : TMarketType );
    Destructor  Destroy; override;

    function ParsePrepareMaster : integer; override;
    function RequestMaster : boolean ; override;
  end;

implementation

uses
  GApp  , UApiConsts
  , UUpbitParse
  ;

{ TBinanceSpotNMargin }

constructor TUpbitSpot.Create(aObj: TObject; aMarketType: TMarketType);
begin
  inherited Create( aObj, aMarketType );

end;

destructor TUpbitSpot.Destroy;
begin

  inherited;
end;



function TUpbitSpot.ParsePrepareMaster: integer;
var
  master : TJsonArray;
  aObj : TJsonObject;
  i, iLen : Integer;
  stTmp : string;
  sts   : TArray<string>;
begin
  master :=  TJsonObject.ParseJSONValue( MasterData) as TJsonArray;

  for I := 0 to master.Size-1 do
  begin
    aObj := master.Get(i) as TJsonObject;
    stTmp:= aObj.GetValue('market').Value;

    sts := stTmp.Split(['-']);
    iLen:= Length(sts);
    if iLen <= 1 then continue;

    if sts[0] = 'KRW' then begin
      Codes.Add( sts[1] );
    end;
  end;
end;

function TUpbitSpot.RequestMaster: boolean;
var
  aList : TStringList;
  sTmp, sOut, sJson, sData  : string;
  I: Integer;
begin
  aList := TStringList.Create;
  try
    GetCodeList(aList);
    if aList.Count <= 0 then Exit;
    sTmp := '';
    for I := 0 to aList.Count-1 do
    begin
      sTmp := sTmp + 'KRW-'+aList[i];
      if i < aList.Count-1  then
        sTmp := sTmp + ','
    end;



    SetBaseUrl( App.Engine.ApiConfig.GetBaseUrl( GetExKind , mtSpot ) );
    SetParam('markets', sTmp );



    if Request( rmGET, 'v1/ticker', '', sJson, sOut ) then
    begin
      //App.Log( llDebug, '', '%s (%s, %s)', [ TExchangeKindDesc[GetExKind], sOut, sJson] );
//      gBinReceiver.ParseMarginPair( sJson );
      gUpReceiver.ParseSpotTicker( sJson );
    end else
    begin
      App.Log( llError, '', 'Failed %s RequestMaster (%s, %s)',
        [ TExchangeKindDesc[GetExKind], sOut, sJson] );
      Exit( false );
    end;

    sTmp := '';
    for I := 0 to aList.Count-1 do
    begin
      sTmp := sTmp + Format('"KRW-%s"', [aList[i]]);
      if i < aList.Count-1  then
        sTmp := sTmp + ','
    end;

//    App.DebugLog(sTmp);

    Result := App.Engine.SymbolCore.Symbols[ GetExKind].Count > 0 ;

  finally
    aList.Free;
  end;

end;

end.
