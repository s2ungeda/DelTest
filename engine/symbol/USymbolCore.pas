unit USymbolCore;

interface

uses
  system.Classes, system.SysUtils , system.DateUtils,

  UFQN, UMarketSpecs, USymbols, UMarkets,

  UApiTypes, UOtherData,

  UApiConsts
  ;

type

  TSymbolArray  = array [TExchangeKind] of TSymbolList;
  TSpotArray    = array [TExchangeKind] of TSpots;
  TFutureArray  = array [TExchangeKind] of TFutures;

  TMarketArray        = array [TExchangeKind] of TMarketList;
  TSpotMarketArray    = array [TExchangeKind] of TSpotMarkets;
  TFutureMarketArray  = array [TExchangeKind] of TFutureMarkets;

  TMarketGroupsArray  = array [ TExchangeKind ] of TMarketGroups;

  TMajorSymbols    = array [TExchangeKind] of TSymbol;
  TMainSymbols     = array [TMajorSymbolKind] of  TMajorSymbols;

  TMainKimpArray  = array [TExchangeKind] of double;
  TMainWDCArray  = array [TExchangeKind] of double;

  TSymbolCore = class
  private

    FSymbols: TSymbolArray;
    FSpots: TSpotArray;
    FFutures: TFutureArray;

    FMarkets: TMarketArray;
    FSpotMarkets: TSpotMarketArray;
    FFutureMarkets: TFutureMarketArray;

    FSpecs: TMarketSpecs;

    FExchanges  : TMarketGroupsArray;
    FUnderlyings: TMarketGroupsArray;
    FMainSymbols: TMainSymbols;
    FMainKimp: TMainKimpArray;
    FSymbolDnwStates: TSymbolArray;
    FCommSymbols: TCommSymbolList;
    FBaseSymbols: TBaseSymbols;
    FMainExKind: TExchangeKind;
    FSubExKind2: TExchangeKind;
    FSubExKind1: TExchangeKind;
    FMainExMarket: TMarketType;
    FMainWDC: TMainWDCArray;

    FWCDays: TWCDDataList;
    FWCD30s: TWCDDataList;


  public

    JungKopi  : array [TExchangeKind, 0..47] of double;
    JKIdx     : array [TExchangeKind] of integer;
    JungKopiLog  : array [TExchangeKind, 0..47] of string;
    //LastJKIdx : integer;

    RprsntWDC : array [TExchangeKind, 0..47] of double;
    WDCIdx    : array [TExchangeKind] of integer;
    RprsntWDCLog : array [TExchangeKind, 0..47] of string;
    //LastWDCIdx : integer;

    constructor Create;
    destructor Destroy; override;

    procedure RegisterSymbol(aExKind : TExchangeKind; aSymbol: TSymbol); overload;
    function  RegisterSymbol(aExKind : TExchangeKind; aMarket : TMarketType; aCode : string ) : TSymbol ; overload;
    function  FindSymbol(aExKind : TExchangeKind; aCode : string ): TSymbol;
//    function  FindQuoteSymbol(aExKind : TExchangeKind; sBaseCode : string ): TSymbol;
    function  IsOSMain( aSymbol : TSymbol ) : boolean;
    function  IsKRMain( aSymbol : TSymbol ) : boolean;

    procedure Log;
    procedure PreSubscribe;   // 주요종목 구독
    procedure PreUnSubscribe;   // 주요종목 구독
    procedure SubscribeSymbols; // 모든종목 한방에
    procedure RepresentCoin;
    procedure MakePrevData;

    procedure GetSymbolList( aExKind : TExchangeKind; var aList : TList );

    // 모든 종목을 구독할수 없어서.. QuoteBroker 에서 여기로 이전..
    procedure CalcKimp( aPrice : double; aSymbol : TSymbol ); overload;
    procedure CalcKimp( aSymbol : TSymbol ); overload;
    procedure CalcMainKimp( aSymbol : TSymbol );  overload;
    procedure CalcMainKimp( aExKind: TExchangeKind ) ;   overload;
    procedure CalcMainWDC( aSymbol : TSymbol );
    procedure CalcSP(aPrice : double; aSymbol: TSymbol );

    procedure SetMainKimp( aExKind : TExchangeKind; Value : double; sLog : string );
    procedure SetMainWDC(aExKind: TExchangeKind; Value: double; sLog : string);
    procedure SymbolArrange;

    property Specs: TMarketSpecs read FSpecs;

    property Symbols: TSymbolArray read FSymbols;
    property Spots: TSpotArray read FSpots;
    property Futures: TFutureArray read FFutures;
    property CommSymbols : TCommSymbolList read FCommSymbols;

    property SymbolDnwStates :  TSymbolArray read FSymbolDnwStates;

    property Markets: TMarketArray read FMarkets;
    property SpotMarkets: TSpotMarketArray read FSpotMarkets;
    property FutureMarkets: TFutureMarketArray read FFutureMarkets;

    property Underlyings : TMarketGroupsArray read FUnderlyings;
    property Exchanges   : TMarketGroupsArray read FExchanges;
    // symbolKind . ExchangeKind
    property MainSymbols : TMainSymbols read FMainSymbols;
    property BaseSymbols : TBaseSymbols read FBaseSymbols;

    property MainKimp : TMainKimpArray read FMainKimp ;//write FMainKimp;
    property MainWDC  : TMainWDCArray  read FMainWDC;

    property MainExMarket : TMarketType read FMainExMarket write FMainExMarket;
    property MainExKind : TExchangeKind read FMainExKind write FMainExKind;
    property SubExKind1 : TExchangeKind read FSubExKind1 write FSubExKind1;
    property SubExKind2 : TExchangeKind read FSubExKind2 write FSubExKind2;

    property WCDays : TWCDDataList read FWCDays;
    property WCD30s : TWCDDataList read FWCD30s;

  end;

function TicksFromPrice(aSymbol: TSymbol; dPrice: Double; iTicks: Integer): Double;
function LocksFromPrice(aSymbol: TSymbol; dPrice: Double; iTicks: Integer): Double;
function GetPrecision(aSymbol: TSymbol; dPrice: Double): integer;

implementation

uses
  GApp, UConsts, GLibs
  , Math
  ;


{ TSymbolCore }

// aPrice 는 해외종목 가격..
procedure TSymbolCore.CalcKimp( aPrice : double; aSymbol : TSymbol );
var
  dEx : double;
begin

  dEx :=  Max( aPrice * App.Engine.ApiManager.ExRate.Value , 1 );

  if dEx > EPSILON then begin
    aSymbol.KimpPrice     := ( aSymbol.Last - dEx) / dEx * 100;
    aSymbol.DataTrace.Kip.SetData( App.Engine.ApiManager.ExRate.Value,
      aPrice, aSymbol.Last, aSymbol.KimpPrice );
  end;
  if aPrice > EPSILON then begin
    aSymbol.WDCPrice      := ( 1/ aPrice) * aSymbol.Last;
    aSymbol.DataTrace.WCD.SetData( aPrice, aSymbol.Last, aSymbol.WDCPrice );
  end;

//  if aSymbol.KimpPrice > 99999 then
//    App.DebugLog('%s, %s, %.3f =  %s, %s, %.1f', [ TExchangeKindDesc[ aSymbol.Spec.ExchangeType ],
//      aSymbol.Code, aSymbol.KimpPrice, aSymbol.PriceToStr(aSymbol.Last)
//      , aSymbol.PriceToStr(aPrice),  App.Engine.ApiManager.ExRate.Value ]
//      );

//  aSymbol.KimpAskPrice  := ( aSymbol.Asks[0].Price - dEx) / dEx * 100;
//  aSymbol.KimpBidPrice  := ( aSymbol.Bids[0].Price - dEx) / dEx * 100;

end;

procedure TSymbolCore.CalcSP(aPrice : double; aSymbol: TSymbol);
begin
  if aSymbol.Spec.ExchangeType = FSubExKind2 then
  begin
    aSymbol.SPrice := 0;
    Exit;
  end;

  if not IsZero( aPrice ) then begin
    aSymbol.SPrice := (aSymbol.Last - aPrice) / aPrice * 100  ;
    aSymbol.DataTrace.SP.SetData( aPrice, aSymbol.Last, aSymbol.SPrice);
  end
  else
    aSymbol.SPrice := 0;
end;

procedure TSymbolCore.CalcKimp( aSymbol : TSymbol );
var
  dEx : double;
  pSymbol : TSymbol;
  aList   : TSymbolList;
  I: Integer;
  bOS : boolean;
begin

  bOS := false;
  if (aSymbol.Spec.ExchangeType = FMainExKind ) then
    bOS := true;

  if bOS then
  begin

    if aSymbol.Spec.Market <> FMainExMarket then Exit;

    aList := FBaseSymbols.FindSymbolList( aSymbol.Spec.BaseCode );
    if aList = nil then Exit;

    for I := 0 to aList.Count-1 do
    begin
      pSymbol := aList.Symbols[i];
      if (pSymbol.Spec.ExchangeType = FSubExKind1) or
         (pSymbol.Spec.ExchangeType = FSubExKind2) then
         CalcKimp( aSymbol.Last, pSymbol );
    end;

  end else
  begin
    pSymbol := FBaseSymbols.FindSymbol( aSymbol.Spec.BaseCode, FMainExKind, FMainExMarket  );
    if pSymbol <> nil then begin
      CalcKimp( pSymbol.Last, aSymbol );
    end;

    if aSymbol.Spec.ExchangeType = FSubExKind1 then begin
      pSymbol := FBaseSymbols.FindSymbol( aSymbol.Spec.BaseCode, FSubExKind2  );
      if pSymbol <> nil then
        CalcSP( pSymbol.Last, aSymbol );
    end  else
    if aSymbol.Spec.ExchangeType = FSubExKind2 then begin
      pSymbol := FBaseSymbols.FindSymbol( aSymbol.Spec.BaseCode, FSubExKind1  );
      if pSymbol <> nil then
        CalcSP( aSymbol.Last, pSymbol );
    end;


//    aList := FBaseSymbols.FindSymbolList( aSymbol.Spec.BaseCode );
//    if aList = nil then Exit;
//
//    for I := 0 to aList.Count-1 do
//    begin
//      pSymbol := aList.Symbols[i];
//      if pSymbol = nil then Continue;
//
//      if (pSymbol.Spec.ExchangeType = FSubExKind1) or
//         (pSymbol.Spec.ExchangeType = FSubExKind2) then
//         CalcKimp( aSymbol.Last, pSymbol );
//
//      if (aSymbol.Spec.ExchangeType = FSubExKind1) and
//         (pSymbol.Spec.ExchangeType = FSubExKind2) then
//         CalcSP( pSymbol.Last, aSymbol );
//
//    end;


  end;

end;

procedure TSymbolCore.CalcMainKimp( aSymbol : TSymbol );
var
  dPrice : double;
  aKind : TMajorSymbolKind;
  aExKind : TExchangeKind;
begin

  if (aSymbol.Spec.BaseCode = TMajorSymbolCode[msBTC] ) then
    aKind := msBTC
  else if (aSymbol.Spec.BaseCode = TMajorSymbolCode[msETH] ) then
    aKind := msETH
  else Exit;

  if IsOSMain( aSymbol ) then
  begin
    CalcMainKimp( FSubExKind1 );
    CalcMainKimp( FSubExKind2 );
  end else
  if IsKRMain( aSymbol ) then
    CalcMainKimp( aSymbol.Spec.ExchangeType );
end;

procedure TSymbolCore.CalcMainKimp(aExKind: TExchangeKind);
var
  aBTC, aETH : TSymbol;
  dMo, dVal : double;
  sLog : string;
begin

  aBTC := FMainSymbols[msBTC][aExKind];
  aETH := FMainSymbols[msETH][aExKind];

  if ( aBTC = nil ) or ( aETH = nil ) then Exit;

  dMo := Max( aBTC.DayAmount + aETH.DayAmount, 1);
  dVal:= (aBTC.DayAmount * aBTC.KimpPrice + aETH.DayAmount * aETH.KimpPrice) / dMo ;

  if dVal <  0  then
  begin
   // App.DebugLog( '%s, %.2f = %f, %f, %f, %f', [ TExchangeKindDesc[aExKind],dVal, aBTC.KimpPrice, aBTC.DayAmount , aETH.KimpPrice, aETH.DayAmount]   );
  end;

  sLog := Format( '%s : %.1f = ( %.0f * %.3f ) + ( %.0f * %.3f ) / ( %.0f + %.0f ) ', [ TExchangeKindShortDesc[ aExKind ],
    dVal, aBTC.DayAmount , aBTC.KimpPrice , aETH.DayAmount , aETH.KimpPrice , aBTC.DayAmount , aETH.DayAmount
    ]);

  SetMainKimp( aExKind , dVal, sLog );

end;

procedure TSymbolCore.CalcMainWDC(aSymbol: TSymbol);
var
  aMainKind,   aSubKind   : TMajorSymbolKind;
  aMainSymbol, aSubSymbol : TSymbol;
  dAmt, dVal : double;
  sLog : string;
begin
  if (aSymbol.Spec.BaseCode = TMajorSymbolCode[msBTC] ) then  begin
    aMainKind := msBTC;
    aSubKind  := msETH;
  end
  else if (aSymbol.Spec.BaseCode = TMajorSymbolCode[msETH] ) then begin
    aMainKind := msETH;
    aSubKind  := msBTC;
  end
  else Exit;

  if aSymbol.Spec.ExchangeType = FSubExKind1 then begin
    aMainSymbol := FMainSymbols[aMainKind][FSubExKind1];
    aSubSymbol  := FMainSymbols[aSubKind][FSubExKind1];
  end
  else if aSymbol.Spec.ExchangeType = FSubExKind2  then begin
    aMainSymbol := FMainSymbols[aMainKind][FSubExKind2];
    aSubSymbol  := FMainSymbols[aSubKind][FSubExKind2];
  end
  else Exit;

  if ( aMainSymbol = nil ) or ( aSubSymbol = nil ) then Exit;

  dAmt := aMainSymbol.DayAmount + aSubSymbol.DayAmount ;
  if dAmt > EPSILON then begin
    dVal := ( aMainSymbol.DayAmount * aMainSymbol.WDCPrice + aSubSymbol.DayAmount * aSubSymbol.WDCPrice ) / dAmt;
    sLog := Format( '%s : %.1f = ( %.0f * %.1f ) + ( %.0f * %.1f ) / ( %.0f + %.0f ) ', [ TExchangeKindShortDesc[ aMainSymbol.Spec.ExchangeType ],
      dVal, aMainSymbol.DayAmount , aMainSymbol.WDCPrice, aSubSymbol.DayAmount , aSubSymbol.WDCPrice
      , aMainSymbol.DayAmount , aSubSymbol.DayAmount  ]);
    SetMainWDC( aMainSymbol.Spec.ExchangeType, dVal, sLog );
  end;
end;



constructor TSymbolCore.Create;
var
  I: TExchangeKind;
begin
  FSpecs:= TMarketSpecs.Create;

  for I := ekBinance to High( TExchangeKind ) do
  begin
    FSymbols[i] := TSymbolList.Create;
    FSpots[i]   := TSpots.Create;

    FFutures[i] := TFutures.Create;

    FMarkets[i]       := TMarketList.Create;
    FSpotMarkets[i]   := TSpotMarkets.Create;
    FFutureMarkets[i] := TFutureMarkets.Create;

    FExchanges[i]   := TMarketGroups.Create;
    FUnderlyings[i] := TMarketGroups.Create;

    FSymbolDnwStates[i] := TSymbolList.Create;

    FMainKimp[i] := 0.0;
    JKIdx[i]     := 0;

    WDCIdx[i]    := 0;
    FMainWDC[i]  := 0.0;
  end;


  FCommSymbols := TCommSymbolList.Create;
  FBaseSymbols := TBaseSymbols.Create;
  FMainExKind  := ekBinance;
  FSubExKind1  := ekUpbit;
  FSubExKind2  := ekBithumb;
  FMainExMarket:= mtFutures;

  FWCDays:= TWCDDataList.Create;
  FWCD30s:= TWCDDataList.Create;
end;

destructor TSymbolCore.Destroy;
var
  I: TExchangeKind;
begin

  for I := ekBinance to High( TExchangeKind ) do
  begin
    FSymbolDnwStates[i].Free;
    FSymbols[i].Free;
    FSpots[i].Free;

    FFutures[i].Free;


    FMarkets[i].Free;
    FSpotMarkets[i].Free;
    FFutureMarkets[i].Free;

    FExchanges[i].Free;
    FUnderlyings[i].Free;
  end;

  FWCDays.Free;
  FWCD30s.Free;

  FBaseSymbols.Free;
  FCommSymbols.Free;
  FSpecs.Free;

  inherited;
end;

//function TSymbolCore.FindQuoteSymbol(aExKind: TExchangeKind;
//  sBaseCode: string): TSymbol;
//var
//  I: Integer;
//begin
//  Result := nil;
//  // 일단 나중에..빠른  검색 로직을 추가 하자..
//  for I := 0 to FSpots[aExKind].Count -1 do
//    if FSpots[aExKind].Spots[i].Spec.BaseCode = sBaseCode then
//    begin
//      Result := FSpots[aExKind].Spots[i];
//      break;
//    end;
//end;

function TSymbolCore.FindSymbol(aExKind: TExchangeKind; aCode: string): TSymbol;
begin
  Result := FSymbols[aExKind].FindCode( aCode );
end;

procedure TSymbolCore.GetSymbolList(aExKind: TExchangeKind; var aList: TList);
var
  I: Integer;
begin
  aList.Clear;

  for I := 0 to FSpots[aExKind].Count-1 do
    aList.Add( FSpots[aExKind].Spots[i] as TSymbol );

end;

function TSymbolCore.IsKRMain(aSymbol: TSymbol): boolean;
begin
  if ( aSymbol.Spec.ExchangeType = FSubExKind1 )
    or ( aSymbol.Spec.ExchangeType = FSubExKind2 ) then
    Result := true
  else
    Result := false;
end;

function TSymbolCore.IsOSMain(aSymbol: TSymbol): boolean;
begin
  if ( aSymbol.Spec.ExchangeType = FMainExKind )
    and ( aSymbol.Spec.Market = FMainExMarket ) then
    Result := true
  else
    Result := false;
end;

procedure TSymbolCore.Log;
var
  I: TExchangeKind;
  j, k: Integer;
  aFutMarket : TFutureMarket;
  aGroup : TMarketGroup;
  aSymbol : TSymbol;
  aMarket : TMarket;
  stData : string;
  a: Integer;
begin


  App.DebugLog('%s_%s:%d, %s_%s:%d, %s:%d, %s:%d', [
    TExchangeKindDesc[ekBinance], TMarketTypeDesc[mtSpot], FSpots[ekBinance].Count
    , TExchangeKindDesc[ekBinance], TMarketTypeDesc[mtFutures], FFutures[ekBinance].Count
    , TExchangeKindDesc[ekUpbit], FSpots[ekUpbit].Count
    , TExchangeKindDesc[ekBithumb],  FSpots[ekBithumb].Count
    ]);

  Exit;

  for j := 0 to FBaseSymbols.Count-1 do
  begin
    App.DebugLog('======  %03d : BASE : %s ( %d ) ========', [ j, FBaseSymbols[j], TSymbolList(FBaseSymbols.Objects[j]).Count ]  )    ;
    with TSymbolList(FBaseSymbols.Objects[j]) do
      for k := 0 to Count-1 do
      begin
        aSymbol := Symbols[k];
        App.DebugLog('%03d(%03d) : %s %s %s', [ j, k, TExchangeKindDesc[aSymbol.Spec.ExchangeType],
                 TMarketTypeDesc[ aSymbol.Spec.Market], aSymbol.Spec.BaseCode]);
      end;

  end;

  for I := ekBinance to High(TExchangeKind) do
  begin
    App.DebugLog( '============  %s Symbols total : %03d  --> spot : %03d future : %03d'
      ,[ TExchangeKindDesc[i], FSymbols[i].Count, FSpots[i].Count, FFutures[i].Count ] );

    for j := 0 to FFutures[i].Count -1 do
    begin
      aSymbol := FFutures[i].Futures[j] as TSymbol;
      App.DebugLog( 'fut (%d), %s,  %s ', [
        j, aSymbol.Code, aSymbol.Spec.FQN
        ]);
    end;

    for j := 0 to Underlyings[i].Count - 1 do
    begin
      aGroup := Underlyings[i].Groups[j];
      for k := 0 to aGroup.Markets.Count - 1 do
      begin
        aMarket := aGroup.Markets.Markets[k];
        App.DebugLog( 'Under %d(%d), %s, %s, %s ', [
          j,k, aMarket.FQN, aMarket.Spec.FQN, aGroup.FQN
          ]);

      end;
    end;

    for j := 0 to FutureMarkets[i].Count -1 do
    begin
      aFutMarket := FutureMarkets[i].FutureMarkets[j];
      for k := 0 to aFutMarket.Symbols.Count - 1 do
      begin
        aSymbol := aFutMarket.Symbols.Symbols[k];
        App.DebugLog( 'fut %d(%d), %s, (%s) ', [
          j,k , aSymbol.Code, aSymbol.Spec.FQN
          ]);

      end;
    end;

  end;
end;

procedure TSymbolCore.MakePrevData;
var
  I: Integer;
  aWcd : TWCDData;
  dAmt, dVAl : double;
begin

  for I := 0 to FWCDays.Count-1 do
  begin
    aWcd := FWCDays.WCDs[i];
    aWcd.CalcWCD;
    aWcd.CalcRpsntWCD( ekUpbit);
    aWcd.CalcRpsntWCD( ekBithumb);
  end;

  for I := 0 to FWCD30s.Count-1 do
  begin
    aWcd := FWCD30s.WCDs[i];
    aWcd.CalcWCD;
    aWcd.CalcRpsntWCD( ekUpbit);
    aWcd.CalcRpsntWCD( ekBithumb);

    aWcd.CalcKIP;
    aWcd.CalcRpsntKIP(ekUpbit);
    aWcd.CalcRpsntKIP(ekBithumb);

    RprsntWDC[ekUpbit][FWCD30s.Count-1 - i] := aWcd.RpsntWCD[ekUpbit];
    RprsntWDC[ekBithumb][FWCD30s.Count-1 - i] := aWcd.RpsntWCD[ekBithumb];
    JungKopi [ekUpbit][FWCD30s.Count-1 - i] := aWcd.RpsntKIP[ekUpbit];
    JungKopi [ekBithumb][FWCD30s.Count-1 - i] := aWcd.RpsntKIP[ekBithumb];
  end;

  JKIdx[ekUpbit] :=  FWCD30s.Count-1;
  WDCIdx[ekUpbit]:=  FWCD30s.Count-1;
  JKIdx[ekBithumb] :=  FWCD30s.Count-1;
  WDCIdx[ekBithumb]:=  FWCD30s.Count-1;
end;

procedure TSymbolCore.PreSubscribe;
var
  i : TMajorSymbolKind;
  iRow, k : integer;
  j : TExchangeKind;
  aSymbol : TSymbol;
begin
  // 선구독 종목들...BTC, ETH, XRP

  for I := msBTC to High(TMajorSymbolKind) do
    for j := ekBinance to High(TExchangeKind) do
      if FMainSymbols[i][j] <> nil then
          App.Engine.QuoteBroker.Brokers[j].Subscribe(Self,  FMainSymbols[i][j],
            App.Engine.QuoteBroker.Brokers[j].DummyEventHandler
            );
        //App.DebugLog('Main Symbol [%s][%s] : %s', [ TMajorSymbolCode[i], TExchangeKindDesc[j], FMainSymbols[i][j].Code]   );
end;

procedure TSymbolCore.PreUnSubscribe;
begin
  App.Engine.QuoteBroker.Cancel( Self );
end;

procedure TSymbolCore.SubscribeSymbols;
begin

end;

function TSymbolCore.RegisterSymbol(aExKind: TExchangeKind;
  aMarket: TMarketType; aCode: string): TSymbol;
  var
    aSpec : TMarketSpec;
    sFQN  : string;
    aUnder: TSymbol;
begin
  case aMarket of
    mtSpot:
      begin
        Result := FSpots[aExKind].New(aCode);
        sFQN   := Format( '%s@%s', [ aCode,  GetBaseSpotFQN( aExKind ) ]);
        aSpec  := FSpecs.Find( sFQN );
        if aSpec = nil then
        begin
          aSpec := FSpecs.New(sFQN);
        end;
      end;
    mtFutures:
      begin
        aUnder := FSpots[aExKind].Find( aCode );
        if aUnder = nil then
          Exit ( nil );

        Result := FFutures[aExKind].New(aCode+Fut_Suf);   // add perpectual future Suffix;
        with Result as TFuture do
          Underlying := aUnder;

        aUnder.IsFuture := true;

        sFQN   := Format( '%s@%s.%s', [ aCode+Fut_Suf, aCode, GetBaseFuturesFQN( aExKind ) ]);
        aSpec  := FSpecs.Find( sFQN );
        if aSpec = nil then
        begin
          aSpec := FSpecs.New(sFQN);
        end;
      end;
  end;

  if aSpec <> nil then begin
    aSpec.ExchangeType  := aExKind;
    Result.Spec := aSpec
  end
  else
    Result := nil;
end;

procedure TSymbolCore.RepresentCoin;
begin
  //
//  FMainSymbols[msBTC][ekUpbit].DayAmount;
end;



procedure TSymbolCore.SetMainKimp(aExKind: TExchangeKind; Value: double; sLog : string);
var
  iPrev, idx : Integer;
  aWcd : TWCDData;
begin
  FMainKimp[aExKind] := Value;
  idx  := Get30TermIdx;
  iPrev :=  JKIdx[aExKind];
  JKIdx[aExKind]  := idx;
  JungKopi[aExKind][idx] := Value;
  JungKopiLog[aExKind][idx] := sLog;

  if iPrev <> idx then
    App.log(llInfo, 'KIP', sLog );
end;

procedure TSymbolCore.SetMainWDC(aExKind: TExchangeKind; Value: double; sLog : string);
var
  iPrev, idx : Integer;
begin
  FMainWDC[aExKind] := Value;
  idx  := Get30TermIdx;;
  iPrev := WDCIdx[aExKind];
  WDCIdx[aExKind] := idx;
  RprsntWDC[aExKind][idx] := Value;
  RprsntWDCLog[aExKind][idx] := sLog;

  if iPrev <> idx then
    App.log(llInfo, 'WDC', sLog );
end;


procedure TSymbolCore.SymbolArrange;
//var
//  I, idx: Integer;
//  aSymbol : TSymbol;
//  aList : TStrings;
//begin
//  Exit;
//  aList := TStringList.Create;
//  try
//    // 1 선물 없는 현물 삭제하기.
//    for I := FSpots[ekBinance].Count-1 downto 0 do
//    begin
//      aSymbol := FSpots[ekBinance].Spots[i];
//      if not aSymbol.IsFuture then
//      begin
//        idx := FSymbols[ekBinance].IndexOfObject( aSymbol );
//        if idx >= 0 then
//          FSymbols[ekBinance].Delete(idx);
//        App.DebugLog(' Binance Spot Arrangement : %s, %d, %d, %d', [ aSymbol.Code, FSpots[ekBinance].Count, i, idx ]  );
//        FSpots[ekBinance].Delete(i);
//      end;
//    end;
//  finally
//    aList.Free;
//  end;

var
  i : TMajorSymbolKind;
  iRow, k : integer;
  j : TExchangeKind;
  aSymbol : TSymbol;
begin
  // 선구독 종목들...BTC, ETH, XRP

  for I := msBTC to High(TMajorSymbolKind) do
  begin
      for j := ekBinance to High(TExchangeKind) do
      begin
        aSymbol := nil;
        if j= ekBinance then
          aSymbol := FBaseSymbols.FindSymbol( TMajorSymbolCode[i], j, FMainExMarket)
        else
          aSymbol := FBaseSymbols.FindSymbol( TMajorSymbolCode[i], j );

        if aSymbol <> nil then
        begin
          FMainSymbols[i][j] := aSymbol;
        end;
      end;
  end;
end;

procedure TSymbolCore.RegisterSymbol(aExKind : TExchangeKind; aSymbol: TSymbol);
var
  aMarket : TMarket;
begin
  if aSymbol = nil then Exit;

  if FSymbols[aExKind].IndexOfObject(aSymbol) < 0 then
    FSymbols[aExKind].AddObject(aSymbol.Code, aSymbol );

  if aSymbol.Spec <> nil then
  begin
    aMarket := FMarkets[aExKind].FindMarket( aSymbol.Spec.FQN );

    if aMarket = nil then
    begin

      case aSymbol.Spec.Market of
        mtSpot:     aMarket := FSpotMarkets[aExKind].New(aSymbol.Spec.FQN);
        mtFutures:  aMarket := FFutureMarkets[aExKind].New(aSymbol.Spec.FQN);
        else
          Exit;
      end;
    end;

    aMarket.Spec  := aSymbol.Spec;
    FMarkets[aExKind].AddMarket(aMarket);

    if (aSymbol.Spec.Market = mtFutures ) and (( aSymbol as TFuture ).Underlying <> nil ) then
        FUnderlyings[aExKind].AddMarket(aMarket,
                               aSymbol.Spec.SubMarket
                               + aSymbol.Spec.Underlying
                               + '.' + aSymbol.Spec.Exchange
                               + '.' + aSymbol.Spec.Country,
                               (aSymbol as TFuture).Underlying.Code,
                               (aSymbol as TFuture).Underlying);

    FExchanges[aExKind].AddMarket(aMarket,
                         aSymbol.Spec.Exchange + '.' + aSymbol.Spec.Country,
                         aSymbol.Spec.Exchange);


    if aSymbol.Spec.Country = 'kr' then
      FCommSymbols.Add( aSymbol );

    if ( aSymbol.Code = 'BTC' ) and ( aSymbol.Spec.ExchangeType = ekBithumb )  then
      App.DebugLog('');


    FBaseSymbols.AddSymbol( aSymbol );

  end;

  aMarket.AddSymbol( aSymbol );

end;



function TicksFromPrice(aSymbol: TSymbol; dPrice: Double; iTicks: Integer): Double;
var
  i, iSign : Integer;
begin
  Result := dPrice;

  if (iTicks = 0)
     or (aSymbol = nil)
     or (aSymbol.Spec = nil) then Exit;

  iSign := iTicks div Abs(iTicks);

  case aSymbol.Spec.ExchangeType of
    ekBinance: ;
    ekUpbit:
      begin
        for i:=1 to Abs(iTicks) do
        if iSign > 0 then
        begin
          if Result > 2000000.0 - EPSILON then
            Result := Result + 1000
          else if Result > 1000000 - EPSILON then
            Result := Result + 500
          else if Result > 500000 - EPSILON then
            Result := Result + 100
          else if Result > 100000 - EPSILON then
            Result := Result + 50
          else if Result > 10000 - EPSILON then
            Result := Result + 10
          else if Result > 1000 - EPSILON then
            Result := Result + 5
          else if Result > 100 - EPSILON then
            Result := Result + 1
          else if Result > 10 - EPSILON then
            Result := Result + 0.1
          else if Result > 1 - EPSILON then
            Result := Result + 0.01
          else if Result > 0.1 - EPSILON then
            Result := Result + 0.001
          else
            Result := Result + 0.0001;
        end else
        begin
          if Result < 0.1 + EPSILON then
            Result := Result - 0.0001
          else if Result < 1 + EPSILON then
            Result := Result - 0.001
          else if Result < 10 + EPSILON then
            Result := Result - 0.01
          else if Result < 100 + EPSILON then
            Result := Result - 0.1
          else if Result < 1000 + EPSILON then
            Result := Result - 1
          else if Result < 10000 + EPSILON then
            Result := Result - 5
          else if Result < 100000 + EPSILON then
            Result := Result - 10
          else if Result < 500000 + EPSILON then
            Result := Result - 50
          else if Result < 1000000 + EPSILON then
            Result := Result - 100
          else if Result < 2000000 + EPSILON then
            Result := Result - 500
          else
            Result := Result - 1000;
        end;
      end;
    ekBithumb:
      begin
        for i:=1 to Abs(iTicks) do
        if iSign > 0 then
        begin
          if Result > 1000000.0 - EPSILON then
            Result := Result + 1000
          else if Result > 500000.0 - EPSILON then
            Result := Result + 500
          else if Result > 100000 - EPSILON then
            Result := Result + 100
          else if Result > 50000 - EPSILON then
            Result := Result + 50
          else if Result > 10000 - EPSILON then
            Result := Result + 10
          else if Result > 5000 - EPSILON then
            Result := Result + 5
          else if Result > 1000 - EPSILON then
            Result := Result + 1
          else if Result > 100 - EPSILON then
            Result := Result + 0.1
          else if Result > 10 - EPSILON then
            Result := Result + 0.01
          else if Result > 1 - EPSILON then
            Result := Result + 0.001
          else
            Result := Result + 0.0001;
        end else
        begin
          if Result < 1 + EPSILON then
            Result := Result - 0.0001
          else if Result < 10 + EPSILON then
            Result := Result - 0.001
          else if Result < 100 + EPSILON then
            Result := Result - 0.01
          else if Result < 1000 + EPSILON then
            Result := Result - 0.1
          else if Result < 5000 + EPSILON then
            Result := Result - 1
          else if Result < 10000 + EPSILON then
            Result := Result - 5
          else if Result < 50000 + EPSILON then
            Result := Result - 10
          else if Result < 100000 + EPSILON then
            Result := Result - 50
          else if Result < 500000 + EPSILON then
            Result := Result - 100
          else if Result < 1000000 + EPSILON then
            Result := Result - 500
          else
            Result := Result - 1000;
        end;
      end;
  end;

end;

function LocksFromPrice(aSymbol: TSymbol; dPrice: Double; iTicks: Integer): Double;
var
  i, iSign : Integer;
begin
  Result := dPrice;

  if (iTicks = 0)
     or (aSymbol = nil)
     or (aSymbol.Spec = nil) then Exit;

  iSign := iTicks div Abs(iTicks);

  case aSymbol.Spec.ExchangeType of
    ekBinance: ;
    ekUpbit: ;
    ekBithumb:
      begin
        for i:=1 to Abs(iTicks) do
        if iSign > 0 then
        begin
          if Result > 1000000 - EPSILON then
            Result := Result + 0.0001
          else if Result > 100000 - EPSILON then
            Result := Result + 0.001
          else if Result > 10000 - EPSILON then
            Result := Result + 0.01
          else if Result > 1000 - EPSILON then
            Result := Result + 0.1
          else if Result > 100 - EPSILON then
            Result := Result + 1
          else
            Result := Result + 10;
        end else
        begin
          if Result < 100 + EPSILON then
            Result := Result - 10
          else if Result < 1000 + EPSILON then
            Result := Result - 1
          else if Result < 10000 + EPSILON then
            Result := Result - 0.1
          else if Result < 100000 + EPSILON then
            Result := Result - 0.01
          else if Result < 1000000 + EPSILON then
            Result := Result - 0.001
          else
            Result := Result - 0.0001;
        end;
      end;
  end;
end;

function GetPrecision(aSymbol: TSymbol; dPrice: Double): integer;
begin
   case aSymbol.Spec.ExchangeType of
    ekBinance: Result := aSymbol.Spec.Precision ;
    ekUpbit:
      begin
        if dPrice > 100 - EPSILON then
          Result := 0
        else if dPrice > 1 - EPSILON then
          Result := 1
        else if dPrice > 1 - EPSILON then
          Result := 2
        else if dPrice > 0.1 - EPSILON then
          Result := 3
        else
          REsult := 4;
      end;
    ekBithumb:
      begin
        if dPrice > 1000 - EPSILON then
          Result := 0
        else if dPrice > 100 - EPSILON then
          Result := 1
        else if dPrice > 10 - EPSILON then
          Result := 2
        else if dPrice > 1 - EPSILON then
          Result := 3
        else
          Result := 4;
      end;
   end;
end;

end.
