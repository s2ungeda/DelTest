unit USymbolCore;

interface

uses
  system.Classes, system.SysUtils , system.DateUtils,

  UFQN, UMarketSpecs, USymbols, UMarkets,

  UApiTypes,

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
  TMainSymbols    = array [TMajorSymbolKind] of  TMajorSymbols;

  TMainKimpArray  = array [TExchangeKind] of double;

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

  public

    JungKopi  : array [TExchangeKind, 0..47] of double;

    constructor Create;
    destructor Destroy; override;

    procedure RegisterSymbol(aExKind : TExchangeKind; aSymbol: TSymbol); overload;
    function  RegisterSymbol(aExKind : TExchangeKind; aMarket : TMarketType; aCode : string ) : TSymbol ; overload;
    function  FindSymbol(aExKind : TExchangeKind; aCode : string ): TSymbol;
    function  FindQuoteSymbol(aExKind : TExchangeKind; sBaseCode : string ): TSymbol;

    procedure Log;
    procedure PreSubscribe;   // 한종목씩
    procedure SubscribeSymbols; // 모든종목 한방에
    procedure RepresentCoin;

    procedure GetSymbolList( aExKind : TExchangeKind; var aList : TList );

//    function CalcKimp( aOSSymbol, aKSymbol : TSymbol; iType : integer ) : double;
    procedure SetMainKimp( aExKind : TExchangeKind; Value : double );

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

    property MainExKind : TExchangeKind read FMainExKind write FMainExKind;
    property SubExKind1 : TExchangeKind read FSubExKind1 write FSubExKind1;
    property SubExKind2 : TExchangeKind read FSubExKind2 write FSubExKind2;

  end;

function TicksFromPrice(aSymbol: TSymbol; dPrice: Double; iTicks: Integer): Double;
function LocksFromPrice(aSymbol: TSymbol; dPrice: Double; iTicks: Integer): Double;
function GetPrecision(aSymbol: TSymbol; dPrice: Double): integer;

implementation

uses
  GApp, UConsts
  , Math
  ;


{ TSymbolCore }

//function TSymbolCore.CalcKimp( aOSSymbol, aKSymbol : TSymbol; iType : integer ) : double;
//var
//  tmp : TSymbol;
//  dEx : double;
//begin
//  if (aOSSymbol = nil) or (aKSymbol = nil) then Exit (0);
////
//  dEx :=  Max( aOSSymbol.Last * App.Engine.ApiManager.ExRate.Value , 1 );
//
//  case iType of
//    -1 : Result := ( aKSymbol.Asks[0].Price - dEx) / dEx ;
//    0 : Result := ( aKSymbol.Last - dEx) / dEx ;
//    1 : Result := ( aKSymbol.Bids[0].Price - dEx) / dEx ;
//  end;
////
////  if ( aKSymbol.Code = 'BTC' ) and ( Result < 0 ) and ( iType < 0 )
////    and ( aKSymbol.Spec.ExchangeType = ekUpbit )  then
////    App.DebugLog( 'ex : %f %f, %f, %f (%s, %s) ', [ Result, aKSymbol.Asks[0].Price, aOSSymbol.Last, App.Engine.ApiManager.ExRate.Value
////    , aOSSymbol.Code, aKSymbol.Code ] );
//
////  if iType = -1 then
//
//
//  Result := Result * 100;
////  if Result < 0 then Result := 0.0;
//
//end;

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
  end;

  FCommSymbols := TCommSymbolList.Create;
  FBaseSymbols := TBaseSymbols.Create;
  FMainExKind  := ekBinance;
  FSubExKind1  := ekUpbit;
  FSubExKind2  := ekBithumb;

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

  FBaseSymbols.Free;
  FCommSymbols.Free;
  FSpecs.Free;

  inherited;
end;

function TSymbolCore.FindQuoteSymbol(aExKind: TExchangeKind;
  sBaseCode: string): TSymbol;
var
  I: Integer;
begin
  Result := nil;
  // 일단 나중에..빠른  검색 로직을 추가 하자..
  for I := 0 to FSpots[aExKind].Count -1 do
    if FSpots[aExKind].Spots[i].Spec.BaseCode = sBaseCode then
    begin
      Result := FSpots[aExKind].Spots[i];
      break;
    end;
end;

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

procedure TSymbolCore.PreSubscribe;
var
  i : TMajorSymbolKind;
  iRow : integer;
  j : TExchangeKind;
  aSymbol : TSymbol;
begin
  // 선구독 종목들...BTC, ETH, XRP
  for I := msBTC to High(TMajorSymbolKind) do
    for j := ekBinance to High(TExchangeKind) do
    begin
      aSymbol := App.Engine.SymbolCore.FindQuoteSymbol( j, TMajorSymbolCode[i] );
      if aSymbol <> nil then
      begin
        FMainSymbols[i][j] := aSymbol;
        // 전종목 구독할것이기에..주석
        App.Engine.QuoteBroker.Brokers[j].Subscribe(Self, aSymbol,
          App.Engine.QuoteBroker.Brokers[j].DummyEventHandler
          );
      end;
    end;

//  for I := msBTC to High(TMajorSymbolKind) do
//    for j := ekBinance to High(TExchangeKind) do
//      if FMainSymbols[i][j] <> nil then
//        App.DebugLog('Main Symbol [%s][%s] : %s', [ TMajorSymbolCode[i], TExchangeKindDesc[j], FMainSymbols[i][j].Code]   );
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

procedure TSymbolCore.SetMainKimp(aExKind: TExchangeKind; Value: double);
var
  yy, mm, dd, hh, nn, ss, zz : word;
  idx : Integer;
begin
  FMainKimp[aExKind] := Value;
  DecodeDateTime( now, yy, mm, dd, hh, nn, ss, zz );

  idx  := ( hh mod 24 * 2 ) + ( nn div 30 );

  JungKopi[aExKind][idx] := Value;
end;



procedure TSymbolCore.SymbolArrange;
var
  I, idx: Integer;
  aSymbol : TSymbol;
  aList : TStrings;
begin
  Exit;
  aList := TStringList.Create;
  try
    // 1 선물 없는 현물 삭제하기.
    for I := FSpots[ekBinance].Count-1 downto 0 do
    begin
      aSymbol := FSpots[ekBinance].Spots[i];
      if not aSymbol.IsFuture then
      begin
        idx := FSymbols[ekBinance].IndexOfObject( aSymbol );
        if idx >= 0 then
          FSymbols[ekBinance].Delete(idx);
        App.DebugLog(' Binance Spot Arrangement : %s, %d, %d, %d', [ aSymbol.Code, FSpots[ekBinance].Count, i, idx ]  );
        FSpots[ekBinance].Delete(i);
      end;
    end;
  finally
    aList.Free;
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
