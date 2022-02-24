unit UBinanceParse;
interface
uses
  System.Classes, System.SysUtils, System.DateUtils
  , System.JSON  //, Rest.Json , Rest.Types
  , UApiTypes
  ;
type
  TBinanceParse = class
  public
    constructor Create;
    destructor Destroy; override;
    procedure ParseMarginPair( aData : string );
    procedure ParseSpotTicker( aData : string );

    procedure ParseFuttMaster( aData : string );
    procedure ParseFuttTicker( aData : string );
  end;
var
  gBinReceiver : TBinanceParse;
implementation
uses
  GApp   , UApiConsts
  , USymbols
  ;
{ TBinanceParse }
constructor TBinanceParse.Create;
begin
  gBinReceiver := self;
end;
destructor TBinanceParse.Destroy;
begin
  gBinReceiver := nil;
  inherited;
end;

procedure TBinanceParse.ParseFuttMaster(aData: string);
var
  aVal : TJsonValue;

  aObj, aFil  : TJsonObject;
  aArr, aArr2  : TJsonArray;
  aSymbol : TSymbol;
  I, j, iPre: Integer;
  sTmp, sCode, aFiType : string;
  bNew : boolean;
  dSize, dSize2 : double;
begin
  if aData = '' then
  begin
    App.Log(llError, 'Binance ParseFuttMaster data is empty') ;
    Exit;
  end;

  aArr := (TJsonObject.ParseJSONValue( aData ) as TJsonObject).Get('symbols').JsonValue as TJsonArray;

  for I := 0 to aArr.Size-1 do
  begin
    aObj  := aArr.Get(i) as TJsonObject;
    sTmp  := aObj.GetValue('quoteAsset').Value;
    if sTmp <> 'USDT' then Continue;

    sCode   := aObj.GetValue('symbol').Value;
    sTmp    := sCode + Fut_Suf;  // add perpectual future Suffix;
    aSymbol := App.Engine.SymbolCore.FindSymbol(ekBinance, sTmp);
    if aSymbol = nil then
    begin
      bNew := true;
      aSymbol := App.Engine.SymbolCore.RegisterSymbol(ekBinance, mtFutures, sCode );
      if aSymbol = nil then Exit;
    end else
      bNew := false;

    with aSymbol do
    begin
      OrgCode     := sCode;
      Spec.BaseCode    := aObj.GetValue('baseAsset').Value;;
      Spec.QuoteCode   := aObj.GetValue('quoteAsset').Value;  // USTD
      Spec.SettleCode  := aObj.GetValue('symbol').Value;;
    end;

    sTmp := aObj.GetValue('status').Value;
    if sTmp = 'TRADING' then aSymbol.TradeAble := true  else aSymbol.TradeAble := false;

    iPRe  :=  aObj.GetValue<integer>('pricePrecision');
    //iPre  := aObj.getvalue('pricePrecision');
    aArr2 := aObj.Get('filters').JsonValue as TJsonArray;

    for j := 0 to aArr2.Size-1 do
    begin
      aFil  := aArr2.Get(j) as TJsonObject;
      aFiType := aFil.GetValue('filterType').Value;
      if aFiType = 'PRICE_FILTER' then
      begin
        sTmp := aFil.GetValue('tickSize').Value;
        dSize := StrToFloatDef( sTmp, 1.0 );
      end else
      if aFiType = 'LOT_SIZE' then
      begin
        //sTmp := aFil.GetValue('minQty').Value;
        sTmp := aFil.GetValue('stepSize').Value;
        dSize2  := StrToFloatDef( sTmp, 1.0);
      end;
    end;

    aSymbol.Spec.SetSpec( iPre, dSize, dSize2 );

    if bNew then
      App.Engine.SymbolCore.RegisterSymbol( ekBinance, aSymbol );


  end;
end;

procedure TBinanceParse.ParseFuttTicker(aData: string);
var
  aArr : TJsonArray;
  aVal : TJsonValue;
  I: Integer;
  sTmp : string;
  dTmp : double;
  bAble : boolean;
  aSymbol : TSymbol;

begin
  if aData = '' then
  begin
    App.Log(llError, 'Binance ParseFuttTicker data is empty') ;
    Exit;
  end;

  aArr := TJsonObject.ParseJSONValue( aData) as TJsonArray;

  for I := 0 to aArr.Size-1 do
  begin
    aVal := aArr.Get(i);
    sTmp := aVal.GetValue<string>('symbol');
    aSymbol := App.Engine.SymbolCore.FindSymbol(ekBinance, sTmp+Fut_Suf);
    if aSymbol <> nil then
    begin

      aSymbol.DayOpen := StrToFloatDef( aVal.GetValue<string>( 'openPrice' ), 0.0 );
      aSymbol.DayHigh := StrToFloatDef( aVal.GetValue<string>( 'highPrice' ), 0.0 );
      aSymbol.DayLow  := StrToFloatDef( aVal.GetValue<string>( 'lowPrice' ), 0.0 );
      aSymbol.Last    := StrToFloatDef( aVal.GetValue<string>( 'lastPrice' ), 0.0 );
      aSymbol.DayAmount   := StrToFloatDef( aVal.GetValue<string>( 'quoteVolume' ), 0.0 )
        * App.Engine.ApiManager.ExRate.Value;

      aSymbol.DayVolume   := StrToFloatDef( aVal.GetValue<string>( 'volume' ), 0.0 );

      aSymbol.Time  := UnixToDateTime(  aVal.GetValue<int64>( 'closeTime' ) );
      aSymbol.LocalTime := now;

//    선물에 호가 정보 제공 않마..
//      aSymbol.Asks[0].Price   := StrToFloatDef( aVal.GetValue<string>( 'askPrice' ), 0.0 );
//      aSymbol.Asks[0].Volume  := StrToFloatDef( aVal.GetValue<string>( 'askQty' ), 0.0 );
//
//      aSymbol.Bids[0].Price   := StrToFloatDef( aVal.GetValue<string>( 'bidPrice' ), 0.0 );
//      aSymbol.Bids[0].Volume  := StrToFloatDef( aVal.GetValue<string>( 'bidQty' ), 0.0 );

    end;
  end;


end;

procedure TBinanceParse.ParseMarginPair(aData: string);
var
  aArr : TJsonArray;
  aVal : TJsonValue;
  I: Integer;
  sTmp : string;
  bAble : boolean;
  aSymbol : TSymbol;
begin
  if aData = '' then
  begin
    App.Log(llError, 'Binance ParseMarginPair data is empty') ;
    Exit;
  end;

  aArr := TJsonObject.ParseJSONValue( aData) as TJsonArray;

  for I := 0 to aArr.Size-1 do
  begin
    aVal  := aArr.Get(i) ;
    sTmp  := aVal.GetValue<string>('quote');
    if sTmp <> 'USDT' then continue;
    bAble := aVal.GetValue<boolean>('isMarginTrade');
    if not bAble then Continue;
    sTmp := aVal.GetValue<string>('symbol');

    aSymbol := App.Engine.SymbolCore.FindSymbol(ekBinance, sTmp);
    if aSymbol <> nil then
    begin
      aSymbol.IsMargin := true;
//      App.DebugLog('%s Enabled Margin Trade', [ aSymbol.Code] );
    end;

  end;

end;

procedure TBinanceParse.ParseSpotTicker(aData: string);
var
  aArr : TJsonArray;
  aVal : TJsonValue;
  I: Integer;
  sTmp : string;
  dTmp : double;
  bAble : boolean;
  aSymbol : TSymbol;

begin
  if aData = '' then
  begin
    App.Log(llError, 'Binance ParseSpotTicker data is empty') ;
    Exit;
  end;

  aArr := TJsonObject.ParseJSONValue( aData) as TJsonArray;

  for I := 0 to aArr.Size-1 do
  begin
    aVal := aArr.Get(i);
    sTmp := aVal.GetValue<string>('symbol');
    aSymbol := App.Engine.SymbolCore.FindSymbol(ekBinance, sTmp);
    if aSymbol <> nil then
    begin

      aSymbol.DayOpen := StrToFloatDef( aVal.GetValue<string>( 'openPrice' ), 0.0 );
      aSymbol.DayHigh := StrToFloatDef( aVal.GetValue<string>( 'highPrice' ), 0.0 );
      aSymbol.DayLow  := StrToFloatDef( aVal.GetValue<string>( 'lowPrice' ), 0.0 );
      aSymbol.Last    := StrToFloatDef( aVal.GetValue<string>( 'lastPrice' ), 0.0 );
      aSymbol.DayAmount   := StrToFloatDef( aVal.GetValue<string>( 'quoteVolume' ), 0.0 );
      aSymbol.DayVolume   := StrToFloatDef( aVal.GetValue<string>( 'volume' ), 0.0 );
      aSymbol.PrevClose   := StrToFloatDef( aVal.GetValue<string>( 'prevClosePrice' ), 0.0 );

      aSymbol.Time  := UnixToDateTime(  aVal.GetValue<int64>( 'closeTime' ) );
      aSymbol.LocalTime := now;

      aSymbol.Asks[0].Price   := StrToFloatDef( aVal.GetValue<string>( 'askPrice' ), 0.0 );
      aSymbol.Asks[0].Volume  := StrToFloatDef( aVal.GetValue<string>( 'askQty' ), 0.0 );

      aSymbol.Bids[0].Price   := StrToFloatDef( aVal.GetValue<string>( 'bidPrice' ), 0.0 );
      aSymbol.Bids[0].Volume  := StrToFloatDef( aVal.GetValue<string>( 'bidQty' ), 0.0 );

    end;
  end;

end;

end.
