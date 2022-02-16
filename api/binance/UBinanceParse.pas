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
  end;
var
  gBinReceiver : TBinanceParse;
implementation
uses
  GApp
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
  aSymbol, aUnder : TSymbol;
  I: Integer;
  sTmp : string;
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

    sTmp   := aObj.GetValue('symbol').Value;
    aUnder := App.Engine.SymbolCore.FindSymbol(ekBinance, sTmp );
    if aUnder = nil then Continue;

    sTmp := sTmp + 'PF';  // perpectual future;
//    aSymb


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
      App.DebugLog('%s Enabled Margin Trade', [ aSymbol.Code] );
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
