unit UBithParse;

interface

uses
  System.Classes, System.SysUtils, System.DateUtils
  , System.JSON , USymbols
  , UApiTypes
  ;

type

  TBithParse = class
  private
    FExKind: TExchangeKind;
    procedure ParseSpotOrderBook( aJson : TJsonObject ); overload;
    procedure ParseSpotTrade( aJson : TJsonObject );
    procedure ParseSpotMiniTickers( aJson : TJsonObject );

    function GetSymbolCode(sCode : string): string;

  public
    constructor Create( aExKind : TExchangeKind );
    destructor Destroy; override;

    procedure ParseSpotOrderBook( aData : string ); overload;
    procedure ParseSocketData( aMarket : TMarketType; aData : string);

    property ExKind : TExchangeKind read FExKind;
  end;

var
  gBithReceiver : TBithParse;
implementation

uses
  GApp, GLibs, Math
  , UApiConsts
  , UQuoteBroker
  ;

{ TBithParse }

constructor TBithParse.Create( aExKind : TExchangeKind );
begin
  gBithReceiver := self;
  FExKind := aExKind;
end;

destructor TBithParse.Destroy;
begin
  gBithReceiver := nil;
  inherited;
end;





function TBithParse.GetSymbolCode(sCode: string): string;
var
  sts   : TArray<string>;
begin
  sts := sCode.Split(['_']);
  Result := sts[0];
end;

procedure TBithParse.ParseSpotOrderBook(aData: string);
var
  aObj, aSub, master : TJsonObject;
  aArr : TJsonArray;
  aPair : TJsonPair;
  aVal  : TJsonValue;
  i : Integer;
  sBase, sCode, sTmp : string;
  aSymbol : TSymbol;
  bNew : boolean;
  j: Integer;
begin
  master := TJsonObject.ParseJSONValue( aData ) as TJsonObject;
  aObj := master.GetValue('data') as TJsonObject;

  try
    for I := 0 to aObj.Size-1 do
    begin
      aPair := aObj.Get(i);
      if aPair.JsonValue.ClassType <> TJSONObject then continue;
      sCode := aPair.JsonString.Value;

      aSymbol := App.Engine.SymbolCore.FindSymbol(FExKind, sCode );
      if aSymbol <> nil then
      begin
        aSub  := aPair.JsonValue as TJsonObject;
        aArr  := aSub.Get('bids').JsonValue as TJsonArray;

        if aArr.Count > 0 then begin
          aVal  := aArr.Get(aArr.Count-1);
          aSymbol.Bids[0].Price   := StrToFloatDef( aVal.GetValue<string>( 'price' ), 0.0 );
          aSymbol.Bids[0].Volume  := StrToFloatDef( aVal.GetValue<string>( 'quantity' ), 0.0 );
        end;

        aArr  := aSub.Get('asks').JsonValue as TJsonArray;

        if aArr.Count > 0 then
        begin
          aVal  := aArr.Get(aArr.Count-1);
          aSymbol.Asks[0].Price   := StrToFloatDef( aVal.GetValue<string>( 'price' ), 0.0 );
          aSymbol.Asks[0].Volume  := StrToFloatDef( aVal.GetValue<string>( 'quantity' ), 0.0 );
        end;
      end;

    end;
  //  if ( GetCodeIndex( sCode ) < 0 ) then Continue;

  finally
    aObj.Free
  end;

end;




procedure TBithParse.ParseSocketData(aMarket: TMarketType; aData: string);
var
  aObj : TJsonObject;
  aPair : TJsonPair;
  sTmp : string;
begin
  if aData = '' then
  begin
    App.Log(llError, '%s %s ParseSocketData data is empty',
       [ TExchangeKindDesc[FExKind],  TMarketTypeDesc[aMarket] ] ) ;
    Exit;
  end;

  aObj := TJsonObject.ParseJSONValue( aData) as TJsonObject;
  try
    aPair := aObj.Get('type');
    if aPair <> nil then
    begin
      sTmp := aPair.JsonValue.Value;

      if sTmp.Compare(sTmp, 'ticker')= 0 then
        ParseSpotMiniTickers( aObj )
      else if sTmp.Compare(sTmp, 'orderbookdepth')= 0 then
        ParseSpotOrderBook( aObj )
      else if sTmp.Compare(sTmp, 'transaction')= 0 then
        ParseSpotTrade( aObj )
      else Exit;
    end;
  finally
    aObj.Free;
  end;
end;

procedure TBithParse.ParseSpotOrderBook(aJson: TJsonObject);
var
  aArr : TJsonArray;
  aVal : TJsonValue;
  I: Integer;
  sCode, sTmp : string;
  aQuote : TQuote;
begin
  aVal := aJson.GetValue('content') ;
  if aVal = nil then Exit;

  aArr := aVal.FindValue('list') as TJsonArray ;
  if aArr = nil then Exit;

  aQuote := nil;

  try


  except
  end;
end;

procedure TBithParse.ParseSpotMiniTickers(aJson: TJsonObject);
var
  aArr : TJsonArray;
  aVal : TJsonValue;
  I: Integer;
  sCode, sTmp : string;
  aQuote : TQuote;
begin
  aVal := aJson.GetValue('content') ;
  if aVal = nil then Exit;

  sCode := GetSymbolCode( aVal.GetValue<string>('symbol') );
  aQuote:= App.Engine.QuoteBroker.Brokers[FExKind].Find(sCode)    ;
  if (aQuote = nil) or (aQuote.Symbol = nil ) then Exit;

  with aQuote do
  begin
    Symbol.DayOpen := StrToFloatDef( aVal.GetValue<string>( 'openPrice' ), 0.0 );
    Symbol.DayHigh := StrToFloatDef( aVal.GetValue<string>( 'highPrice' ), 0.0 );
    Symbol.DayLow  := StrToFloatDef( aVal.GetValue<string>( 'lowPrice' ), 0.0 );

    Symbol.PrevClose   := StrToFloatDef( aVal.GetValue<string>( 'prevClosePrice' ), 0.0 );
    Symbol.DayAmount   := StrToFloatDef( aVal.GetValue<string>( 'value' ), 0.0 );
    Symbol.DayVolume   := StrToFloatDef( aVal.GetValue<string>( 'volume' ), 0.0 );
  end;
end;

procedure TBithParse.ParseSpotTrade(aJson: TJsonObject);
var
  aArr : TJsonArray;
  aVal : TJsonValue;
  I: Integer;
  sCode, sTmp : string;
  aQuote: TQuote;
  dtTime : TDateTime;
  aSale  : TTimeNSale;
begin
  aVal := aJson.GetValue('content') ;
  if aVal = nil then Exit;

  aArr := aVal.FindValue('list') as TJsonArray ;
  if aArr = nil then Exit;

  aQuote := nil;

  try

    for I := 0 to aArr.Size-1 do
    begin
      aVal  := aArr.get(i);
      // BTC_KRW -> BTC
      sCode := GetSymbolCode( aVal.GetValue<string>('symbol') );

      if aQuote = nil then
        aQuote  := App.Engine.QuoteBroker.Brokers[FExKind].Find(sCode)    ;
      if (aQuote = nil) or (aQuote.Symbol = nil ) then Exit;

      with aQuote do
      begin
        sTmp  := aVal.GetValue<string>('contDtm');

        dtTime:= EncodeDate(  StrToInt(copy(sTmp, 1, 4 ) ) , StrToInt( copy(sTmp, 6, 2 )) , StrToInt( copy(sTmp, 9,2)) )
                + EnCodeTime(StrToInt(copy(sTmp, 12,2))
                          ,StrToInt(copy(sTmp, 15,2))
                          ,StrToInt(copy(sTmp, 18,2))
                          ,StrToInt(copy(sTmp, 21,3)) )  ;

        aSale := Symbol.Sales.New;
        aSale.LocalTime := now;
        aSale.Time      := dtTime;
        aSale.Price     := StrToFloat( aVal.GetValue<string>('contPrice'));
        aSale.Volume    := StrToFloat( aVal.GetValue<string>('contQty'));

        aSale.Side      := ifthen( aVal.GetValue<string>('buySellGb') = '1', -1, 1 );

        Symbol.Last := aSale.Price;
        Symbol.Volume := aSale.Volume;
        Symbol.Side   := aSale.Side;

        LastEvent := qtTimeNSale;

        Update(dtTime );
      end;

    end;
  except
  end;
  
end;

end.

