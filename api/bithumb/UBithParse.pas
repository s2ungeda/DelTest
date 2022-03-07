unit UBithParse;

interface

uses
  System.Classes, System.SysUtils, System.DateUtils
  , System.JSON , USymbols, UExchangeManager
  , UApiTypes
  ;

type

  TSendDoneNotify = procedure(  Sender : TObject ) of object;

  TBithParse = class
  private

    FOnSendDone: TSendDoneNotify;
    FParent: TExchangeManager;
    procedure ParseSpotOrderBook( aJson : TJsonObject ); overload;
    procedure ParseSpotTrade( aJson : TJsonObject );
    procedure ParseSpotMiniTickers( aJson : TJsonObject );

    function GetSymbolCode(sCode : string): string;

  public
    constructor Create( aObj :TExchangeManager );
    destructor Destroy; override;

    procedure ParseDnwState( aData : string );
    procedure ParseSpotOrderBook( aData : string ); overload;
    procedure ParseSocketData( aMarket : TMarketType; aData : string);

    property Parent : TExchangeManager read FParent;
    property OnSendDone : TSendDoneNotify read FOnSendDone write FOnSendDone;
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

constructor TBithParse.Create( aObj :TExchangeManager );
begin
  gBithReceiver := self;
  FParent := aObj;
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

      aSymbol := App.Engine.SymbolCore.FindSymbol(FParent.ExchangeKind, sCode );
      if aSymbol <> nil then
      begin
        aSub  := aPair.JsonValue as TJsonObject;
        aArr  := aSub.Get('bids').JsonValue as TJsonArray;

        if aArr.Count > 0 then begin
          aVal  := aArr.Get(0);
          aSymbol.Bids[0].Price   := StrToFloatDef( aVal.GetValue<string>( 'price' ), 0.0 );
          aSymbol.Bids[0].Volume  := StrToFloatDef( aVal.GetValue<string>( 'quantity' ), 0.0 );
        end;

        aArr  := aSub.Get('asks').JsonValue as TJsonArray;

        if aArr.Count > 0 then
        begin
          aVal  := aArr.Get(0);
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


procedure TBithParse.ParseDnwState(aData: string);
var
  aSub, aObj : TJsonObject;
  aPair : TJsonPair;
  sTmp : string;
  I, iw, id: Integer;
  aSymbol : TSymbol;
begin
  if aData = '' then
  begin
    App.Log(llError, '%s ParseDnwState data is empty', [ TExchangeKindDesc[FParent.ExchangeKind] ] ) ;
    Exit;
  end;

  aObj := TJsonObject.ParseJSONValue( aData) as TJsonObject;
  try
    sTmp := aObj.GetValue('status').Value;
    if ( sTmp = '' ) or ( sTmp <> '0000' ) then Exit;

    aSub  := aObj.GetValue('data') as TJsonObject;

    for I := 0 to aSub.Size-1 do
    begin
      aPair := aSub.Get(i);
      sTmp := aPair.JsonString.Value;
      if FParent.Codes.IndexOf(sTmp) < 0 then Continue;
      aSymbol := App.Engine.SymbolCore.FindSymbol(FParent.ExchangeKind, sTmp);

      iw := aPair.JsonValue.GetValue<integer>('withdrawal_status');
      id := aPair.JsonValue.GetValue<integer>('deposit_status');

      aSymbol.WithDrawlState := iw = 1;
      aSymbol.DepositState   := id = 1;

    end;


  finally
    if aObj <> nil then aObj.free;
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
       [ TExchangeKindDesc[FParent.ExchangeKind],  TMarketTypeDesc[aMarket] ] ) ;
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
    end else  begin
      aPair := aObj.Get('status');

      if aPair <> nil then
      begin
        sTmp := aPair.JsonValue.Value;
        if sTmp = '0000' then begin
          if Assigned(FOnSendDone) then
            FOnSendDone( Self );
        end
        else
          App.DebugLog(' %s, %s oops !! ....... %s ', [ TExchangeKindDesc[FParent.ExchangeKind],  TMarketTypeDesc[aMarket], aData ]);
      end;
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
  aQuote:= App.Engine.QuoteBroker.Brokers[FParent.ExchangeKind].Find(sCode)    ;
  if (aQuote = nil) or (aQuote.Symbol = nil ) then Exit;

  with aQuote do
  begin
    Symbol.DayOpen := StrToFloatDef( aVal.GetValue<string>( 'openPrice' ), 0.0 );
    Symbol.DayHigh := StrToFloatDef( aVal.GetValue<string>( 'highPrice' ), 0.0 );
    Symbol.DayLow  := StrToFloatDef( aVal.GetValue<string>( 'lowPrice' ), 0.0 );

    Symbol.PrevClose   := StrToFloatDef( aVal.GetValue<string>( 'prevClosePrice' ), 0.0 );
    Symbol.DayAmount   := StrToFloatDef( aVal.GetValue<string>( 'value' ), 0.0 ) / 100000000;
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
        aQuote  := App.Engine.QuoteBroker.Brokers[FParent.ExchangeKind].Find(sCode)    ;
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

