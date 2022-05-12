unit UBithParse;

interface

uses
  System.Classes, System.SysUtils, System.DateUtils
  , System.JSON , USymbols, UExchangeManager
  , UApiTypes, UOtherData
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
    procedure ParseTicker( aData : string );
    procedure ParseSpotOrderBook( aData : string ); overload;
    procedure ParseSocketData( aMarket : TMarketType; aData : string);
    procedure ParseCandleData( aKind: TMajorSymbolKind;  sUnit , aData : string );

    property Parent : TExchangeManager read FParent;
    property OnSendDone : TSendDoneNotify read FOnSendDone write FOnSendDone;
  end;

var
  gBithReceiver : TBithParse;
implementation

uses
  GApp, GLibs, Math , UTypes
  , UApiConsts
  , UQuoteBroker
  , USymbolUtils
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
  aQuote : TQuote;
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

        if App.AppStatus > asLoad then  begin
          App.Engine.SymbolCore.CalcKimp( aSymbol );
          aQuote:= App.Engine.QuoteBroker.Brokers[FParent.ExchangeKind].Find(aSymbol.Code)    ;
          if aQuote <> nil then  begin
            aQuote.LastEvent := qtMarketDepth;
            aQuote.Update(now);
          end;
        end;
      end;

    end;
  //  if ( GetCodeIndex( sCode ) < 0 ) then Continue;

  finally
    aObj.Free
  end;
end;

// 24 는 0 시기준 -> 뒤에서 부터 24까지만..
// 30 분 은 날자 파싱해서..오늘날자 까지만. 만.

procedure TBithParse.ParseCandleData(aKind: TMajorSymbolKind; sUnit, aData: string);
var
  aObj : TJsonObject;
  aPair : TJsonPair;
  iResult : integer;
  aArr, aSubArr : TJsonArray;
  I, j, iCnt: Integer;    iTime : int64;
  dTime : TDateTime;
  aNum : TJSONNumber;
  sTmp, sTmp2 : string;
  aWcd : TWCDData;
  d, h, m, mt : word;
  aPrice, amt : double;
begin
  if aData = '' then
  begin
    App.Log(llError, '%s ParseDnwState data is empty', [ TExchangeKindDesc[FParent.ExchangeKind] ] ) ;
    Exit;
  end;

  aObj := nil;
  aObj := TJsonObject.ParseJSONValue( aData) as TJsonObject;
  try

    try
      iResult := StrToInt( aObj.GetValue('status').Value );
      if iResult <> 0 then begin
        App.Log(llError, 'Request Result %d (ParseCandleData)', [ iResult ] );
        Exit;
      end;

      aArr := aObj.GetValue('data') as TJsonArray;

      j := 0;
      for i := aArr.Size-1 downto 0 do
      begin
        if ( sUnit = '24h' ) and ( j >= 25 ) then break;

        aSubArr := aArr.Get(i) as TJsonArray;

        aNum  := aSubArr.Get(0) as TJSONNumber ;
        iTime := aNum.GetValue<int64>;
        dTime := UnixTimeToDateTime( iTime );
        sTmp  := aSubArr.Get(2).Value;
        sTmp2 := aSubArr.Get(5).Value;

        aPrice:= StrToFloat( sTmp );
        amt   := StrToFloat( sTmp2 );

        mt:= MonthOf(dTime);
        d := Dayof(  dTime );
        h := Hourof( dTime );
        m := MinuteOf(dTime);

        inc(j);

        if (sUnit = '30m') and ( not IsToday( dTime )) then
          break;

        aWcd := nil;

        if (sUnit = '24h') and ( not IsToday( dTime )) then begin
          sTmp := Format('%2.2d00', [ d] );
          aWcd := App.Engine.SymbolCore.WCDays.Find( sTmp );
          if aWcd = nil then begin
            aWcd := App.Engine.SymbolCore.WCDays.New( sTmp );
            aWcd.m := mt;  aWcd.d := d;
          end;
          iCnt := App.Engine.SymbolCore.WCDays.Count;
        end else
        if (sUnit = '30m') and (( m = 30 ) or ( m = 0)) then
        begin
          sTmp := Format('%2.2d%2.2d', [ h, m] );
          aWcd := App.Engine.SymbolCore.WCD30s.Find( sTmp );
          if aWcd = nil then
            aWcd := App.Engine.SymbolCore.WCD30s.New( sTmp );
          iCnt := App.Engine.SymbolCore.WCD30s.Count;
        end;

        if aWcd <> nil then
        begin
          aWcd.Price[ aKind, FParent.ExchangeKind] := aPrice;
          aWcd.Amount[aKind, FParent.ExchangeKind] := amt * aPrice;
//          App.DebugLog('%s, %s %s %s %d -> %f, %f', [ sUnit, TExchangeKindDesc[ FParent.ExchangeKind], TMajorSymbolCode[aKind],
//            sTmp, iCnt ,  aPrice, amt * aPrice ]);
        end;

        if j > 50 then break;

      end;

    except
    end;
  finally
    if aObj <> nil then aObj.Free;
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

      if sTmp = 'BTC' then
        sTmp := 'BTC';

      var iRes : integer;
      iRes := aSymbol.CheckDnwState( id = 1, iw = 1 ) ;
      if iRes > 0 then
        App.Engine.SymbolBroker.DnwEvent( aSymbol, iRes);
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

procedure TBithParse.ParseTicker(aData: string);
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
  aQuote : TQuote;
  dtTime : TDateTime;
  bCalc : boolean;
  dTmp  : double;
  aList : TList;
begin
  master := TJsonObject.ParseJSONValue( aData ) as TJsonObject;
  aObj := master.GetValue('data') as TJsonObject;

  aList := TList.Create;
  try

    try

      dtTime  := UnixTimeToDateTime( aObj.GetValue<int64>('date') );

      for I := 0 to aObj.Size-1 do
      begin
        aPair := aObj.Get(i);
        if aPair.JsonValue.ClassType <> TJSONObject then continue;
        sCode := aPair.JsonString.Value;

        aSymbol := App.Engine.SymbolCore.FindSymbol(FParent.ExchangeKind, sCode );
        if aSymbol <> nil then
        begin

          bCalc := false;
          aVal := aPair.JsonValue;

          aSymbol.DayHigh     := StrToFloatDef( aVal.GetValue<string>( 'max_price' ), 0.0 );
          aSymbol.DayLow      := StrToFloatDef( aVal.GetValue<string>( 'min_price' ), 0.0 );
          aSymbol.DayOpen     := StrToFloatDef( aVal.GetValue<string>( 'opening_price' ), 0.0 );

          aSymbol.PrevClose   := StrToFloatDef( aVal.GetValue<string>( 'prev_closing_price' ), 0.0 );
          aSymbol.DayAmount   := StrToFloatDef( aVal.GetValue<string>( 'acc_trade_value_24H' ), 0.0 ) / 100000000;
          aSymbol.DayVolume   := StrToFloatDef( aVal.GetValue<string>( 'units_traded_24H' ), 0.0 );
          dTmp                := StrToFloatDef( aVal.GetValue<string>( 'closing_price' ), 0.0 );

          if dtTime > aSymbol.LastEventTime then
          begin
            aSymbol.Last          := dTmp ;
            aSymbol.LastEventTime := dtTime;
            aSymbol.LastTime      := now;
            bCalc := true;
          end;

//            App.DebugLog( 'ticker not up : %s -> (%s)%s, %s', [ aSymbol.Code, aSymbol.PriceToStr(aSymbol.Last),
//              aSymbol.PriceToStr( dTmp )
//              , FormatDateTime('hh:nn:ss', dtTime)] );


          if App.AppStatus > asLoad then
          begin
//  웹소켓이 불안해서 일단 실시간 구독한 종목도 계산한다. - 웹소켓 안정화 되면..빼야 됨.
//            aQuote:= App.Engine.QuoteBroker.Brokers[FParent.ExchangeKind].Find(sCode)    ;
//            if aQuote = nil then
//            begin
//            if bCalc then begin

              App.Engine.SymbolCore.CalcKimp( aSymbol );
              App.Engine.SymbolCore.CalcMainKimp( aSymbol );
              App.Engine.SymbolCore.CalcMainWDC(aSymbol);
//            end;
//            end;
          end;

          if ( App.Engine.SymbolCore.MainSymbols[msBTC][ekBithumb] <> aSymbol ) and
            ( App.Engine.SymbolCore.MainSymbols[msETH][ekBithumb] <> aSymbol ) and
            ( App.Engine.SymbolCore.MainSymbols[msXRP][ekBithumb] <> aSymbol ) then
            aList.Add( aSymbol );

        end;
      end;

      aList.Sort(CompareDailyAmount);
      if aList.Count > 2 then
      begin
        App.Engine.ApiManager.ExManagers[ekBithumb].TopAmtSymbols[0] := TSymbol( aList.Items[0] );
        App.Engine.ApiManager.ExManagers[ekBithumb].TopAmtSymbols[1] := TSymbol( aList.Items[1] );
      end;

    except on e : exception do
      App.log( llError,  ' %s ParseTicker Error', [  TExchangeKindDesc[ FParent.ExchangeKind], aData ] ) ;
    end;
  //  if ( GetCodeIndex( sCode ) < 0 ) then Continue;

  finally
    if aObj <> nil then aObj.Free ;
    aList.Free;
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

