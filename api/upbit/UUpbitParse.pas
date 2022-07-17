unit UUpbitParse;

interface

uses
  System.Classes, System.SysUtils, System.DateUtils
  , System.JSON , USymbols , UExchangeManager , UQuoteBroker
  , UAccounts, UPositions
  , UApiTypes
  ;

type

  TUpbitParse = class
  private

    FParent: TExchangeManager;
    procedure ParseSpotOrderBook( aJson : TJsonObject );  overload;
    procedure ParseSpotTrade( aJson : TJsonObject );
    procedure ParseSpotRealTicker( aJson : TJsonObject );
    function GetSymbolCode(sCode: string): string;

  public
    constructor Create( aObj :TExchangeManager );
    destructor Destroy; override;

    // 처음에만..
    procedure ParseSpotTicker( aData : string );
    // 주기적 조회
    procedure ParseSpotTicker2( aData : string );
    procedure ParseSocketData( aMarket : TMarketType; aData : string);
    procedure ParseDNWSate( aData : string );
    procedure ParseSpotOrderBook( aData : string ); overload;
    procedure ParseCandleData( sUnit, aData : string );

    // private
    procedure ParseAccounts( aData : string );
    procedure ParseWaitORder( aData : string );

    property Parent : TExchangeManager read FParent;
  end;

var
  gUpReceiver : TUpbitParse;

implementation

uses
  GApp, GLibs  , Math , UTypes
  , UApiConsts
  , UOtherData
  , USymbolUtils
  ;

{ TUpbitParse }

constructor TUpbitParse.Create( aObj :TExchangeManager );
begin
  gUpReceiver := self;
  FParent     := aObj;
end;

destructor TUpbitParse.Destroy;
begin
  gUpReceiver := nil;
  inherited;
end;


procedure TUpbitParse.ParseSocketData(aMarket: TMarketType; aData: string);
var
  aObj : TJsonObject;
  aPair : TJsonPair;
  sTmp : string;
  aAccount : TAccount;
begin
  if aData = '' then
  begin
    App.Log(llError, '%s %s ParseSocketData data is empty',
       [ TExchangeKindDesc[FParent.ExchangeKind],  TMarketTypeDesc[aMarket] ] ) ;
    Exit;
  end;

  try

    aObj := TJsonObject.ParseJSONValue( aData) as TJsonObject;
    try
    	if aObj = nil then Exit;      
      aPair := aObj.Get('ty');
      if aPair <> nil then
      begin
        sTmp := aPair.JsonValue.Value;

        if sTmp.Compare(sTmp, 'ticker')= 0 then
          ParseSpotRealTicker( aObj )
        else if sTmp.Compare(sTmp, 'orderbook')= 0 then
          ParseSpotOrderBook( aObj )
        else if sTmp.Compare(sTmp, 'trade')= 0 then
          ParseSpotTrade( aObj )
        else Exit;
      end else  begin
        App.DebugLog(' %s, %s oops !! ....... %s ',
          [ TExchangeKindDesc[FParent.ExchangeKind],  TMarketTypeDesc[aMarket], aData ]);
      end;
    finally
      if aObj <> nil then aObj.Free;
    end;

  except
    App.DebugLog('%s %s parse error : %s', [TExchangeKindDesc[FParent.ExchangeKind],  TMarketTypeDesc[aMarket] , aData] );
  end;

end;

procedure TUpbitParse.ParseSpotOrderBook(aData: string);
var
  i , j : integer;
  aArr, aSubArr  : TJsonArray;
  aVal, aSubVal  : TJsonValue;
  sCode : string;
  sts   : TArray<string>;
  aSymbol : TSymbol;
begin
  if aData = '' then
  begin
    App.Log(llError, 'Upbit REST ParseSpotTicker data is empty') ;
    Exit;
  end;

  aArr := TJsonObject.ParseJSONValue( aData) as TJsonArray;
  try
    if aArr = nil then Exit;

    for I := 0 to aArr.Size-1 do
    begin
      aVal := aArr.Get(i);
      sCode:= aVal.GetValue<string>('market');

      sts := sCode.Split(['-']);
      aSymbol := App.Engine.SymbolCore.FindSymbol(ekUpbit, sts[1]);

//      if sts[1] = 'SC' then
//        sCode := 'SC';

      if aSymbol = nil then continue;

      aSubArr := aVal.FindValue('orderbook_units') as TJsonArray;

      for j := 0 to aSubArr.Size-1 do
      begin
        aSubVal := aSubArr.get(j);
        aSymbol.Asks[0].Price := aSubVal.GetValue<double>('ask_price');
        aSymbol.Asks[0].Volume:= aSubVal.GetValue<double>('ask_size');
        aSymbol.Bids[0].Price := aSubVal.GetValue<double>('bid_price');
        aSymbol.Bids[0].Volume:= aSubVal.GetValue<double>('bid_size');

//        if App.AppStatus > asLoad then
//          App.Engine.SymbolCore.CalcKimp( aSymbol );
        break;
      end;
    end;

  finally
    if aArr <> nil then aArr.Free;
  end;
end;

procedure TUpbitParse.ParseSpotRealTicker(aJson: TJsonObject);
var
  sCode : string;
  aQuote : TQuote;
begin
  sCode := GetSymbolCode(aJson.GetValue('cd').Value );
  aQuote  := App.Engine.QuoteBroker.Brokers[FParent.ExchangeKind].Find(sCode)    ;
  if (aQuote = nil) or (aQuote.Symbol = nil ) then Exit;

  with aQuote do
  begin
    Symbol.DayOpen :=  aJson.GetValue<double>( 'op' );
    Symbol.DayHigh :=  aJson.GetValue<double>( 'hp' );
    Symbol.DayLow  :=  aJson.GetValue<double>( 'lp' );

    Symbol.DayAmount   := aJson.GetValue<double>( 'atp24h' ) / 100000000;
    Symbol.DayVolume   := aJson.GetValue<double>( 'atv24h' );
  end;
end;

procedure TUpbitParse.ParseSpotOrderBook(aJson: TJsonObject);
var
  sCode : string;
  aQuote : TQuote;
  i : integer;
  aVal : TJsonValue;
  aArr : TJsonArray;
  dtTime: TDateTime;

begin
  sCode := GetSymbolCode(aJson.GetValue('cd').Value );
  aQuote  := App.Engine.QuoteBroker.Brokers[FParent.ExchangeKind].Find(sCode)    ;
  if (aQuote = nil) or (aQuote.Symbol = nil ) then Exit;

  dtTime  := UnixTimeToDateTime( aJson.GetValue<int64>('tms') );

  //  aJson.GetValue<double>('tas');

  with aQuote do
  begin
    aArr := aJson.GetValue('obu') as TJsonArray;
    for I := 0 to aArr.Size-1 do
    begin
      aVal  := aArr.Get(i);

      if i >= Symbol.Asks.Size then break;

      Symbol.Asks[i].Price  := aVal.GetValue<double>('ap');
      Symbol.Asks[i].Volume := aVal.GetValue<double>('as');
      Symbol.Bids[i].Price  := aVal.GetValue<double>('bp');
      Symbol.Bids[i].Volume := aVal.GetValue<double>('bs');

    end;

    LastEvent := qtMarketDepth;
    Update( dtTime );
  end;
end;

procedure TUpbitParse.ParseSpotTicker(aData: string);
var
  aArr : TJsonArray;
  aVal : TJsonValue;
  i : integer;
  sCode , sTmp : string;
  aSymbol : TSymbol;
  bNew : boolean;
  sts   : TArray<string>;
  aList : TList;
begin
  if aData = '' then
  begin
    App.Log(llError, 'Upbit ParseSpotTicker data is empty') ;
    Exit;
  end;
  aList := nil;
  aList:= TList.Create;
  aArr := TJsonObject.ParseJSONValue( aData) as TJsonArray;
  try

    for I := 0 to aArr.Count-1 do
    begin
      aVal := aArr.Items[i];
      sCode:= aVal.GetValue<string>('market');

      sts := sCode.Split(['-']);
      aSymbol := App.Engine.SymbolCore.FindSymbol(ekUpbit, sts[1]);

      if aSymbol = nil then
      begin
        bNew := true;
        aSymbol := App.Engine.SymbolCore.RegisterSymbol(ekUpbit, mtSpot, sts[1] );
        if aSymbol = nil then Exit;
      end else
        bNew := false;

      with aSymbol do
      begin
        OrgCode     := sCode;
        Spec.BaseCode    := sts[1];   // BTC ...
        Spec.QuoteCode   := sts[0];   // KRW ...
        Spec.SettleCode  := sts[0];
      end;

      aSymbol.DayOpen := StrToFloatDef( aVal.GetValue<string>( 'opening_price' ), 0.0 );
      aSymbol.DayHigh := StrToFloatDef( aVal.GetValue<string>( 'high_price' ), 0.0 );
      aSymbol.DayLow  := StrToFloatDef( aVal.GetValue<string>( 'low_price' ), 0.0 );
      aSymbol.Last    := StrToFloatDef( aVal.GetValue<string>( 'trade_price' ), 0.0 );
      aSymbol.PrevClose   := StrToFloatDef( aVal.GetValue<string>( 'prev_closing_price' ), 0.0 );
      aSymbol.DayAmount   := StrToFloatDef( aVal.GetValue<string>( 'acc_trade_price_24h' ), 0.0 ) / 100000000;
      aSymbol.DayVolume   := StrToFloatDef( aVal.GetValue<string>( 'acc_trade_volume_24h' ), 0.0 );

  //    aSymbol.Time  := UnixToDateTime(  aVal.GetValue<int64>( 'date' ) );
      aSymbol.LocalTime := now;

      if bNew then
        App.Engine.SymbolCore.RegisterSymbol( ekUpbit, aSymbol );

      if ( sts[1] <> 'BTC' ) and ( sts[1] <> 'ETH') and ( sts[1] <> 'XRP') then
        aList.Add( aSymbol );
    end;

    aList.Sort(CompareDailyAmount);
    if aList.Count > 2 then
    begin
      App.Engine.ApiManager.ExManagers[ekUpbit].TopAmtSymbols[0] := TSymbol( aList.Items[0] );
      App.Engine.ApiManager.ExManagers[ekUpbit].TopAmtSymbols[1] := TSymbol( aList.Items[1] );
    end;
  finally
    if aArr <> nil then aArr.Free;
    if aList <> nil then aList.Free;
  end;
end;


procedure TUpbitParse.ParseSpotTicker2(aData: string);
var
  aArr : TJsonArray;
  aVal : TJsonValue;
  i : integer;
  sCode , sTmp : string;
  aSymbol : TSymbol;
  sts   : TArray<string>;
  aQuote : TQuote;
  dtTime : TDateTime;
  bCalc : boolean;
  aList : TList;
  dPrice : double;

begin
  aList := nil;
  if aData = '' then
  begin
    App.Log(llError, 'Upbit ParseSpotTicker data is empty') ;
    Exit;
  end;

  aList:= TList.Create;
  aArr := TJsonObject.ParseJSONValue( aData) as TJsonArray;

  try

    for I := 0 to aArr.Size-1 do
    begin
      aVal := aArr.Get(i);
      sCode:= aVal.GetValue<string>('market');

      sts := sCode.Split(['-']);
      aSymbol := App.Engine.SymbolCore.FindSymbol(ekUpbit, sts[1]);

      if aSymbol = nil then  continue;

      dtTime := UnixTimeToDateTime( aVal.GetValue<int64>( 'trade_timestamp') );

      aSymbol.DayOpen := StrToFloatDef( aVal.GetValue<string>( 'opening_price' ), 0.0 );
      aSymbol.DayHigh := StrToFloatDef( aVal.GetValue<string>( 'high_price' ), 0.0 );
      aSymbol.DayLow  := StrToFloatDef( aVal.GetValue<string>( 'low_price' ), 0.0 );

      aSymbol.PrevClose   := StrToFloatDef( aVal.GetValue<string>( 'prev_closing_price' ), 0.0 );
      aSymbol.DayAmount   := StrToFloatDef( aVal.GetValue<string>( 'acc_trade_price_24h' ), 0.0 ) / 100000000;
      aSymbol.DayVolume   := StrToFloatDef( aVal.GetValue<string>( 'acc_trade_volume_24h' ), 0.0 );

      dPrice   := StrToFloatDef( aVal.GetValue<string>( 'trade_price' ), 0.0 );
      bCalc    := false;
      if dtTime > aSymbol.LastTradeTime then
      begin
        aSymbol.Last := dPrice;
        aSymbol.LastTradeTime := dtTime;
        aSymbol.LastTime      := now;
        bCalc   := true;
      end                                                         ;
//      else
//        App.DebugLog( 'ticker not up : %s -> (%s)%s, %s, %.6f', [ aSymbol.Code, aSymbol.PriceToStr(aSymbol.Last),
//              aSymbol.PriceToStr( dPrice )
//              , FormatDateTime('hh:nn:ss.zzz', dtTime), dtTime] );


      if App.AppStatus > asLoad then
      begin
  //  웹소켓이 불안해서 일단 실시간 구독한 종목도 계산한다. - 웹소켓 안정화 되면..빼야 됨.
  //      aQuote:= App.Engine.QuoteBroker.Brokers[FParent.ExchangeKind].Find(sCode)    ;
  //      if aQuote = nil then
  //      begin
        if bCalc then begin
          App.Engine.SymbolCore.CalcIndex( aSymbol );
          App.Engine.TradeCore.Positions[ aSymbol.Spec.ExchangeType].UpdatePosition( aSymbol);
        end;
  //      end;
      end;

      if ( App.Engine.SymbolCore.MainSymbols[msBTC][ekUpbit] <> aSymbol ) and
        ( App.Engine.SymbolCore.MainSymbols[msETH][ekUpbit] <> aSymbol ) and
        ( App.Engine.SymbolCore.MainSymbols[msXRP][ekUpbit] <> aSymbol ) then
        aList.Add( aSymbol );
    end;

    aList.Sort(CompareDailyAmount);
    if aList.Count > 2 then
    begin
      App.Engine.ApiManager.ExManagers[ekUpbit].TopAmtSymbols[0] := TSymbol( aList.Items[0] );
      App.Engine.ApiManager.ExManagers[ekUpbit].TopAmtSymbols[1] := TSymbol( aList.Items[1] );

//      with App.Engine.ApiManager.ExManagers[ekUpbit] do
//      begin
//        App.DebugLog('first : %s , %f',[ TopAmtSymbols[0].Code, TopAmtSymbols[0].DayAmount  ] );
//        App.DebugLog('first : %s , %f',[ TopAmtSymbols[1].Code, TopAmtSymbols[1].DayAmount  ] );
//      end;
    end;

  finally
    if aArr <> nil then aArr.Free;
    if aList <> nil then aList.Free;
  end;

end;


// .. 처음에 한번 전체 로드...
procedure TUpbitParse.ParseAccounts(aData: string);
var
  aArr : TJsonArray;
  aObj : TJsonObject;
  aVal : TJsonValue;
  I: Integer;
  aAcnt : TAccount;
  aSymbol : TSymbol;
  sTmp,sCode, sCur : string;
  aSCType : TSettleCurType;
  dTmp : double;
begin
  if aData = '' then
  begin
    App.Log(llError, '%s ParseDNWSate data is empty',
       [ TExchangeKindDesc[FParent.ExchangeKind] ] ) ;
    Exit;
  end;

  aAcnt := App.Engine.TradeCore.Accounts[ FParent.ExchangeKind].Accounts[0];
  if aAcnt = nil then Exit;

  aArr := nil;
  aArr := TJsonObject.ParseJSONValue( aData) as TJsonArray;

  try

    for I := 0 to aArr.Size-1 do
    begin
      aObj  := aArr.Get(i) as TJsonObject;
      sCur  := aObj.GetValue('currency').Value;
      sCode := aObj.GetValue('unit_currency').Value;
      aSCType := GetSettleType( sCur );

      dTmp    := aObj.GetValue<double>('balance', 0 );
      if aSCType <> scNone then
      begin
        aAcnt.TradeAmt[ aSCType ] := dTmp; //aObj.GetValue<double>('balance', 0 );
      end else
      begin
        aSymbol := App.Engine.SymbolCore.FindSymbol( FParent.ExchangeKind, sCur);
        if aSymbol = nil then continue;

        // 잔고가 잇으면..
        if (IsZero(dTmp )) and ( dTmp > 0 )then
        begin

        end;

      end;

      aObj.GetValue('unit_currency').Value;
    end;

  finally
    if aArr <> nil then aArr.Free;

  end;
end;

procedure TUpbitParse.ParseWaitORder(aData: string);
begin

end;

procedure TUpbitParse.ParseCandleData(sUnit, aData: string);
var
  aArr : TJsonArray;
  aVal : TJsonValue;
  I, j: Integer;
  bStart : boolean;
  d, dd, h, m, mt : word;
  sTmp, sCode : string;
  dTime, dtime2: TDateTime;
  aSymbol : TSymbol;
  accAmt, aAmt, aPrice, aClose : double;
  aWcd : TWCDData;
  aKind : TMajorSymbolKind;
begin
  if aData = '' then
  begin
    App.Log(llError, '%s ParseDNWSate data is empty',
       [ TExchangeKindDesc[FParent.ExchangeKind] ] ) ;
    Exit;
  end;
  aArr := nil;
  aArr := TJsonObject.ParseJSONValue( aData) as TJsonArray;
  try
    if aArr = nil then Exit;

    try

      dTime2 := Date;
      accAmt := 0;
      j      := 0;

      for I := 0 to aArr.Count-1 do
      begin

        aVal := aArr.Items[i];

        sCode:= GetSymbolCode( aVal.GetValue<string>('market') );

        if sCode = 'BTC' then
          aKind := msBTC
        else
          aKind := msETH;

        sTmp := aVal.GetValue<string>('candle_date_time_kst');
        dTime:= GetStrToTime( sTmp );

        aAmt   := aVal.GetValue<double>('candle_acc_trade_price');
        aPrice := aVal.GetValue<double>('trade_price');

        mt:= MonthOf(dTime);
        d := Dayof(  dTime );
        h := Hourof( dTime );
        m := MinuteOf(dTime);

        if sUnit = '240' then begin

          // 봉이 6개 시간은 05:00 ~ 09:00
          accAmt := accAmt + aAmt;
          // 1일 캔들 시작
          if h = 5 then begin
             // 24시간 종가가 됨
            aClose := aPrice;
            // 1일 캔들 시작 플래그
            bStart := true;
          end
          // 1일 캔들 종료
          else if h = 9 then
          begin
            if bStart then
            begin
              sTmp := Format('%2.2d00', [ d] );
              aWcd := App.Engine.SymbolCore.WCDays.Find( sTmp );
              if aWcd = nil then begin
                aWcd := App.Engine.SymbolCore.WCDays.New( sTmp );
                aWcd.m := mt;  aWcd.d := d;
              end;

              aWcd.Price[ aKind, FParent.ExchangeKind] := aClose;
              aWcd.Amount[aKind, FParent.ExchangeKind] := accAmt;
         //     App.DebugLog('%s %s %s %s %d -> %f, %f', [ sUnit, TExchangeKindDesc[ FParent.ExchangeKind], TMajorSymbolCode[aKind],
         //      sTmp,  App.Engine.SymbolCore.WCDays.Count,  aClose, accAmt ]);
              bStart := false;
              inc(j);
            end;
            accAmt := 0;
            if j >= 25 then break;
          end;

          dd := d;
        end
        else if sUnit = '30' then begin

          if not IsToday( dTime ) then break;

          sTmp := Format('%2.2d%2.2d', [ h,m] );
          aWcd := App.Engine.SymbolCore.WCD30s.Find( sTmp );
          if aWcd = nil then
            aWcd := App.Engine.SymbolCore.WCD30s.New( sTmp );

          aWcd.Price[ aKind, FParent.ExchangeKind] := aPrice;
          aWcd.Amount[aKind, FParent.ExchangeKind] := aAmt;
       //ugLog('%s %s %s %s %d -> %f, %f', [ sUnit, TExchangeKindDesc[ FParent.ExchangeKind], TMajorSymbolCode[aKind],
       //     sTmp,  App.Engine.SymbolCore.WCD30s.Count,  aPrice, aAmt ]);
        end;

      end;

    except
    end;

  finally
    if aArr <> nil  then aArr.Free;
  end;


end;

procedure TUpbitParse.ParseDNWSate(aData: string);
var
  aArr : TJsonArray;
  aVal : TJsonValue;
  I: Integer;
  sTmp : string;
  aSymbol : TSymbol;
begin
  if aData = '' then
  begin
    App.Log(llError, '%s ParseDNWSate data is empty',
       [ TExchangeKindDesc[FParent.ExchangeKind] ] ) ;
    Exit;
  end;

  try

    aArr := TJsonObject.ParseJSONValue( aData) as TJsonArray;
    try
      if aArr = nil then Exit;

      for I := 0 to aArr.Count-1 do
      begin
        aVal := aArr.Items[i];
        sTmp := aVal.GetValue<string>('currency');
        if FParent.Codes.IndexOf(sTmp) < 0 then continue;

        aSymbol := App.Engine.SymbolCore.FindSymbol( FParent.ExchangeKind, sTmp );
        if aSymbol <> nil then
        begin
          var iRes : integer ;        iRes := 0;

          if sTmp = 'BTC' then
            sTmp := 'BTC';

          sTmp := aVal.GetValue<string>('wallet_state');
          if sTmp = 'working' then
          begin
//            aSymbol.DepositState  := true;
//            aSymbol.WithDrawlState  := true;
            aSymbol.CheckDnwState( true,  true );
            continue;
          end else
          if sTmp = 'withdraw_only' then
            iRes := aSymbol.CheckDnwState( false,  true )
          else if sTmp = 'deposit_only' then
            iRes := aSymbol.CheckDnwState( true,  false )
          else if (sTmp = 'paused') or (sTmp = 'unsupported' ) then
            iRes := aSymbol.CheckDnwState( false,  false );

          if iRes > 0 then
            App.Engine.SymbolBroker.DnwEvent( aSymbol, iRes);

        end;

      end;

    finally
      if aArr <> nil then aArr.Free;
    end;
  except on e : exception do
    App.Log(llError, 'Upbit ParseDNWState parse error : %s, %s ' ,[ e.Message, aData ] );
  end;
end;


function TUpbitParse.GetSymbolCode(sCode: string): string;
var
  sts   : TArray<string>;
begin
  sts := sCode.Split(['-']);
  Result := sts[1];
end;

procedure TUpbitParse.ParseSpotTrade(aJson: TJsonObject);
var
  I: Integer;
  sCode, sTmp : string;
  aQuote: TQuote;
  dtTime : TDateTime;
  aSale  : TTimeNSale;
begin

  sCode := GetSymbolCode(aJson.GetValue('cd').Value );
  aQuote  := App.Engine.QuoteBroker.Brokers[FParent.ExchangeKind].Find(sCode)    ;
  if (aQuote = nil) or (aQuote.Symbol = nil ) then Exit;

  with aQuote do
  begin

    dtTime  := UnixTimeToDateTime( aJson.GetValue<int64>('ttms') );

//    if Symbol.Code = 'BTC' then
//      App.DebugLog( 'BTC time : %s %.6f', [ FormatDateTime('hh:nn:ss.zzz', dtTime), dtTime] )   ;


    aSale := Symbol.Sales.New;
    aSale.LocalTime := now;
    aSale.Time      := dtTime;
    aSale.Price     := aJson.GetValue<double>('tp');
    aSale.Volume    := aJson.GetValue<double>('tv');

    aSale.Side      := ifthen( aJson.GetValue<string>('ab') = 'ASK', -1, 1 );

    Symbol.Last := aSale.Price;
    Symbol.Volume := aSale.Volume;
    Symbol.Side   := aSale.Side;

    LastEvent := qtTimeNSale;

    Update(dtTime );
  end;

end;



end.
