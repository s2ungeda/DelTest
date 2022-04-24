unit UUpbitParse;

interface

uses
  System.Classes, System.SysUtils, System.DateUtils
  , System.JSON , USymbols , UExchangeManager , UQuoteBroker
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

    property Parent : TExchangeManager read FParent;
  end;

var
  gUpReceiver : TUpbitParse;

implementation

uses
  GApp, GLibs  , Math , UTypes
  , UApiConsts
  , UOtherData
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
      aObj.Free;
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
begin
  if aData = '' then
  begin
    App.Log(llError, 'Upbit ParseSpotTicker data is empty') ;
    Exit;
  end;

  aArr := TJsonObject.ParseJSONValue( aData) as TJsonArray;

  for I := 0 to aArr.Size-1 do
  begin
    aVal := aArr.Get(i);
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
begin
  if aData = '' then
  begin
    App.Log(llError, 'Upbit ParseSpotTicker data is empty') ;
    Exit;
  end;

  aArr := TJsonObject.ParseJSONValue( aData) as TJsonArray;

  for I := 0 to aArr.Size-1 do
  begin
    aVal := aArr.Get(i);
    sCode:= aVal.GetValue<string>('market');

    sts := sCode.Split(['-']);
    aSymbol := App.Engine.SymbolCore.FindSymbol(ekUpbit, sts[1]);

    if aSymbol = nil then  continue;

    aSymbol.DayOpen := StrToFloatDef( aVal.GetValue<string>( 'opening_price' ), 0.0 );
    aSymbol.DayHigh := StrToFloatDef( aVal.GetValue<string>( 'high_price' ), 0.0 );
    aSymbol.DayLow  := StrToFloatDef( aVal.GetValue<string>( 'low_price' ), 0.0 );
    aSymbol.Last    := StrToFloatDef( aVal.GetValue<string>( 'trade_price' ), 0.0 );
    aSymbol.PrevClose   := StrToFloatDef( aVal.GetValue<string>( 'prev_closing_price' ), 0.0 );
    aSymbol.DayAmount   := StrToFloatDef( aVal.GetValue<string>( 'acc_trade_price_24h' ), 0.0 ) / 100000000;
    aSymbol.DayVolume   := StrToFloatDef( aVal.GetValue<string>( 'acc_trade_volume_24h' ), 0.0 );

    if App.AppStatus > asLoad then
    begin
      aQuote:= App.Engine.QuoteBroker.Brokers[FParent.ExchangeKind].Find(sCode)    ;
      if aQuote = nil then
      begin
        App.Engine.SymbolCore.CalcKimp( aSymbol );
        App.Engine.SymbolCore.CalcMainKimp( aSymbol );
        App.Engine.SymbolCore.CalcMainWDC(aSymbol);
      end;
    end;
  end;

end;

procedure TUpbitParse.ParseCandleData(sUnit, aData: string);
var
  aArr : TJsonArray;
  aVal : TJsonValue;
  I, j: Integer;
  bStart : boolean;
  d, dd, h, m : word;
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

      for I := 0 to aArr.Size-1 do
      begin

        aVal := aArr.Get(i);

        sCode:= GetSymbolCode( aVal.GetValue<string>('market') );

        if sCode = 'BTC' then
          aKind := msBTC
        else
          aKind := msETH;

        sTmp := aVal.GetValue<string>('candle_date_time_kst');
        dTime:= GetStrToTime( sTmp );

        aAmt   := aVal.GetValue<double>('candle_acc_trade_price');
        aPrice := aVal.GetValue<double>('trade_price');

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
              if aWcd = nil then
                aWcd := App.Engine.SymbolCore.WCDays.New( sTmp );

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

      for I := 0 to aArr.Size-1 do
      begin
        aVal := aArr.Get(i);
        sTmp := aVal.GetValue<string>('currency');
        if FParent.Codes.IndexOf(sTmp) < 0 then continue;

        aSymbol := App.Engine.SymbolCore.FindSymbol( FParent.ExchangeKind, sTmp );
        if aSymbol <> nil then
        begin
          var iRes : integer ;        iRes := 0;

          sTmp := aVal.GetValue<string>('wallet_state');
          if sTmp = 'working' then
          begin
            aSymbol.DepositState  := true;
            aSymbol.WithDrawlState  := true;
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

  if sCode = 'BTC' then
    sCode := 'BTC';

  with aQuote do
  begin

    dtTime  := UnixTimeToDateTime( aJson.GetValue<int64>('tms') );

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
