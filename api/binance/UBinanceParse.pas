unit UBinanceParse;
interface
uses
  System.Classes, System.SysUtils, System.DateUtils
  , System.JSON  //, Rest.Json , Rest.Types
  , UApiTypes    , UExchangeManager
  , UAccounts, UPositions, UOrders
  ;

const
  SUCC_CODE = 200;

type
  TBinanceParse = class
  private
    FParent: TExchangeManager;

    procedure ParseSpotBookTicker( aJson : TJsonObject );
    procedure ParseSpotTrade( aJson : TJsonObject );
    procedure ParseSpotMiniTickers( aJson : TJsonObject );

    procedure ParseFutMarketDepth( aJson : TJsonObject );
    procedure ParseFutaggTrade( aJson : TJsonObject );
    procedure ParseFutBookTicker( aJson : TJsonObject );
    procedure ParseAllMiniTicker( aArr: TJsonArray );

    procedure ParseFutAccountUpdate( aJson : TJsonObject );
    procedure ParseFutOrderUpdate( aJson : TJsonObject );
    function GetOrderStatus(const s: string): TOrderState;

  public
    constructor Create( aObj :TExchangeManager );
    destructor Destroy; override;

    procedure ParseMarginPair( aData : string );
    procedure ParseDNWState( aData : string );
    procedure ParseSpotTicker( aData : string );
    procedure ParseSpotBalance(aData : string );

    procedure ParseFuttMaster( aData : string );
    function  ParsePrepareFuttMaster( aData : string ) : boolean;
    procedure ParseFuttTicker( aData : string );
    procedure ParseListenKey(  aMarket : TMarketType; aData : string );
    procedure ParseFutBalance(aData : string );
    procedure ParseFutPosition(aData : string );

    procedure ParseSocketData( aMarket : TMarketType; aData : string);
    procedure ParseCandleData( aKind: TMajorSymbolKind; sUnit, aData : string );

    procedure ParseFutAllTicker( aData : string );
    procedure ParseFutAllOrderBook( aData : string );

    property Parent : TExchangeManager read FParent;

  end;
var
  gBinReceiver : TBinanceParse;
implementation
uses
  GApp   , UApiConsts,  UConsts
  , GLibs  , UTypes
  , UQuoteBroker
  , UOtherData
  , USymbols  , UFills
  ;
{ TBinanceParse }
constructor TBinanceParse.Create( aObj :TExchangeManager );
begin
  gBinReceiver := self;
  FParent := aObj;
end;
destructor TBinanceParse.Destroy;
begin
  gBinReceiver := nil;
  inherited;
end;



procedure TBinanceParse.ParseFutAccountUpdate(aJson: TJsonObject);
begin

end;

procedure TBinanceParse.ParseFutaggTrade(aJson: TJsonObject);
var
  sCode : string;
  aQuote : TQuote;
  dtTime : TDateTime;
  aSale  : TTimeNSale;
  iTime  : int64;
  bSide  : boolean;
begin
  sCode := aJson.GetValue('s').Value;


  aQuote  := App.Engine.QuoteBroker.Brokers[FParent.ExchangeKind].Find(sCode+Fut_Suf)    ;
  if (aQuote = nil) or (aQuote.Symbol = nil ) then Exit;

  with aQuote do
  begin

    iTime := aJson.GetValue<int64>('T');
    dtTime:= UnixTimeToDateTime( iTime );

//    App.DebugLog('time compare %s, %d', [ FormatDateTime('hh:nn:ss.zzz', dtTime), iTime ]   );

    aSale := Symbol.Sales.New;
    aSale.LocalTime := now;
    aSale.Time      := dtTime;
    aSale.Price     := StrToFloat( aJson.GetValue('p').Value );
    aSale.Volume    := StrToFloat( aJson.GetValue('q').Value );

    bSide := aJson.GetValue<boolean>('m');
    	// true : 매도  ,  false : 매수
    if bSide then aSale.Side := -1
    else aSale.Side := 1;

    Symbol.Last   := aSale.Price;
    Symbol.Volume := aSale.Volume;
    Symbol.Side   := aSale.Side;
    Symbol.LastID := aJson.GetValue<int64>('l');

    LastEvent := qtTimeNSale;

    Update( dtTime );
  end;

end;

procedure TBinanceParse.ParseFutAllOrderBook(aData: string);
var
	aVal : TJsonValue;
	aArr : TJsonArray;
  aObj : TJsonObject;
  sCode : string;
  aSymbol : TSymbol;
  I: Integer;
  iTrns : int64;
begin
	aVal := nil;
  if aData = '' then
  begin
    App.Log(llError, '%s ParseFutAllOrderBook data is empty', [ TExchangeKindDesc[FParent.ExchangeKind] ] ) ;
    Exit;
  end;        
  
  aVal := TJsonObject.ParseJSONValue( aData);// as TJsonArray;  
  if aVal = nil then Exit;  
  if not (aVal is TJsonArray) then
  begin
  	App.Log(llError, 'ParseFutAllOrderBook InValid data : %s', [ aData ] );
    Exit;
  end;

  try                       

  	aArr := aVal as TJsonArray;
  
  	for I := 0 to aArr.Size-1 do
		begin
    	aObj	:= aArr.Get(i) as TJsonObject;

		  sCode := aObj.GetValue('symbol').Value;
      aSymbol := App.Engine.SymbolCore.FindSymbol(ekBinance, sCode+Fut_Suf);
      if aSymbol <> nil then
      begin
      	iTrns	:= aObj.GetValue<int64>('time');
      	if iTrns > aSymbol.TrnsTime then
        begin
      
          aSymbol.Asks[0].Price  := aObj.GetValue<double>('askPrice');
          aSymbol.Asks[0].Volume := aObj.GetValue<double>('askQty');
          aSymbol.Bids[0].Price  := aObj.GetValue<double>('bidPrice');
          aSymbol.Bids[0].Volume := aObj.GetValue<double>('bidQty');

          aSymbol.TrnsTime	:= iTrns;
        end;

      end;      
    end;
    
  finally
    if aVal <> nil then aVal.Free;
  end;

end;

procedure TBinanceParse.ParseFutAllTicker(aData: string);
var
	aVal : TJsonValue;
	aArr : TJsonArray;
  aObj : TJsonObject;
  sCode : string;
  aSymbol : TSymbol;
  I: Integer;
  iLast : int64;
begin
	aVal := nil;
  if aData = '' then
  begin
    App.Log(llError, '%s ParseFutAllTicker data is empty', [ TExchangeKindDesc[FParent.ExchangeKind] ] ) ;
    Exit;
  end;        
  
  aVal := TJsonObject.ParseJSONValue( aData);// as TJsonArray;  
  if aVal = nil then Exit;  
  if not (aVal is TJsonArray) then
  begin
  	App.Log(llError, 'ParseFutAllTicker InValid data : %s', [ aData ] );
    Exit;
  end;

  try                       

  	aArr := aVal as TJsonArray;
  
  	for I := 0 to aArr.Size-1 do
		begin
    	aObj	:= aArr.Get(i) as TJsonObject;

		  sCode := aObj.GetValue('symbol').Value;
      aSymbol := App.Engine.SymbolCore.FindSymbol(ekBinance, sCode+Fut_Suf);
      if aSymbol <> nil then
      begin
        aSymbol.DayOpen  := aObj.GetValue<double>('openPrice');
        aSymbol.DayHigh  := aObj.GetValue<double>('highPrice');
        aSymbol.DayLow   := aObj.GetValue<double>('lowPrice');
        aSymbol.DayVolume:= aObj.GetValue<double>('volume') ;
        aSymbol.DayAmount:= aObj.GetValue<double>('quoteVolume') * App.Engine.ApiManager.ExRate.Value / 100000000;

       	iLast	:= aObj.GetValue<int64>('lastId');
        if iLast > aSymbol.LastID then
        begin
          aSymbol.Last := aObj.GetValue<double>('lastPrice');

          App.Engine.SymbolCore.CalcIndex( aSymbol );
          App.Engine.TradeCore.Positions[ aSymbol.Spec.ExchangeType].UpdatePosition( aSymbol);
//
//          App.DebugLog('%s : %d > %d %s, %.0f', [ aSymbol.Code, iLast, aSymbol.LastID, 
//          	aSymbol.PriceToStr( aSymbol.Last ), aSymbol.DayAmount
//          	]  );
          aSymbol.LastID	:= iLast;
        end;

      end;      
    end;
    
  finally
    if aVal <> nil then aVal.Free;
  end;
end;



procedure TBinanceParse.ParseFutBookTicker(aJson: TJsonObject);
begin

end;

// 조회로 변경했기에..사용안함..
procedure TBinanceParse.ParseAllMiniTicker(aArr: TJsonArray);
var
  I : Integer;
  iLast : int64;
  aVal : TJsonValue;
  aObj : TJsonObject;
  sCode, sTmp : string;
  aSymbol : TSymbol;
  aQuote  : TQuote;
begin
  for I := 0 to aArr.Size-1 do
  begin
    aObj := aArr.Get(i) as TJsonObject ;
    sTmp := aObj.GetValue('e').Value;

    if sTmp <> '24hrTicker' then exit;

    sCode := aObj.GetValue('s').Value;
    aSymbol := App.Engine.SymbolCore.FindSymbol(ekBinance, sCode+Fut_Suf);
    if aSymbol <> nil then
    begin
      aSymbol.DayOpen  := StrToFloat( aObj.GetValue('o').Value );
      aSymbol.DayHigh  := StrToFloat( aObj.GetValue('h').Value );
      aSymbol.DayLow   := StrToFloat( aObj.GetValue('l').Value );
      aSymbol.DayVolume:= StrToFloat( aObj.GetValue('v').Value );
      aSymbol.DayAmount:= StrToFloat( aObj.GetValue('q').Value ) * App.Engine.ApiManager.ExRate.Value / 100000000;

      iLast	:= aObj.GetValue<int64>('L');
      if iLast > aSymbol.LastID then
      begin
        aSymbol.Last := aObj.GetValue<double>('c');
        App.Engine.SymbolCore.CalcKimp( aSymbol );
        App.Engine.SymbolCore.CalcMainKimp( aSymbol );
        App.Engine.SymbolCore.CalcMainWDC(aSymbol);

//        App.DebugLog('%s : %d > %d %s, %.0f', [ aSymbol.Code, iLast, aSymbol.LastID, 
//          aSymbol.PriceToStr( aSymbol.Last ), aSymbol.DayAmount
//          ]  );
        aSymbol.LastID	:= iLast;
      end;      

    end;
  end;

//  for I := 0 to aArr.Size-1 do
//  begin
//    aObj := aArr.Get(i) as TJsonObject ;
//    sTmp := aObj.GetValue('e').Value;
//
//    if sTmp <> '24hrMiniTicker' then exit;
//
//    sCode := aObj.GetValue('s').Value;
//    aSymbol := App.Engine.SymbolCore.FindSymbol(ekBinance, sCode+Fut_Suf);
//    if aSymbol <> nil then
//    begin
//      aSymbol.DayOpen  := StrToFloat( aObj.GetValue('o').Value );
//      aSymbol.DayHigh  := StrToFloat( aObj.GetValue('h').Value );
//      aSymbol.DayLow   := StrToFloat( aObj.GetValue('l').Value );
//      aSymbol.DayVolume:= StrToFloat( aObj.GetValue('v').Value );
//      aSymbol.DayAmount:= StrToFloat( aObj.GetValue('q').Value ) * App.Engine.ApiManager.ExRate.Value / 100000000;
//
//      if App.AppStatus > asLoad then
//      begin
//        aQuote:= App.Engine.QuoteBroker.Brokers[FParent.ExchangeKind].Find(sCode)    ;
//        if aQuote = nil then
//        begin
//          aSymbol.Last := StrToFloat( aObj.GetValue('c').Value );
//
//          App.Engine.SymbolCore.CalcKimp( aSymbol );
//          App.Engine.SymbolCore.CalcMainKimp( aSymbol );
//          App.Engine.SymbolCore.CalcMainWDC(aSymbol);
//        end;
//      end;
//    end;
//  end;

end;

procedure TBinanceParse.ParseCandleData(aKind: TMajorSymbolKind; sUnit, aData: string);
var
  aArr, aSubArr : TJsonArray;
  aVal : TJsonValue;
  I, j, iCnt: Integer;
  d, dd, h, m, mt : word;
  sTmp, sCode : string;
  aNum : TJSONNumber;
  dTime, dtime2: TDateTime;
  aSymbol : TSymbol;
  accAmt, aAmt, aPrice, aClose : double;
  aWcd : TWCDData;
begin
  if aData = '' then
  begin
    App.Log(llError, '%s ParseCandleData data is empty',
       [ TExchangeKindDesc[FParent.ExchangeKind] ] ) ;
    Exit;
  end;

  aArr := nil;
  aArr := TJsonObject.ParseJSONValue( aData) as TJsonArray;
  if aArr = nil then Exit;
  try
    try
      j := 0;
      for I :=aArr.Size-1 downto 0 do
      begin
        if ( sUnit = '1d' ) and ( j >= 25 ) then break;

        aSubArr := aArr.Get(i) as TJsonArray;
        aNum    := aSubArr.Get(0) as TJSONNumber;
        dTime   := UnixTimeToDateTime( aNum.GetValue<int64> );
        sTmp    := aSubArr.Get(4).Value;      // 종가
        aPrice  := StrToFloat( sTmp );
        aAmt    := StrToFloat( aSubArr.Get(7).Value );      // quote volume

        inc(j);

        if (sUnit = '30m') and ( not IsToday( dTime )) then
          break;

        aWcd := nil;

        mt:= MonthOf(dTime);
        d := Dayof(  dTime );
        h := Hourof( dTime );
        m := MinuteOf(dTime);

        if (sUnit = '1d') and ( not IsToday( dTime )) then begin
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
          aWcd.Amount[aKind, FParent.ExchangeKind] := aAmt;
//          App.DebugLog('%s, %s %s %s %d -> %f, %f', [ sUnit, TExchangeKindDesc[ FParent.ExchangeKind], TMajorSymbolCode[aKind],
//            sTmp, iCnt ,  aPrice, aAmt  ]);
        end;

      end;
    except
    end;
  finally
    if aArr <> nil then aArr.Free;
  end;
end;

procedure TBinanceParse.ParseFutMarketDepth(aJson: TJsonObject);
var
  sCode : string;
  aQuote : TQuote;
  i : integer;
  aVal : TJsonValue;
  aArr, aArr2 : TJsonArray;
  dtTime: TDateTime;

begin
  sCode := aJson.GetValue('s').Value;
  aQuote  := App.Engine.QuoteBroker.Brokers[FParent.ExchangeKind].Find(sCode+Fut_Suf)    ;
  if (aQuote = nil) or (aQuote.Symbol = nil ) then Exit;  

  dtTime  := UnixTimeToDateTime( aJson.GetValue<int64>('E') );

  with aQuote do
  begin
    aArr := aJson.GetValue('b') as TJsonArray;
    for I := aArr.Size-1 downto 0 do
    begin
    	if i > Symbol.Asks.Size then continue;      
      aArr2  := aArr.Get(i) as TJsonArray;  

			Symbol.Bids[i].Price := StrToFloat( aArr2.Get(0).Value );
      Symbol.Bids[i].Volume := StrToFloat( aArr2.Get(1).Value );
    end;

    aArr := aJson.GetValue('a') as TJsonArray;
    for I := 0 to aArr.Size-1 do
    begin
    	if i > Symbol.Asks.Size then continue;      
      aArr2  := aArr.Get(i) as TJsonArray;  

			Symbol.Asks[i].Price := StrToFloat( aArr2.Get(0).Value );
      Symbol.Asks[i].Volume := StrToFloat( aArr2.Get(1).Value );
    end;    

    Symbol.TrnsTime  := aJson.GetValue<int64>('T') ;
    LastEvent := qtMarketDepth;
    Update( dtTime );

//    if Symbol.Spec.BaseCode = 'XRP' then
//    begin
//			App.DebugLog( '%s(%f) %s(%f) | %s(%f) %s(%f)', [  
//      	Symbol.PriceToStr( Symbol.Asks[1].Price ), Symbol.Asks[1].Volume ,
//        Symbol.PriceToStr( Symbol.Asks[0].Price ), Symbol.Asks[0].Volume ,
//        Symbol.PriceToStr( Symbol.Bids[0].Price ), Symbol.Bids[1].Volume ,
//        Symbol.PriceToStr( Symbol.Bids[1].Price ), Symbol.Bids[1].Volume 
//        ]  )    ;
//    end;
    
  end;
end;

{
  TOrderState = (osReady,
                   // with server
                 osSent, osSrvAcpt, osSrvRjt,
                   // with exchange
                 osActive, osRejected, osFilled, osCanceled,
                 osConfirmed, osFailed);
}

function TBinanceParse.GetOrderStatus( const s : string ) : TOrderState;
begin
  if s = 'NEW' then
      Result :=  osActive
  else if (s = 'PARTIALLY_FILLED') or ( s = 'FILLED') then
      Result := osFilled
  else if (s = 'CANCELED') or ( s = 'EXPIRED') then
      Result := osCanceled
  else if (s = 'REJECTED') then
      Result := osRejected
end;

procedure TBinanceParse.ParseFutOrderUpdate(aJson: TJsonObject);
var
  aObj : TJsonObject;
  aVal : TJsonValue;
  sID, sTmp, sCode, sStatus : string;

  aAcnt   : TAccount;
  aSymbol : TSymbol;
  aOrder  : TOrder;

  iTime   : int64;
  dtTime  : TDateTime;
  iSide   : integer;
  dOrderQty , dPrice : double;
  otType   : TOrderType;
  pcValue  : TPriceControl;
  aStatus  : TOrderState;
begin

  App.DebugLog( 'binan order : %s ' , [ aJson.ToJSON ]);

  aVal  := aJson.GetValue('o');
  if aVal = nil then Exit;

  sCode := aVal.FindValue('s').Value;
  sID   := aVal.FindValue('c').Value;

  //App.Engine.TradeCore.
  aAcnt := App.Engine.TradeCore.FindAccount(ekBinance, amFuture);
  if aACnt = nil then Exit;
  aSymbol := App.Engine.SymbolCore.FindSymbol( ekBinance, sCode+Fut_Suf);
  if aSymbol = nil then Exit;

  aOrder  := App.Engine.TradeCore.FindOrder( ekBinance, aAcnt, aSymbol, sID);
  if aOrder = nil then
  begin

    sTmp  := aVal.FindValue('S').Value;
    iSide := 1;
    if sTmp = 'SELL' then
      iSide := -1
    else
      iSide  := 1;

    sTmp  := aVal.FindValue('o').Value;
    if sTmp = 'MARKET' then
      pcValue := pcMarket
    else pcValue := pcLimit;

    dOrderQty := aVal.GetValue<double>('q', 0 );
    dPrice    := aVal.GetValue<double>('p', 0 );

    aOrder := App.Engine.TradeCore.Orders[ekBinance].NewOrder( aAcnt, aSymbol,
      iSide, dOrderQty, pcValue, dPrice, tmGTC );

    aOrder.ReduceOnly := aVal.GetValue<boolean>('R', false);
    aOrder.OrderNo    := sID;
    if aOrder = nil then
    begin
      App.Log(llError, 'biance future not found order : %s, %s, %s, %s, %s ',
        [ sID, sCode, ifThenStr( iSide = 1, '매수','매도')
          , aSymbol.PriceToStr(dPrice), aSymbol.QtyToStr(dOrderQty)  ]  );
      Exit;
    end ;
  end;

  iTime := aJson.GetValue<int64>('T');
  dtTime:= UnixTimeToDateTime( iTime );

  sTmp  :=  aVal.FindValue('X').Value;
  aStatus := GetOrderStatus( sTmp );

  App.DebugLog('Binance Fut : %s, %s, %s, %s, %s, %s, %s ', [  sTmp, sID, sCode
    , ifThenStr( iSide = 1, '매수','매도')
    , aSymbol.PriceToStr(dPrice), aSymbol.QtyToStr(dOrderQty)
    , ifThenStr( aOrder.ReduceOnly, 'Reduce', 'Limit' )
    ]  );

  case aStatus of
    osActive    : App.Engine.TradeBroker.Accept( aOrder, dtTime, true ) ;
    osFilled    :
      begin
        var aFill : TFill;
        var dFillVol, dFillPrice, dAvgPrice : double;
        var sTrdID : string;

        dAvgPrice := aVal.GetValue<double>('ap', 0 );
        dFillVol  := aVal.GetValue<double>('l', 0);
        dFillPrice:= aVal.GetValue<double>('L', 0);
        sTrdID    := aVal.GetValue<string>('t');

        App.DebugLog('Binance Fut Fill : %s, %s, %s, %s, %s, %s ', [
          sTmp, sID,
          aSymbol.PriceToStr(dFillPrice), aSymbol.QtyToStr(dFillVol),
          aSymbol.PriceToStr(dAvgPrice),  sTrdID
        ]);

        aFill := App.Engine.TradeCore.Fills[ekBinance].New( sTrdID, dtTime, now,
          sID, aAcnt, aSymbol, dFillVol , iSide, dFillPrice);

        App.Engine.TradeBroker.Fill( aOrder, aFill );
      end;
    osCanceled  : App.Engine.TradeBroker.Cancel( aOrder, 0 ) ;
    osRejected  : App.Engine.TradeBroker.Accept( aOrder, dtTime, false );
  end;


end;

function TBinanceParse.ParsePrepareFuttMaster(aData: string): boolean;
var
  aObj  : TJsonObject;
  aArr : TJsonArray;

  I : Integer;
  sTmp : string;
  bNew : boolean;
  dSize, dSize2 : double;
begin
  if aData = '' then
  begin
    App.Log(llError, 'Binance ParsePrepareFuttMaster data is empty') ;
    Exit (false);
  end;

  aArr := (TJsonObject.ParseJSONValue( aData ) as TJsonObject).Get('symbols').JsonValue as TJsonArray;

  for I := 0 to aArr.Size-1 do
  begin
    aObj  := aArr.Get(i) as TJsonObject;
    sTmp  := aObj.GetValue('quoteAsset').Value;
    if sTmp = 'USDT' then
      FParent.Exchanges[ mtFutures ].Codes.Add( aObj.GetValue('baseAsset').Value );
  end;

  REsult := true;

end;


procedure TBinanceParse.ParseFuttMaster(aData: string);
var
  aVal : TJsonValue;

  aObj, aFil  : TJsonObject;
  aArr, aArr2  : TJsonArray;
  aSymbol : TSymbol;
  I, j, iPre, iQtyPre: Integer;
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
    if sTmp <> 'USDT' then  begin
      //sCode   := aObj.GetValue('symbol').Value;
      //App.DebugLog( 'binance  fut  not found  : %d , %s ', [i, sCode] );
      Continue;
    end;

    sCode   := aObj.GetValue('symbol').Value;

    sTmp    := sCode + Fut_Suf;  // add perpectual future Suffix;
    aSymbol := App.Engine.SymbolCore.FindSymbol(ekBinance, sTmp);
    if aSymbol = nil then
    begin
      bNew := true;
      aSymbol := App.Engine.SymbolCore.RegisterSymbol(ekBinance, mtFutures, sCode );
      if aSymbol = nil then
      begin
        //App.DebugLog( 'binance  fut  not RegisterSymbol  : %d , %s ', [i, sCode] );
        continue;
      end;
    end else
      bNew := false;

    with aSymbol do
    begin
      OrgCode     := lowercase( sCode );
      Spec.BaseCode    := aObj.GetValue('baseAsset').Value;;
      Spec.QuoteCode   := aObj.GetValue('quoteAsset').Value;  // USTD
      Spec.SettleCode  := aObj.GetValue('symbol').Value;;
    end;

    sTmp := aObj.GetValue('status').Value;
    if sTmp = 'TRADING' then aSymbol.TradeAble := true  else aSymbol.TradeAble := false;

    iPRe  	:=  aObj.GetValue<integer>('pricePrecision');
    iQtyPre :=  aObj.GetValue<integer>('quantityPrecision');

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

    aSymbol.Spec.SetSpec( iPre, dSize, dSize2, iQtyPre );

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

  for I := 0 to aArr.Count-1 do
  begin
    aVal := aArr.Items[i];
    sTmp := aVal.GetValue<string>('symbol');
    aSymbol := App.Engine.SymbolCore.FindSymbol(ekBinance, sTmp+Fut_Suf);
    if aSymbol <> nil then
    begin

      aSymbol.DayOpen := StrToFloatDef( aVal.GetValue<string>( 'openPrice' ), 0.0 );
      aSymbol.DayHigh := StrToFloatDef( aVal.GetValue<string>( 'highPrice' ), 0.0 );
      aSymbol.DayLow  := StrToFloatDef( aVal.GetValue<string>( 'lowPrice' ), 0.0 );
      aSymbol.Last    := StrToFloatDef( aVal.GetValue<string>( 'lastPrice' ), 0.0 );
      aSymbol.DayAmount   := StrToFloatDef( aVal.GetValue<string>( 'quoteVolume' ), 0.0 )
        * App.Engine.ApiManager.ExRate.Value / 100000000;

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

procedure TBinanceParse.ParseListenKey( aMarket : TMarketType; aData: string);
var
  aObj : TJsonObject;
begin
  if aData = '' then
  begin
    App.Log(llError, 'Binance ParseFuttTicker data is empty') ;
    Exit;
  end;

  aObj := TJsonObject.ParseJSONValue( aData) as TJsonObject;

  try
    aObj.GetValue('listenKey').Value;
  finally
    if aObj <> nil then aObj.Free;
  end;

end;

procedure TBinanceParse.ParseMarginPair(aData: string);
var
  aArr : TJsonArray;
  aVal : TJsonValue;
  I, j: Integer;
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
  j:= 0;

  for I := 0 to aArr.Count-1 do
  begin
    aVal  := aArr.Items[i] ;
    sTmp  := aVal.GetValue<string>('quote');
    if sTmp <> 'USDT' then continue;
    bAble := aVal.GetValue<boolean>('isMarginTrade');
    if not bAble then Continue;
    sTmp := aVal.GetValue<string>('symbol');

    aSymbol := App.Engine.SymbolCore.FindSymbol(ekBinance, sTmp);
    if aSymbol <> nil then
    begin
      aSymbol.IsMargin := true;
      inc(j);
//      App.DebugLog('%s Enabled Margin Trade', [ aSymbol.Code] );
    end;

  end;

  App.DebugLog('binance margin count : %d', [j] );

end;






procedure TBinanceParse.ParseDNWState(aData: string);
var
  aObj : TJsonObject;
  aPair: TJsonPair;
  I: Integer;
  sTmp : string;
  aSymbol : TSymbol;

begin
  if aData = '' then
  begin
    App.Log(llError, 'Binance ParseDNWState data is empty') ;
    Exit;
  end;

  try

    aObj := TJsonObject.ParseJSONValue( aData) as TJsonObject;
    try
      for I := 0 to aObj.Size-1 do
      begin
        aPair := aObj.Get(i);

        sTmp  := aPair.JsonString.Value;
        if FParent.Codes.IndexOf(sTmp) < 0 then Continue;

        aSymbol := App.Engine.SymbolCore.FindSymbol( FParent.ExchangeKind, sTmp+'USDT' );
        if aSymbol <> nil then
        begin

          var iRes : integer;
          iRes := aSymbol.CheckDnwState(  aPair.JsonValue.GetValue<boolean>('depositStatus')
                                        , aPair.JsonValue.GetValue<boolean>('withdrawStatus') ) ;
          if iRes > 0 then
            App.Engine.SymbolBroker.DnwEvent( aSymbol, iRes);
        end;

      end;
    finally
      aObj.Free;
    end;
  except on e : exception do
    App.Log(llError, 'ParseDNWState parse error : %s, %s ' ,[ e.Message, aData ] );
  end;
end;

procedure TBinanceParse.ParseSpotBalance(aData: string);
var
  aObj, aObj2 : TJsonObject;
  aArr, aArr2 : TJsonArray;
  aVal : TJsonValue;
  iRes : integer;
  sTmp : string;
  dTmp, dTmp2, dVol : double;
  i, j : integer;
  //
  aAcnt : TAccount;
  aSymbol : TSymbol;
  aPos    : TPosition;
//  aBal    : TBalance;
begin
  if aData = '' then
  begin
    App.Log(llError, 'Binance ParseSpotBalance data is empty') ;
    Exit;
  end;

  aObj := TJsonObject.ParseJSONValue( aData) as TJsonObject;
  try
    iRes := aObj.GetValue<integer>('code');
    if iRes <> SUCC_CODE then
    begin
      sTmp  := aObj.GetValue('msg').Value;
      App.Log(llError, '%s ParseBalance Result %d %s',
        [ TExShortDesc[FParent.ExchangeKind], iRes, sTmp ] );
      Exit;
    end;

    aArr  := aObj.GetValue('snapshotVos') as TJsonArray;

    aAcnt := App.Engine.TradeCore.FindAccount( Fparent.ExchangeKind, amSpot );
    if aAcnt = nil then Exit;

    for I := aArr.Size-1 downto 0 do
    begin
      aObj2 := aArr.Get(i).FindValue('data') as TJsonObject;
      aArr2 := aObj2.GetValue('balances') as TJsonArray;

      for j := 0 to aArr2.Size-1 do
      begin
        aVal  := aArr2.Get(j);

        sTmp  := aVal.GetValue<string>('asset', '');
        if sTmp = '' then continue;

        if sTmp = 'USDT' then
        begin
          aAcnt.AvailableAmt[scUSDT] := aVal.GetValue<double>('free', 0 );
          dTmp  := aVal.GetValue<double>('locked', 0 );
          aAcnt.Balance[scUSDT]  :=  aAcnt.AvailableAmt[scUSDT] + dTmp;
        end else
        begin
          dTmp  := aVal.GetValue<double>('free', 0 );
          dTmp2 := aVal.GetValue<double>('locked', 0 );
          dVol  := dTmp + dTmp2 ;

          if CheckZero( dVol ) then
            continue;

          aSymbol := App.Engine.SymbolCore.FindSymbol( ekBinance, sTmp+'USDT');
          if aSymbol = nil then continue;

          aPos  := App.Engine.TradeCore.Positions[ekBinance].FindOrNew( aAcnt, aSymbol);
          aPos.Volume := dTmp + dTmp2;
//          aBal  := aAcnt.New( sTmp );
//          if aBal <> nil then begin
//            aBal.SetItem(dTmp + dTmp2, dTmp2, aSymbol);
//            App.DebugLog('balance : %s', [ aBal.Represent ] );
//          end;
        end;

      end;

      break;
    end;

  finally
    if aObj <> nil then aObj.Free;
  end;
end;


procedure TBinanceParse.ParseFutBalance(aData: string);
var
  aArr, aArr2  : TJsonArray;
  aObj, aObj2  : TJsonObject;
  i     : integer;
  aAcnt : TAccount;
  sCode : string;
  dTmp, dTmp2, dVol : double;

  aSymbol : TSymbol;
  aPos    : TPosition;
begin
  if aData = '' then
  begin
    App.Log(llError, 'Binance ParseFutBalance data is empty') ;
    Exit;
  end;

  aObj := TJsonObject.ParseJSONValue( aData) as TJsonObject;

  try
    if aObj = nil  then Exit;

    aAcnt := App.Engine.TradeCore.FindAccount( ekBinance, amFuture);
    if aAcnt = nil then Exit;

    aArr := aObj.GetValue('assets') as TJsonArray;

    for I := 0 to aArr.Size-1 do
    begin
      aObj2  := aArr.Get(i) as TJsonObject;

      sCode := aObj2.GetValue('asset').Value;
      if sCode = '' then continue;

      if sCode = 'USDT' then
      begin
        aAcnt.Balance[ scUSDT ] := aObj2.GetValue<double>('walletBalance', 0);
        aAcnt.AvailableAmt[ scUSDT ] := aObj2.GetValue<double>('availableBalance', 0);
        break;
      end;
    end;

    aArr := aObj.GetValue('positions') as TJsonArray;
    for I := 0 to aArr.Size-1 do
    begin
      aObj2 := aArr.Get(i) as TJsonObject;

      sCode := aObj2.GetValue('symbol').Value;
      aSymbol := App.Engine.SymbolCore.FindSymbol(ekBinance, sCode+Fut_Suf)  ;
      if aSymbol = nil then continue;

      dTmp := aObj2.GetValue<double>('positionAmt', 0);
      if CheckZero( dTmp ) then continue;

//      aPos  := App.Engine.TradeCore.Positions[ekBinance].FindOrNew( aAcnt, aSymbol);
//      if aPos <> nil then
//      begin
//        aPos.Volume := dTmp;
//      end;

    end;


  finally
    if aObj <> nil then aObj.Free;
  end;
end;

procedure TBinanceParse.ParseFutPosition(aData: string);
var
  aArr  : TJsonArray;
  aObj  : TJsonObject;
  i     : integer;
  aAcnt : TAccount;
  sCode : string;
  dTmp, dTmp2, dVol : double;

  aSymbol : TSymbol;
//  aBal    : TBalance;
begin
  if aData = '' then
  begin
    App.Log(llError, 'Binance ParseFutPosition data is empty') ;
    Exit;
  end;

  aArr := TJsonObject.ParseJSONValue( aData) as TJsonArray;

  try
    if aArr = nil  then Exit;

    aAcnt := App.Engine.TradeCore.FindAccount( ekBinance, amFuture);
    if aAcnt = nil then Exit;

    for I := 0 to aArr.Size-1 do
    begin
      aObj  := aArr.Get(i) as TJsonObject;

      sCode := aObj.GetValue('symbol').Value;
      if sCode = '' then continue;

      if sCode = 'USDT' then
      begin

        aAcnt.Balance[ scUSDT ] := aObj.GetValue<double>('balance', 0);
        aAcnt.AvailableAmt[ scUSDT ] := aObj.GetValue<double>('availableBalance', 0);

        break;
      end else
      begin
//        aSymbol := App.Engine.SymbolCore.FindSymbol( ekBinance, sCode+'USDT'+Fut_Suf);
//        if aSymbol = nil then continue;
//        dTmp := aObj.GetValue<double>('balance', 0);
//        if not CheckZero( dTmp ) then
//        begin
//          aBal  := aAcnt.New(sCode);
//          if aBal <> nil then begin
//            dTmp2 := aObj.GetValue<double>('availableBalance', 0);
//            aBal.SetItem(dTmp, dTmp - dTmp2, aSymbol );
//            App.DebugLog('balance : %s', [ aBal.Represent ] );
//          end;
//        end;
      end;

    end;

  finally
    if aArr <> nil then aArr.Free;
  end;

end;


procedure TBinanceParse.ParseSpotTrade(aJson: TJsonObject);
var
  sCode : string;
  aQuote : TQuote;
  dtTime : TDateTime;
  aSale  : TTimeNSale;
  iTime  : int64;
  bSide  : boolean;
begin
  sCode := aJson.GetValue('s').Value;

  aQuote  := App.Engine.QuoteBroker.Brokers[FParent.ExchangeKind].Find(sCode)    ;
  if (aQuote = nil) or (aQuote.Symbol = nil ) then Exit;

  with aQuote do
  begin

    iTime := aJson.GetValue<int64>('T');
    dtTime:= UnixTimeToDateTime( iTime );

//    App.DebugLog('time compare %s, %d', [ FormatDateTime('hh:nn:ss.zzz', dtTime), iTime ]   );

    aSale := Symbol.Sales.New;
    aSale.LocalTime := now;
    aSale.Time      := dtTime;
    aSale.Price     := StrToFloat( aJson.GetValue('p').Value );
    aSale.Volume    := StrToFloat( aJson.GetValue('q').Value );

    bSide := aJson.GetValue<boolean>('m');
    	// true : 매도  ,  false : 매수
    if bSide then aSale.Side := -1
    else aSale.Side := 1;

    Symbol.Last   := aSale.Price;
    Symbol.Volume := aSale.Volume;
    Symbol.Side   := aSale.Side;

    LastEvent := qtTimeNSale;

    Update( dtTime );
  end;
end;




procedure TBinanceParse.ParseSpotBookTicker(aJson: TJsonObject);
var
  sCode : string;
  aQuote : TQuote;
//  dtTime : TDateTime;
begin
  sCode := aJson.GetValue('s').Value;

  aQuote  := App.Engine.QuoteBroker.Brokers[FParent.ExchangeKind].Find(sCode)    ;
  if (aQuote = nil) or (aQuote.Symbol = nil ) then Exit;

  with aQuote do
  begin

    Symbol.Asks[0].Price := StrToFloat( aJson.GetValue('a').Value );
    Symbol.Asks[0].Volume := StrToFloat( aJson.GetValue('A').Value );
    Symbol.Bids[0].Price := StrToFloat( aJson.GetValue('b').Value );
    Symbol.Bids[0].Volume := StrToFloat( aJson.GetValue('B').Value );

    LastEvent := qtMarketDepth;

    Update( now );
  end;

end;

procedure TBinanceParse.ParseSpotMiniTickers(aJson: TJsonObject);
var
  sCode : string;
  aQuote : TQuote;
  dtTime : TDateTime;
begin
  sCode := aJson.GetValue('s').Value;

  aQuote  := App.Engine.QuoteBroker.Brokers[FParent.ExchangeKind].Find(sCode)    ;
  if (aQuote = nil) or (aQuote.Symbol = nil ) then Exit;

  try

    with aQuote do
    begin
      Symbol.DayOpen  := StrToFloat( aJson.GetValue('o').Value );
      Symbol.DayHigh  := StrToFloat( aJson.GetValue('h').Value );
      Symbol.DayLow   := StrToFloat( aJson.GetValue('l').Value );
      Symbol.DayVolume:= StrToFloat( aJson.GetValue('v').Value );
      Symbol.DayAmount:= StrToFloat( aJson.GetValue('q').Value ) * App.Engine.ApiManager.ExRate.Value / 100000000;
    end;


  except
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
      aSymbol.DayAmount   := StrToFloatDef( aVal.GetValue<string>( 'quoteVolume' ), 0.0 )* App.Engine.ApiManager.ExRate.Value / 100000000;
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


procedure TBinanceParse.ParseSocketData(aMarket : TMarketType; aData: string);
var
  aObj : TJsonObject;
  aArr : TJsonArray;
  aVal  : TJsonValue;
  aPair : TJsonPair;
  sTmp : string;

begin
  if aData = '' then
  begin
    App.Log(llError, 'Binance %s ParseSocketData data is empty', [ TMarketTypeDesc[aMarket] ] ) ;
    Exit;
  end;


//  App.Log(llInfo, 'Test', aData);
  try

    aVal := TJsonObject.ParseJSONValue( aData);// as TJsonObject;

    try

      if aVal is TJsonArray then begin
          aArr := aVal as TJsonArray;
          case aMarket of
            mtSpot:    ;
            mtFutures : ParseAllMiniTicker( aArr );
          end;
          Exit;
      end else
      begin

          aObj := aVal as TJsonObject;

          aPair := aObj.Get('e');

          if aPair <> nil then
          begin
            sTmp  := aPair.JsonValue.Value;

            case aMarket of
              mtSpot:
                if sTmp = 'trade' then
                  ParseSpotTrade( aObj )
                else if sTmp = '24hrMiniTicker' then
                  ParseSpotMiniTickers( aObj )
                else if sTmp = 'bookTicker' then
                  ParseSpotBookTicker( aObj )
                else exit;
              mtFutures:
                if sTmp = 'aggTrade' then
                  ParseFutaggTrade( aObj )
                else if sTmp = '24hrMiniTicker' then
                  ParseFutBookTicker( aObj )
                else if sTmp = 'depthUpdate' then        // Partial Book Depth Streams
                  ParseFutMarketDepth( aObj )
                else if sTmp = 'ACCOUNT_UPDATE' then
                  ParseFutAccountUpdate( aObj )
                else if sTmp = 'ORDER_TRADE_UPDATE' then
                  ParseFutOrderUpdate( aObj )
                else exit;
            end;
          end else
          begin
            if aMarket = mtSpot then
            begin
              // e 는 없구 s 는 있는건 best ask bid 뿐..
              aPair := aObj.Get('s');
              if aPair <> nil  then
              begin
      //          sTmp  := aPair.JsonValue.Value;
                ParseSpotBookTicker( aObj )
              end
                else App.DebugLog(' %s, %s oops !! ....... %s ', [ TExchangeKindDesc[FParent.ExchangeKind],  TMarketTypeDesc[aMarket], aData ]);
            end;
          end
      end;

    finally
      aVal.Free;
    end;
  except

  end;

end;

end.
