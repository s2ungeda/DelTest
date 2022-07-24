unit UBithParse;

interface

uses
  System.Classes, System.SysUtils, System.DateUtils
  , System.JSON , USymbols, UExchangeManager
  , UAccounts , UPositions, UOrders
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

    procedure ParseBalance( aData : string );
    procedure ParseOrders( aData : string );

    // private
    procedure ParseAccounts( aData : string );

    // 주문 from shared
    procedure ParseSpotNewOrder( aData, aRef : string );    
    procedure ParseSpotCnlOrder( aData, aRef : string );    
    procedure ParseSpotOrderList( aData, aRef : string );    
    procedure ParseSpotOrderDetail( aData, aRef : string );    
    procedure ParseSpotBalance( aData, aRef : string );

    property Parent : TExchangeManager read FParent;
    property OnSendDone : TSendDoneNotify read FOnSendDone write FOnSendDone;
  end;

var
  gBithReceiver : TBithParse;
implementation

uses
  GApp, GLibs, Math , UTypes, UConsts
  , UApiConsts
  , UQuoteBroker
  , USymbolUtils
  , UFills
  , UDecimalHelper
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

//        if aArr.Count > 0 then begin
//          aVal  := aArr.Get(0);
//          aSymbol.Bids[0].Price   := StrToFloatDef( aVal.GetValue<string>( 'price' ), 0.0 );
//          aSymbol.Bids[0].Volume  := StrToFloatDef( aVal.GetValue<string>( 'quantity' ), 0.0 );
//        end;

        if aArr <> nil then        
          for j := 0 to aArr.Size-1 do
          begin
          	if j >= aSymbol.Bids.Size then Exit;            
            aVal  := aArr.Get(j);
            aSymbol.Bids[j].Price   := StrToFloatDef( aVal.GetValue<string>( 'price' ), 0.0 );
            aSymbol.Bids[j].Volume  := StrToFloatDef( aVal.GetValue<string>( 'quantity' ), 0.0 );
          end;

        aArr  := aSub.Get('asks').JsonValue as TJsonArray;

//        if aArr.Count > 0 then
//        begin
//          aVal  := aArr.Get(0);
//          aSymbol.Asks[0].Price   := StrToFloatDef( aVal.GetValue<string>( 'price' ), 0.0 );
//          aSymbol.Asks[0].Volume  := StrToFloatDef( aVal.GetValue<string>( 'quantity' ), 0.0 );
//        end;

        if aArr <> nil then        
          for j := 0 to aArr.Size-1 do
          begin
          	if j >= aSymbol.Bids.Size then Exit;            
            aVal  := aArr.Get(j);
            aSymbol.Asks[j].Price   := StrToFloatDef( aVal.GetValue<string>( 'price' ), 0.0 );
            aSymbol.Asks[j].Volume  := StrToFloatDef( aVal.GetValue<string>( 'quantity' ), 0.0 );
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

procedure TBithParse.ParseAccounts(aData: string);         
begin

end;

procedure TBithParse.ParseBalance(aData: string);
var
  aObj, aBals : TJsonObject;
  iResult : integer;
  aAcnt	: TAccount;
  aPos  : TPosition;
  aSymbol: TSymbol;
  sCode : string;
  I: Integer;
  
begin
  if aData = '' then
  begin
    App.Log(llError, '%s ParseBalance data is empty', [ TExchangeKindDesc[FParent.ExchangeKind] ] ) ;
    Exit;
  end;

  aObj := nil;
  aObj := TJsonObject.ParseJSONValue( aData) as TJsonObject;  

  try
    iResult := StrToInt( aObj.GetValue('status').Value );
    if iResult <> 0 then begin
      App.Log(llError, 'Request Result %d (ParseBalance)', [ iResult ] );
      Exit;
    end;  

    aBals	:= aObj.GetValue('data') as TJsonObject;    
    if aBals = nil then Exit;    

    aAcnt := App.Engine.TradeCore.FindAccount(FParent.ExchangeKind);
    if aAcnt = nil then Exit;
  //   TSettleCurType = ( scNone, scKRW, scUSDT, scBTC );
  	aAcnt.TradeAmt[scKRW]			:= aBals.GetValue<double>('total_krw', 0.0);
    aAcnt.AvailableAmt[scKRW]	:= aBals.GetValue<double>('available_krw', 0.0);

//  	aAcnt.TradeAmt[scBTC]			:= aObj.GetValue<double>('total_btc', 0.0);
//    aAcnt.AvailableAmt[scBTC]	:= aObj.GetValue<double>('available_btc', 0.0);    

    for I := 0 to FParent.Codes.Count-1 do
		begin
    	sCode := FParent.Codes[i] ;
      aSymbol	:= App.Engine.SymbolCore.FindSymbol( ekBithumb, sCode );
      if aSymbol = nil then continue;            
			sCode	:= LowerCase( aSymbol.Code );
      var dVal, dPrice : double;  dVal := 0;  dPrice := 0;
      
      dVal := aBals.GetValue<double>( 'total_'+sCode , 0 );
      if isZero( dVal ) then continue;    
                       
      dPrice:= aBals.GetValue<double>( 'xcoin_last_'+sCode , 0 );      
      aPos	:= App.Engine.TradeCore.Positions[ekBithumb].New( aAcnt, aSymbol, dVal, dPrice );    
      aPos.CalcOpenPL;
    end;
		  
  finally
    if aObj <> nil  then aObj.Free;    
  end;
end;

procedure TBithParse.ParseOrders(aData: string);
var
  aObj, aObj2 : TJsonObject;
  aArr : TJsonArray;
  aVal : TJsonValue;
  iResult : integer;
  aAcnt	: TAccount;
  aOrder : TOrder;
  aSymbol: TSymbol;
  sCode, sOrderId : string;
  I, iSide : Integer;
  dPrice , dVolume : double;
  
begin

  if aData = '' then
  begin
    App.Log(llError, '%s ParseOrders data is empty', [ TExchangeKindDesc[FParent.ExchangeKind] ] ) ;
    Exit;
  end;

  aObj := nil;
  aObj := TJsonObject.ParseJSONValue( aData) as TJsonObject;  

  try
    iResult := StrToInt( aObj.GetValue('status').Value );
    if iResult <> 0 then begin
      if iResult <> 5600 then
        App.Log(llError, 'Request Result %d (ParseOrders) : %s', [ iResult, aData ] );
      Exit;
    end;

    aArr	:= aObj.GetValue('data') as TJsonArray;    
    if aArr = nil then Exit;    

    aAcnt := App.Engine.TradeCore.FindAccount(FParent.ExchangeKind);
    if aAcnt = nil then Exit;

    for I := 0 to aArr.Size-1 do
    begin
    	aVal	:= aArr.Get(i);
      sCode	:= aVal.GetValue<string>('order_currency', '');
      if sCode = '' then continue;

    	aSymbol := App.Engine.SymbolCore.FindSymbol(ekBithumb, sCode);
      if aSymbol = nil then continue;

      sOrderId:= aVal.GetValue<string>('order_id');
      aOrder	:= App.Engine.TradeCore.Orders[ekBithumb].Find( aAcnt, aSymbol, sOrderId);

      if aOrder = nil then
      begin                                                    
      	iSide	:= ifThen( aVal.GetValue<string>('type') = 'bid', 1, -1 );
        dVolume	:= aVal.GetValue<double>( 'units', 0 );
        dPrice	:= aVal.GetValue<double>( 'price', 0 );
      
        aOrder	:= App.Engine.TradeCore.Orders[ekBithumb].NewOrder( aAcnt
        	, aSymbol, iSide, dVolume, pcLimit, dPrice, tmGTC ); 
      end;                                                         
    end;                                                       
		  
  finally
    if aObj <> nil  then aObj.Free;    
  end;

end;

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

procedure TBithParse.ParseSpotBalance(aData, aRef: string);
var
	aOrder : TOrder;
  aObj, aBal  : TJsonObject;
  sCode, sTmp, sStatus  : string;
  aSymbol : TSymbol;
  aAcnt		: TAccount;
  aPos		: TPosition;
  aExKind : TExchangeKind;
  i : Integer;
  biTmp : TBigInt;
begin
  if aData = '' then
  begin
    App.Log(llError, '%s %s SpotBalance data is empty',
       [ TExchangeKindDesc[FParent.ExchangeKind],  TMarketTypeDesc[mtSpot] ] ) ;
    Exit;
  end;

  aExKind	:= ekBithumb;
  aSymbol	:= App.Engine.SymbolCore.FindSymbol( aExKind, aRef );
  aAcnt		:= App.Engine.TradeCore.FindAccount( aExKind );
  if (aSymbol = nil) or ( aAcnt = nil) then Exit;  

	aObj := TJsonObject.ParseJSONValue( aData) as TJsonObject;
  try
  	sStatus	:= aObj.GetValue('status').Value;
    if sStatus <> '0000' then
    begin
      App.Log(llError, '%s ParseSpotBalance Error %s, %s',[
        TExchangeKindDesc[FParent.ExchangeKind] , aData, aRef
        ] );
      Exit;
    end;

    aBal := aObj.GetValue('data') as TJsonObject;
    // 원화
  	aAcnt.TradeAmt[scKRW]			:= aBal.GetValue<double>('total_krw', 0.0);
    aAcnt.AvailableAmt[scKRW]	:= aBal.GetValue<double>('available_krw', 0.0);
    // 선택 코인.
    sCode	:= LowerCase( aSymbol.Code );
    var dVal, dPrice : double;  dVal := 0;  dPrice := 0;
      
    dVal 	:= aBal.GetValue<double>( 'total_'+sCode , 0 );                       
    dPrice:= aBal.GetValue<double>( 'xcoin_last_'+sCode , 0 );   
    aPos	:= App.Engine.TradeCore.FindPosition( aACnt, aSymbol);
    if aPos = nil then begin
	    aPos	:= App.Engine.TradeCore.Positions[ekBithumb].New( aAcnt, aSymbol, dVal, dPrice );    
      App.Engine.TradeBroker.PositionEvent( aPos, POSITION_NEW);
    end;
    aPos.CalcOpenPL;    
    App.Engine.TradeBroker.PositionEvent( aPos, POSITION_UPDATE);   
    
  finally
		if aObj <> nil then aObj.free;         
  end;

end;

procedure TBithParse.ParseSpotCnlOrder(aData, aRef: string);
var
	aObj   : TJsonObject;
  sStatus	: string;
  aOrder  : TOrder;
  aAcnt		: TAccount;
begin
  if aData = '' then
  begin
    App.Log(llError, '%s %s ParseSoptCnlOrder data is empty',
       [ TExchangeKindDesc[FParent.ExchangeKind],  TMarketTypeDesc[mtSpot] ] ) ;
    Exit;
  end;

  aAcnt := App.Engine.TradeCore.FindAccount(FParent.ExchangeKind );//, amSpot);
  if aACnt = nil then Exit;  

	aObj := TJsonObject.ParseJSONValue( aData) as TJsonObject;
  try  
		sStatus := aObj.GetValue('status').Value;
 		aOrder	:= App.Engine.TradeCore.FindOrder(ekBithumb, aRef, 1 );

    if aOrder = nil then
    begin
      App.Log(llError, '%s cancel order not found %s, %s', [TExchangeKindDesc[FParent.ExchangeKind], aData, aRef]  );
      Exit;
    end;

    App.Engine.TradeBroker.Cancel( aOrder, sStatus='0000', sStatus )
    
  finally
 		if aObj <> nil then aObj.free;                                               
  end;
end;

procedure TBithParse.ParseSpotNewOrder(aData, aRef: string);
var
	aOrder : TOrder;
  aObj   : TJsonObject;
  sOID, sTmp, sStatus  : string;
  bAcpt : boolean;
begin

  if aData = '' then
  begin
    App.Log(llError, '%s %s ParseSoptNewOrder data is empty (%s)',
       [ TExchangeKindDesc[FParent.ExchangeKind],  TMarketTypeDesc[mtSpot], aRef ] ) ;
    Exit;
  end;

	aObj := TJsonObject.ParseJSONValue( aData) as TJsonObject;
  try
		sStatus := aObj.GetValue('status').Value;

  	bAcpt	:= true;
    if sStatus <> '0000' then      
      bAcpt:= false
    else
    	sOID := aObj.GetValue('order_id').Value;       
    
    aOrder:= App.Engine.TradeCore.FindOrder(ekBithumb, aRef );

    if aOrder = nil then
    begin
      App.Log(llError, '%s not found order : %s , %s ', [ TExShortDesc[ekBithumb], aData, aRef  ] );
      Exit;
    end;

    aOrder.OrderNo	:= sOID;

    App.DebugLog('Bithumb Spot : %s, %s, %s, %s, %s, %s, %s ', [  sStatus, sOID, aOrder.Symbol.Code
      , ifThenStr( aOrder.Side > 0, '매수','매도')
      , aOrder.PriceBI.OrgVal, aOrder.OrderQtyBI.OrgVal
      , ifThenStr( aOrder.ReduceOnly, 'Reduce', 'Limit' )
      ]  );    

		App.Engine.TradeBroker.Accept( aOrder, now, bAcpt, sStatus ) ;    
    
  finally
    if aObj <> nil then aObj.Free;
  end;

end;


procedure TBithParse.ParseSpotOrderDetail(aData, aRef: string);
var
	aObj, aObj2 : TJsonObject;
  aArr	 : TJsonArray;
  aVal	 : TJsonValue;
  aOrder : TOrder;
  sTmp , sStatus : string;
  i : integer;
begin
  if aData = '' then
  begin
    App.Log(llError, '%s %s ParseSoptOrderDetail data is empty',
       [ TExchangeKindDesc[FParent.ExchangeKind],  TMarketTypeDesc[mtSpot] ] ) ;
    Exit;
  end;

  aOrder	:= App.Engine.TradeCore.FindOrder( ekBithumb, aRef );
  if aOrder = nil then Exit;
  if aOrder.State <> osActive then Exit;  

	aObj := TJSONObject.ParseJSONValue(aData) as TJsonObject;
  try
		sStatus := aObj.GetValue('status').Value;
    if sStatus <> '0000' then
    begin
      App.Log(llError, '%s ParseSpotOrderDetail Error %s, %s',[
        TExchangeKindDesc[FParent.ExchangeKind] , aData, aRef
        ] );
      Exit;
    end;

    aObj2 := aObj.GetValue('data') as TJsonObject;
    sTmp	:= aObj2.GetValue('order_status').Value;
    // 체결일때만 처리한다..
    if sTmp = 'Completed' then
    begin
			aArr	:= aObj2.GetValue('contract') as TJsonArray;
      
      for I := 0 to aArr.Size-1 do
    	begin
        var aFill : TFill;      
        var sTrdID, sPrice, sVolume : string;	    
        var dtTime : TDateTime;        
        
        aVal			:= aArr.Get(i)  ;        		

        sTrdID	:= aVal.GetValue<string>('transaction_date');
        sPrice	:= aVal.GetValue<string>('price');
        sVolume := aVal.GetValue<string>('units');
        dtTime	:= UnixTimeToDateTime( StrToInt64( sTrdID), Length( sTrdID ));

        aFill		:= aOrder.Fills.Find( sTrdID );
        if aFill = nil then        
        begin
          aFill	:=  App.Engine.TradeCore.Fills[ekBithumb].New( sTrdID, dtTime, now,
            aRef, aOrder.Account, aOrder.Symbol, sVolume , aOrder.Side, sPrice);	        

          aFill.SetFee( aVal.GetValue<string>('fee') );
  //
          App.DebugLog('Bithumb Spot Fill : %s, %s, %s, %s, %s, %s ', [
            sTmp, aRef,  sPrice, sVolume, aFill.FeeBI.OrgVal, sTrdID
          ]);    

          App.Engine.TradeBroker.Fill( aOrder, aFill );                      
        end;
      end;
    end;
    
  finally
    if aObj <> nil then aObj.Free;
  end;


end;

// active order list
procedure TBithParse.ParseSpotOrderList(aData, aRef: string);
var
	aOrder : TOrder;
  aObj   : TJsonObject;
  aArr	 : TJsonArray;
  aVal	 : TJsonValue;
  sOID, sTmp, sStatus  : string;
  aSymbol : TSymbol;
  aAcnt		: TAccount;
  aExKind : TExchangeKind;
  i : Integer;
begin
  if aData = '' then
  begin
    App.Log(llError, '%s %s ParseSoptOrderList data is empty',
       [ TExchangeKindDesc[FParent.ExchangeKind],  TMarketTypeDesc[mtSpot] ] ) ;
    Exit;
  end;

  aExKind	:= ekBithumb;
  aSymbol	:= App.Engine.SymbolCore.FindSymbol( aExKind, aRef );
  aAcnt		:= App.Engine.TradeCore.FindAccount( aExKind );
  if (aSymbol = nil) or ( aAcnt = nil) then Exit;  

	aObj := TJsonObject.ParseJSONValue( aData) as TJsonObject;
  try
  	sStatus	:= aObj.GetValue('status').Value;
    if sStatus <> '0000' then
    begin
      App.Log(llError, '%s ParseSoptOrderList Error %s, %s',[
        TExchangeKindDesc[FParent.ExchangeKind] , aData, aRef
        ] );
      Exit;
    end;

    aArr := aObj.GetValue('data') as TJsonArray;
    if aArr <> nil then
    	for I := 0 to aArr.Size-1 do
      begin
      	aVal	:= aArr.Get(i);

        sOID	:= aVal.GetValue<string>('order_id');
        aOrder	:= App.Engine.TradeCore.FindOrder( aExKind, aAcnt, aSymbol, sOID);
        if aOrder = nil then
        begin
        	var iSide : integer;  
          var iTime : int64;
          var sOrderQty, sPrice, sType : string;
          var dtTime	: TDateTime;

          sOrderQty	:= aVal.GetValue<string>('units_remaining');
          sPrice		:= aVal.GetValue<string>('price');
          sType			:= aVal.GetValue<string>('type');
          iTime		  := aVal.GetValue<int64>('order_date');
				
          dtTime		:= now;   // itime -> dtTime 으로 변환 해야 함..

          if sType = 'bid' then iSide := 1
          else iSide := -1;
          
					aOrder	:= App.Engine.TradeCore.Orders[aExKind].NewOrder( aAcnt, aSymbol,
          	iSide, sOrderQty, pcLimit, sPrice, tmGTC );       

          aOrder.OrderNo	:= sOID;

          App.Engine.TradeBroker.Accept( aOrder, dtTime, true);
        end;        
      end;
    
  finally
		if aObj <> nil then aObj.free;
    
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

          if sCode = 'SAND' then
          	sCode := 'SAND';

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
	            if bCalc then begin
                App.Engine.SymbolCore.CalcIndex( aSymbol );
                App.Engine.TradeCore.Positions[ aSymbol.Spec.ExchangeType].UpdatePosition( aSymbol);
	            end;
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

