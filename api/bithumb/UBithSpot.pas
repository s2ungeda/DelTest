unit UBithSpot;

interface

uses
  System.Classes, System.SysUtils, System.DateUtils, //Windows,

  System.JSON,  Rest.Json , Rest.Types ,  REST.Client,

  UExchange, USymbols ,  UMarketSpecs,

  UApiTypes

  ;

type
  TBithSpot = class( TExchange )
  private

    FIndex  : integer;
    FLastIndex : integer;

    function RequestTicker : boolean;
    function RequestOrderBook : boolean;  overload;

    procedure OnHTTPProtocolError(Sender: TCustomRESTRequest);
    procedure parseAssetsstatus;
    procedure parseOrderBook;
    procedure parseTicker;
    procedure MakeRest;
  public
    Constructor Create( aObj : TObject; aMarketType : TMarketType );
    Destructor  Destroy; override;

    procedure RequestData;

    procedure CyclicNotify( Sender : TObject ); override; 
    procedure RestNotify( Sender : TObject ); override;    

    procedure RequestOrderBook( c : char ) ; overload;
    function RequestDNWState : boolean; override;
    procedure ParseRequestData( iCode : integer; sName : string; sData : string ); override;

    function ParsePrepareMaster : integer  ; override;
    function RequestMaster : boolean ; override;
    function RequestCandleData( sUnit : string; sCode : string ) : boolean;override;

  end;

//var
//  CriticalSection: TRTLCriticalSection;

implementation

uses
  GApp   , UApiConsts
  , UBithParse, URestItems ,UCyclicItems, URestRequests
  , USymbolUtils
  , Math
  ;

{ TBinanceSpotNMargin }

constructor TBithSpot.Create(aObj: TObject; aMarketType: TMarketType);
begin
  inherited Create( aObj, aMarketType );

  FIndex  := 0;
  FLastIndex := -1;

end;



destructor TBithSpot.Destroy;
begin

  inherited;
end;          

function TBithSpot.ParsePrepareMaster : integer;
var
  aObj, master : TJsonObject;
  aVal : TJsonValue;
  aPair : TJsonPair;
  i : Integer;

begin
  master := TJsonObject.ParseJSONValue( MasterData ) as TJsonObject;
  if master = nil then Exit;
  aObj := master.GetValue('data') as TJsonObject;

  try
    for I := 0 to aObj.Size-1 do
    begin
      aPair := aObj.Get(i);
      if aPair.JsonValue.ClassType = TJSONObject then
      begin
        aVal  := aPair.JsonValue;
  //      DoLog( Format('%d. %s : %s ', [ i, aPair.JsonString.Value, aPair.JsonValue.Value]));
        Codes.Add( aPair.JsonString.Value );
      end;
    end;
  finally
    if aObj <> nil then aObj.Free
  end;
end;                                      



function TBithSpot.RequestMaster: boolean;
begin

  Result := RequestTicker
    and RequestOrderBook
    and RequestDNWState
    ;

	if Result then
		MakeRest;
end;

procedure TBithSpot.MakeRest;
var
  aItem : TCyclicItem;  
  info : TDivInfo;
begin
	// request item 을 메모리에 미리 만들어둠..생성/해제 줄이기 위해.
	MakeRestItems( 50 );      

  info.Kind		:= GetExKind;
  info.Market	:= MarketType;
  info.Index	:= 0;
  info.WaitTime := 20;
  // rest thread
  MakeRestThread( info );

  aItem := CyclicItems.New('orderbook');
  aItem.Interval  := 100;
  aItem.Index     := 1;

  aItem := CyclicItems.New('ticker');
  aItem.Interval  := 200;
  aItem.Index     := 2;

  aItem := CyclicItems.New('status');
  aItem.Interval  := 3000;
  aItem.Index     := 3;
	// cyclic thread .. 리커버리 끝나면  resume..
  // MakeCyclicThread;
end;


function TBithSpot.RequestTicker: boolean;
var
  aObj, master : TJsonObject;
  aVal : TJsonValue;
  aPair : TJsonPair;
  i : Integer;
  sBase, sCode, sTmp : string;
  aSymbol : TSymbol;
  bNew : boolean;
  aList: TList;
begin
  aList := nil;
  master := TJsonObject.ParseJSONValue( MasterData ) as TJsonObject;
  aObj := master.GetValue('data') as TJsonObject;

  aList := TList.Create;
  try

    for I := 0 to aObj.Size-1 do
    begin
      aPair := aObj.Get(i);
      if aPair.JsonValue.ClassType <> TJSONObject then continue;
      sCode := aPair.JsonString.Value;
      if ( GetCodeIndex( sCode ) < 0 ) then Continue;

      aSymbol := App.Engine.SymbolCore.Symbols[GetExKind].FindCode(sCode);
      if aSymbol = nil then
      begin
        bNew := true;
        aSymbol := App.Engine.SymbolCore.RegisterSymbol(GetExKind, mtSpot, sCode );
        if aSymbol = nil then Exit (false);
      end else
        bNew := false;

      with aSymbol do
      begin
        OrgCode     := sCode+'_KRW';
        Spec.BaseCode    := sCode;
        Spec.QuoteCode   := 'KRW';
        Spec.SettleCode  := 'KRW';
      end;

      aVal  := aPair.JsonValue;

      aSymbol.DayOpen := StrToFloatDef( aVal.GetValue<string>( 'opening_price' ), 0.0 );
      aSymbol.DayHigh := StrToFloatDef( aVal.GetValue<string>( 'max_price' ), 0.0 );
      aSymbol.DayLow  := StrToFloatDef( aVal.GetValue<string>( 'min_price' ), 0.0 );
      aSymbol.Last    := StrToFloatDef( aVal.GetValue<string>( 'closing_price' ), 0.0 );
      aSymbol.PrevClose   := StrToFloatDef( aVal.GetValue<string>( 'prev_closing_price' ), 0.0 );
      aSymbol.DayAmount   := StrToFloatDef( aVal.GetValue<string>( 'acc_trade_value_24H' ), 0.0 ) / 100000000;
      aSymbol.DayVolume   := StrToFloatDef( aVal.GetValue<string>( 'units_traded_24H' ), 0.0 );

  //    aSymbol.Time  := UnixToDateTime(  aVal.GetValue<int64>( 'date' ) );
      aSymbol.LocalTime := now;

      if bNew then
        App.Engine.SymbolCore.RegisterSymbol( GetExKind, aSymbol );

      if ( sCode <> 'BTC' ) and ( sCode <> 'ETH') and ( sCode <> 'XRP') then
        aList.Add( aSymbol );

    end;
    Result := App.Engine.SymbolCore.Spots[GetExKind].Count > 0 ;

    aList.Sort(CompareDailyAmount);
    if aList.Count > 2 then
    begin
      App.Engine.ApiManager.ExManagers[ekBithumb].TopAmtSymbols[0] := TSymbol( aList.Items[0] );
      App.Engine.ApiManager.ExManagers[ekBithumb].TopAmtSymbols[1] := TSymbol( aList.Items[1] );
    end;
  finally
    if aObj <> nil then
      aObj.Free;
    if aList <> nil then aList.Free;

  end;
end;

procedure TBithSpot.CyclicNotify(Sender: TObject);
var
	aItem : TCyclicItem;
  aReq  : TRequest;
  sNm, sRsrc  : string;
begin
  if Sender = nil then Exit;       
  aItem := Sender as TCyclicItem;

  try
  	aReq	:= GetReqItems;
    if (aReq <> nil) and ( aReq.State <> 1 ) then
    begin
			aReq.Req.Params.Clear;

      case aItem.index of
        1 : sRsrc:= '/public/orderbook/ALL_KRW';       
        2 : sRsrc:= '/public/ticker/ALL_KRW'; 		
        3 : sRsrc:= '/public/assetsstatus/ALL';	
      end; 

      aReq.SetParam(rmGET, sRsrc, aITem.Name);

      if RestThread <> nil then
        RestThread.PushQueue( aReq );           
    end else
    if (aReq <> nil) and ( aReq.State = 1 ) then
    begin
      App.DebugLog('%s, %s State = 1 !!! ', [TExShortDesc[GetExKind] , aItem.Name] );
    end; 
  
  except
  	on e : exception do
	  	App.Log(llError, '%s %s CyclicNotify Error : %s', [ TExShortDesc[GetExKind]
      	,aItem.Name , e.Message ]  );
  end;
end;

procedure TBithSpot.RestNotify(Sender: TObject);
var
   aReq : TRequest;
   sTmp : string;
   avg  : integer;
   gap  : integer;
begin
  if Sender = nil then Exit;
  try
  	aReq := Sender as TRequest;
	  ParseRequestData( aReq.StatusCode, aReq.Name, aReq.Content );

    gap := (aReq.EnTime - aReq.StTime);
    AccCnt:= AccCnt+1;
    AccVal:= AccVal + gap;

    avg := AccVal div AccCnt;

    if avg > 200 then
    begin
      App.Log(llInfo, 'bt_latency', 'avg : %05d : %d, %s %d, %d  (%d, %d) ', [ avg,
         aReq.StatusCode, aReq.Name, RestThread.QCount, AccCnt, MaxVal, MinVal ]  );
    end;

    if MaxVal < gap then begin
      App.Log(llInfo, 'bt_latency', 'max : %05d : %d, %s %d, %d (%d)', [ gap,
         aReq.StatusCode, aReq.Name, RestThread.QCount, AccCnt, avg ]  );
      MaxVal := gap;
    end;

    if MinVal > gap then begin
      App.Log(llInfo, 'bt_latency', 'min : %05d : %d, %s %d, %d (%d)', [ gap,
         aReq.StatusCode, aReq.Name, RestThread.QCount, AccCnt, avg ]  );
      MinVal := gap;
    end;

    if True then


  finally
		aReq.State := 2;
  end;

end;

function TBithSpot.RequestOrderBook: boolean;
var
  sOut, sJson : string;
begin
  SetBaseUrl( App.Engine.ApiConfig.GetBaseUrl( GetExKind , mtSpot ) );

  // 406 에러 때문에 아래 와 같이 헤더 추가
  Set406;

//  if not RequestAsync(
//     ReceiveAsyncData
//   , rmGET, '/public/orderbook/ALL_KRW') then
//     App.Log( llError, 'Failed %s RequestOrderBook ', [ TExchangeKindDesc[GetExKind]] );

  if Request( rmGET, '/public/orderbook/ALL_KRW', '', sJson, sOut ) then
  begin
    gBithReceiver.ParseSpotOrderBook( sJson );
  end else
  begin
    App.Log( llError, '', 'Failed %s RequestOrderBook (%s, %s)',
      [ TExchangeKindDesc[GetExKind], sOut, sJson] );
    Exit( false );
  end;

  Result := true;
end;

procedure TBithSpot.RequestOrderBook( c : char );
var
  sOut, sJson : string;
begin

  //EnterCriticalSection(CriticalSection);
  try

    SetBaseUrl( App.Engine.ApiConfig.GetBaseUrl( GetExKind , mtSpot ) );
    // 406 에러 때문에 아래 와 같이 헤더 추가
    Set406;

    if Request( rmGET, '/public/orderbook/ALL_KRW', '', sJson, sOut ) then
    begin
      gBithReceiver.ParseSpotOrderBook( sJson );
    end else
    begin
      App.Log( llError, '', 'Failed %s RequestOrderBook (%s, %s)',
        [ TExchangeKindDesc[GetExKind], sOut, sJson] );
      Exit;
    end;

//    if not RequestAsync(
//       ReceiveAsyncData
//     , rmGET, '/public/orderbook/ALL_KRW') then
//       App.Log( llError, 'Failed %s RequestOrderBook ', [ TExchangeKindDesc[GetExKind]] );
  except
  //  LeaveCriticalSection(CriticalSection);
  end;
end;

function TBithSpot.RequestCandleData(sUnit, sCode: string): boolean;
var
  sOut, sJson, sRrs, sTmp : string;
  bRes : boolean;
  aKind : TMajorSymbolKind;
begin

  bRes := false;
  try

    SetBaseUrl( App.Engine.ApiConfig.GetBaseUrl( GetExKind , mtSpot ) );
    // 406 에러 때문에 아래 와 같이 헤더 추가
    Set406;

    sRrs := Format('public/candlestick/%s/%s', [ sCode, sUnit] );
    if Request( rmGET, sRrs, '', sJson, sOut ) then
    begin
      sTmp := copy( sCode, 1, 3 );
      if sTmp = 'BTC' then
        aKind := msBTC
      else aKind := msETH;
      gBithReceiver.ParseCandleData(aKind, sUnit, sJson );
    end else
    begin
      App.Log( llError, '', 'Failed %s RequestCandleData (%s, %s)',
        [ TExchangeKindDesc[GetExKind], sOut, sJson] );
      Exit ( false );
    end;
    bRes := true;
  except
  end;

  Result := bRes;

end;

procedure TBithSpot.ParseRequestData(iCode: integer; sName, sData: string);
begin
  inherited;

  if sData = '' then
  begin
  	App.Log(llError, '%s %s Data is Empty', [ TExchangeKindShortDesc[ GetExKind ], sName ]  );
    Exit;
  end;

  if iCode <> 200 then begin
  	App.Log(llError, '%s %s Request is Failed : %d,  %s', [ TExchangeKindShortDesc[ GetExKind ], sName, iCode, sData ]  );
    Exit;  	
  end else
		if sName = 'orderbook' then
			gBithReceiver.ParseSpotOrderBook( sData )
    else if sName = 'ticker' then
    	gBithReceiver.ParseTicker( sData )
    else if sName = 'status' then
    	gBithReceiver.ParseDnwState( sData );    
  
end;

procedure TBithSpot.RequestData;
var
	aReq : TReqeustItem;
  i, idx : integer;
begin



	exit;

	for I := 0 to 2 do
  begin

//    if ( i = 2 ) and ( FIndex mod 3 = 0 ) then
//    	continue;
        
    aReq := TReqeustItem.Create;
    aReq.AMethod	:= rmGET;
    aReq.Req.init( App.Engine.ApiConfig.GetBaseUrl( GetExKind , mtSpot ), true );      


    case i of
      0 : begin aReq.AResource:= '/public/orderbook/ALL_KRW';   	idx	:= RestType(PUB_REQ);  
      					aReq.Name := 'orderbook'; end;
      1 : begin aReq.AResource:= '/public/assetsstatus/ALL';		 	idx := RestType(PUB_REQ); 
     						aReq.Name := 'assetsstatus'; 		end;
      2 : begin aReq.AResource:= 'public/ticker/ALL_KRW'; 				idx := RestType(PUB_REQ);
      					aReq.Name := 'ticker'; 		end;
    end;
    
//    if Rest[idx] <> nil then
//    	Rest[idx].PushQueue( aReq );  
  end;


  inc( FIndex );
  if FIndex > (High(int64) - 1000) then
  	FIndex := 0;
    
//  if ( FIndex <> FLastIndex )
//    or ( RestResult = nil )
//    or (( RestResult <> nil ) and ( RestResult.Finished )) then
//  begin
//    FLastIndex := FIndex;
//
//    case FIndex of
//      0 , 2: begin
//        RestResult := Req[0].RequestAsync( parseOrderBook, rmGET, '/public/orderbook/ALL_KRW', true);
//      end;
//      1 : begin
//        RestResult := Req[1].RequestAsync( parseAssetsstatus, rmGET, '/public/assetsstatus/ALL', true);
//      end;
//      3 :begin
//        RestResult := Req[2].RequestAsync( parseTicker, rmGET, '/public/ticker/ALL_KRW', true);
//      end;
//      else exit;
//    end;
//
//    if RestResult = nil then
//      App.Log( llError,  ' !! %s, %d Request %d Error ', [ TExchangeKindDesc[GetExKind], FIndex ] )
//    else begin
//      inc( FIndex );
//      if FIndex >= 4 then
//        FIndex := 0;
//    end;
//
//  end else
//  begin
//    var s : string;
//    if RestResult.Finished then s := 'fin' else s := 'not fin';
//    App.DebugLog( '!! %s, %d waiting req -> %d %s ', [ TExchangeKindDesc[GetExKind], FIndex, RestResult.ThreadID, s ]  );
//  end;
end;


procedure TBithSpot.parseOrderBook;
var
  sJson : string;
begin
 //sJson :=  Req[0].GetResponse;
  if sJson = '' then Exit;
  gBithReceiver.ParseSpotOrderBook( sJson );

end;
procedure TBithSpot.parseAssetsstatus;
var
  sJson : string;
begin
  //sJson :=  Req[1].GetResponse;
  if sJson = '' then Exit;
  gBithReceiver.ParseDnwState( sJson );

end;
procedure TBithSpot.parseTicker;
var
  sJson : string;
begin
  //sJson :=  Req[2].GetResponse;
  if sJson = '' then Exit;
  gBithReceiver.ParseTicker( sJson );
end;

function TBithSpot.RequestDNWState : boolean;
var
  sOut, sJson : string;
begin

 // EnterCriticalSection(CriticalSection);
  try
    SetBaseUrl( App.Engine.ApiConfig.GetBaseUrl( GetExKind , mtSpot ) );

    Set406;

    if Request( rmGET, '/public/assetsstatus/ALL', '', sJson, sOut ) then
    begin
      gBithReceiver.ParseDnwState( sJson );
    end else
    begin
      App.Log( llError, '', 'Failed %s RequestDNWState (%s, %s)',
        [ TExchangeKindDesc[GetExKind], sOut, sJson] );
      Exit (false);
    end;

    result := true;
//    if not RequestAsync(
//      ReceiveAsyncData
//     , rmGET, '/public/assetsstatus/ALL') then
//       App.Log( llError, 'Failed %s RequestDNWStte ', [ TExchangeKindDesc[GetExKind]] );
  except
   // LeaveCriticalSection(CriticalSection);
  end;

end;

procedure TBithSpot.OnHTTPProtocolError(Sender: TCustomRESTRequest);
begin
  if Sender <> nil  then
  begin
    App.Log( llError,  '%s Async Request Error : %s ( status : %d, %s)' , [ TExchangeKindDesc[GetExKind]
    ,  Sender.Response.Content ,  Sender.Response.StatusCode, Sender.Response.StatusText ]  );
  end;
end;

end.
