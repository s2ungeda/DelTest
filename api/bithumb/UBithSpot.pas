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
  , UBithParse
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

procedure TBithSpot.ParseRequestData(iCode: integer; sName, sData: string);
begin
  inherited;

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
	i : integer;
begin
	SetLength( Rest, 2 );	

  for I := 0 to 1 do
  begin
		var info : TDivInfo;
    info.Kind		:= GetExKind;
    info.Market	:= MarketType;
    info.Division	:= i;
    info.Index		:= i;
    info.WaitTime	:= ifThen( i = 0 , 20, 100 );
    MakeRestThread( info );
  end;
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

procedure TBithSpot.RequestData;
begin
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
