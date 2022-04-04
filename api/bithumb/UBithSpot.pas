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
  public
    Constructor Create( aObj : TObject; aMarketType : TMarketType );
    Destructor  Destroy; override;

    procedure RequestData;

    procedure RequestOrderBook( c : char ) ; overload;
    function RequestDNWState : boolean; override;

    function ParsePrepareMaster : integer  ; override;
    function RequestMaster : boolean ; override;

  end;

//var
//  CriticalSection: TRTLCriticalSection;

implementation

uses
  GApp   , UApiConsts
  , UBithParse
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
var
  i : integer;
begin

  for I := 0 to High(Req) do
  begin
    Req[i].init( App.Engine.ApiConfig.GetBaseUrl( GetExKind , mtSpot ));
    Req[i].Req.OnHTTPProtocolError := OnHTTPProtocolError;
  end;

  Result := RequestTicker
    and RequestOrderBook
    and RequestDNWState
    ;
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
begin
  master := TJsonObject.ParseJSONValue( MasterData ) as TJsonObject;
  aObj := master.GetValue('data') as TJsonObject;

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

    end;
    Result := App.Engine.SymbolCore.Spots[GetExKind].Count > 0 ;
  finally
    if aObj <> nil then
      aObj.Free;
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

procedure TBithSpot.RequestData;
begin
  if ( FIndex <> FLastIndex )
    or ( RestResult = nil )
    or (( RestResult <> nil ) and ( RestResult.Finished )) then
  begin
    FLastIndex := FIndex;

    case FIndex of
      0 , 2: begin
        RestResult := Req[0].RequestAsync( parseOrderBook, rmGET, '/public/orderbook/ALL_KRW', true);
      end;
      1 : begin
        RestResult := Req[1].RequestAsync( parseAssetsstatus, rmGET, '/public/assetsstatus/ALL', true);
      end;
      3 :begin
        RestResult := Req[2].RequestAsync( parseTicker, rmGET, '/public/ticker/ALL_KRW', true);
      end;
      else exit;
    end;

    if RestResult = nil then
      App.Log( llError,  ' !! %s, %d Request %d Error ', [ TExchangeKindDesc[GetExKind], FIndex ] )
    else begin
      inc( FIndex );
      if FIndex >= 4 then
        FIndex := 0;
    end;

  end else
  begin
    var s : string;
    if RestResult.Finished then s := 'fin' else s := 'not fin';
    App.DebugLog( '!! %s, %d waiting req -> %d %s ', [ TExchangeKindDesc[GetExKind], FIndex, RestResult.ThreadID, s ]  );
  end;
end;


procedure TBithSpot.parseOrderBook;
var
  sJson : string;
begin
  sJson :=  Req[0].GetResponse;
  if sJson = '' then Exit;
  gBithReceiver.ParseSpotOrderBook( sJson );

end;
procedure TBithSpot.parseAssetsstatus;
var
  sJson : string;
begin
  sJson :=  Req[1].GetResponse;
  if sJson = '' then Exit;
  gBithReceiver.ParseDnwState( sJson );

end;
procedure TBithSpot.parseTicker;
var
  sJson : string;
begin
  sJson :=  Req[2].GetResponse;
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
