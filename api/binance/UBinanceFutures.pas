unit UBinanceFutures;

interface

uses
  System.Classes, System.SysUtils, System.DateUtils,

  UExchange,

  UApiTypes

  ;

type
  TBinanceFutures = class( TExchange )
  private
    FIndex  : integer;
    FLastIndex : integer;
      
    function RequestFuttMaster : boolean;
    function RequestFutTicker  : boolean;

    // async proc
    procedure AsyncFutAllTicker;
    procedure AsyncFutAllOrderBook;
    procedure MakeRest;
    
  public
    Constructor Create( aObj : TObject; aMarketType : TMarketType );
    Destructor  Destroy; override;

    function ParsePrepareMaster : integer; override;
    function RequestMaster : boolean ; override;
    function RequestCandleData( sUnit : string; sCode : string ) : boolean;override;

 
    procedure ParseRequestData( iCode : integer; sName : string; sData : string ); override;    
    procedure CyclicNotify( Sender : TObject ); override; 
    procedure RestNotify( Sender : TObject ); override;    

    procedure RequestData( iMod : integer );
  end;

implementation

uses
  GApp, UApiConsts
  , UBinanceParse
  , UCyclicItems, URestRequests
  , REST.Types
  ;

{ TBinanceSpotNMargin }



constructor TBinanceFutures.Create(aObj: TObject; aMarketType: TMarketType);
begin
  inherited Create( aObj, aMarketType );

  FIndex  := 0;
  FLastIndex := -1;  

end;


destructor TBinanceFutures.Destroy;
begin

  inherited;
end;

function TBinanceFutures.ParsePrepareMaster: integer;
begin
  gBinReceiver.ParsePrepareFuttMaster(MasterData);
end;



function TBinanceFutures.RequestCandleData(sUnit, sCode: string): boolean;
var
  sJson, sOut, sTmp : string;
  bRes : boolean;
  aKind : TMajorSymbolKind;
begin

  bRes := false;
  try
    SetParam('symbol', sCode );
    SetParam('limit', '50' );
    SetParam('interval', sUnit );

    if Request( rmGET, '/fapi/v1/klines' , '', sJson, sOut ) then
    begin
      sTmp := UpperCase(copy( sCode, 1, 3 ));
      if sTmp = 'BTC' then
        aKind := msBTC
      else aKind := msETH;
      gBinReceiver.ParseCandleData(aKind, sUnit, sJson);
    end else
    begin
      App.Log( llError, '', 'Failed %s RequestCandleData (%s, %s)',
        [ TExchangeKindDesc[GetExKind], sOut, sJson] );
      Exit( false );
    end;
    bRes := true;
  except
  end;

  Result := bRes;
end;

procedure TBinanceFutures.RequestData( iMod : integer );
begin

//	case iMod of
//      0 : begin
//        RestResult := Req[iMod].RequestAsync( AsyncFutAllOrderBook, rmGET, '/fapi/v1/ticker/bookTicker');
//      end;
//      1 : begin
//        RestResult := Req[iMod].RequestAsync( AsyncFutAllTicker, rmGET, '/fapi/v1/ticker/24hr');
//      end;    
//  end;

//  if ( FIndex <> FLastIndex )
//    or ( RestResult = nil )
//    or (( RestResult <> nil ) and ( RestResult.Finished )) then
//  begin
//    FLastIndex := FIndex;
//
//    case FIndex of
//      0 : begin
//        RestResult := Req[0].RequestAsync( AsyncFutAllOrderBook, rmGET, '/fapi/v1/ticker/bookTicker');
//      end;
//      1 : begin
//        RestResult := Req[1].RequestAsync( AsyncFutAllTicker, rmGET, '/fapi/v1/ticker/24hr');
//      end;
////      3 :begin  
////        RestResult := Req[2].RequestAsync( parseTicker, rmGET, '/public/ticker/ALL_KRW', true);
////      end;
//      else exit;
//    end;
//
//    if RestResult = nil then
//      App.Log( llError,  ' !! %s, %d Request %d Error ', [ TExchangeKindDesc[GetExKind], FIndex ] )
//    else begin
//      inc( FIndex );
//      if FIndex >= 2 then
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

procedure TBinanceFutures.AsyncFutAllOrderBook;
var
  sJson : string;
begin
//  sJson :=  Req[0].GetResponse;
//  if sJson = '' then Exit;
  gBinReceiver.ParseFutAllOrderBook( sJson );

end;

procedure TBinanceFutures.AsyncFutAllTicker;
var
  sJson : string;
begin
//  sJson :=  Req[1].GetResponse;
//  if sJson = '' then Exit;
  gBinReceiver.ParseFutAllTicker( sJson );

end;

function TBinanceFutures.RequestFutTicker: boolean;
var
  sOut, sJson : string;
begin
//  sTmp  := App.Engine.ApiConfig.GetBaseUrl( GetExKind , mtFutures );
//  SetBaseUrl( sTmp );

  if Request( rmGET, '/fapi/v1/ticker/24hr' , '', sJson, sOut ) then
  begin
    gBinReceiver.ParseFuttTicker(sJson);
  end else
  begin
    App.Log( llError, '', 'Failed %s ParseFuttTicker (%s, %s)',
      [ TExchangeKindDesc[GetExKind], sOut, sJson] );
    Exit( false );
  end;

  Result := App.Engine.SymbolCore.Futures[ GetExKind].Count > 0;
end;

function TBinanceFutures.RequestFuttMaster: boolean;
begin
  gBinReceiver.ParseFuttMaster(MasterData);
  Result := App.Engine.SymbolCore.Futures[ GetExKind].Count > 0;
end;

//function TBinanceFutures.RequestFuttMaster: boolean;
//var
//  sTmp, sOut, sJson : string;
//begin
//  sTmp  := App.Engine.ApiConfig.GetBaseUrl( GetExKind , mtFutures );
//  SetBaseUrl( sTmp );
//
//  if Request( rmGET, '/fapi/v1/exchangeInfo' , '', sJson, sOut ) then
//  begin
//    gBinReceiver.ParseFuttMaster(sJson);
//  end else
//  begin
//    App.Log( llError, '', 'Failed %s Fut PreparMaster (%s, %s)',
//      [ TExchangeKindDesc[GetExKind], sOut, sJson] );
//    Exit( false );
//  end;
//
//  Result := App.Engine.SymbolCore.Futures[ GetExKind].Count > 0;
//
//end;

function TBinanceFutures.RequestMaster: boolean;
begin  
  
  Result := RequestFuttMaster
         and RequestFutTicker;
         
	if Result then
		MakeRest;
end;


procedure TBinanceFutures.MakeRest;
var
  aItem : TCyclicItem;  
begin
  aItem := CyclicItems.New('orderbook');
  aItem.Interval  := 1000;
  aItem.Index     := 0;
  aItem.Resource	:= '/fapi/v1/ticker/bookTicker';  
  aITem.Method		:= rmGET;                                          

	MakeRestItems( 0 );     
  MakeCyclicThread;
end;

procedure TBinanceFutures.CyclicNotify(Sender: TObject);
var
  aReq  : TRequest;
begin
  if Sender = nil then Exit;
  aReq := Sender as TRequest;

  if aReq.RequestAsync then
  	aReq.State := 1;
end;

procedure TBinanceFutures.RestNotify(Sender: TObject);
begin
  if Sender = nil then Exit;
  inherited  RestNotify( Sender )         
end;



procedure TBinanceFutures.ParseRequestData(iCode: integer; sName,
  sData: string);
begin
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
		  gBinReceiver.ParseFutAllOrderBook( sData );

end;

end.
