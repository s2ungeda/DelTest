unit UExchange;

interface

uses
  System.Classes,   System.SysUtils,    
  
  REST.Types, REST.Client ,

  URestRequests, URestThread, UCyclicThreads , UCyclicItems, 

  UApiTypes
  ;
type	

  TExchange = class
  private
    FRESTClient: TRESTClient;
    FRestRes: TRESTResponse;
    FRestReq: TRESTRequest;

    FExCode: string;
    FLastTime: TDateTime;
    FInfo: TExchangeInfo;

    FParent: TObject;
    FMarketType: TMarketType;
    FMarketIdx: integer;
    FMasterData: string;
    FCodes: TStrings;
    FCyclicThreads: TCyclicThread;
    FCyclicItems: TCyclicItems;
    FReqItems: TList;
    FRestThread: TRestThread;
    FReqIndex	: integer;
    FAccCnt: int64;
    FAccVal: int64;
    FMaxVal: int64;
    FMinVal: int64;
    FRestExet: TRESTExecutionThread;

  protected
    function IsAsyncWaiting : boolean;

  public   

	  //Rest : array of TRestThread;

    Constructor Create( aObj : TObject; aMarketType : TMarketType ); overload;
    Destructor  Destroy; override;          

    function Request( AMethod : TRESTRequestMethod;  AResource : string;  ABody : string;
      var OutJson, OutRes  : string  ) : boolean;
    function RequestAsync( aHandler: TCompletionHandler; AMethod : TRESTRequestMethod;  AResource : string  ) : boolean;

    procedure SetBaseUrl(url : string); inline;
    procedure SetParam( const aName, aValue : string;  aKind : TRESTRequestParameterKind = pkGETorPOST) ; inline;
    procedure Set406;

    procedure GetCodeList( var aList : TStringList ) ;
    function  GetExKind : TExchangeKind;
    function  GetCodeIndex( S : string ) : integer;

//----------------------------------------------------------- common request
    function ParsePrepareMaster : integer ; virtual; abstract;
    function RequestMaster : boolean ; virtual; abstract;
    function RequestDNWState : boolean; virtual; abstract;
    function RequestCandleData( sUnit : string; sCode : string ) : boolean; virtual; abstract;

    procedure ParseRequestData( iCode : integer; sName : string; sData : string ); virtual; abstract;
    // cyclicthread notify
 		procedure CyclicNotify( Sender : TObject ); virtual; abstract;     

//--------------------------------------------------------------
		// rest response notify
    procedure RestNotify( Sender : TObject ); virtual; 
    procedure OnHTTPProtocolError(Sender: TCustomRESTRequest); virtual;
    function PrepareMaster : boolean;
    function GetReqItems	 : TRequest;
//--------------------------------------------------------------
		procedure MakeRestThread( aInfo : TDivInfo );
    procedure MakeCyclicThread;
    procedure ProcCyclicWork;
    procedure MakeRestItems( iCount : integer);
		function  RestType( iType : integer ) : integer;    



    property Parent : TObject read FParent;

    property RESTClient: TRESTClient read FRESTClient;
    property RestReq: TRESTRequest read FRestReq;
    property RestRes: TRESTResponse read FRestRes;      
    //
    property RestExet      : TRESTExecutionThread read FRestExet write FRestExet;
    property RestThread    : TRestThread read FRestThread;
    property CyclicThreads : TCyclicThread read  FCyclicThreads; 
    property CyclicItems	 : TCyclicItems read FCyclicItems;
    property ReqItems			 : TList read FReqItems;    

    property Info : TExchangeInfo read FInfo;
    property LastTime : TDateTime read FLastTime write FLastTime;

    property MarketType : TMarketType read FMarketType;
    property MarketIdx  : integer read FMarketIdx;

    // 종목마스터 데이타.
    property MasterData : string read FMasterData;
    // 공통 Base Currency Code 가 담겨 있음.
    property Codes  : TStrings read FCodes;

    // request 평균을 구하기 위해
    property AccVal : int64 read FAccVal write FAccVal;
    property AccCnt : int64 read FAccCnt write FAccCnt;
    property MaxVal : int64 read FMaxVal write FMaxVal;
    property MinVal : int64 read FMinVal write FMinVal;
  end;

implementation

uses
  GApp,
  System.JSON,
  UExchangeManager, UApiConsts
  , windows
  ;

{ TExchagne }

constructor TExchange.Create(  aObj : TObject; aMarketType : TMarketType );
var
  i : Integer;
begin         

  FRESTClient   := TRESTClient.Create('');
  FRestReq := TRESTRequest.Create( FRESTClient );
  FRestRes := TRESTResponse.Create( nil );

  FRestReq.Response := FRestRes;

  FRestExet:= nil;

  FParent := aObj;
  FMarketType := aMarketType;
  FMarketIdx  := integer(FMarketType);

  FCodes := TStringList.Create;

//  Rest	:= nil;

  FRestReq.OnHTTPProtocolError :=  OnHTTPProtocolError;

  // Req 를 100 개 미리 만들어둠..
  FReqItems:= TList.Create;

  FCyclicItems:= TCyclicItems.Create;
  FCyclicThreads:= nil;
  FRestThread		:= nil;
  FReqIndex			:= 0;

  FAccCnt:= 0;
  FAccVal:= 0;

  FMaxVal:= 0;
  FMinVal:= 100;
end;

destructor TExchange.Destroy;
var
  i : integer;
begin

	if FCyclicThreads <> nil then
  	FCyclicThreads.Terminate;
  if FRestThread <> nil then  
    FRestThread.Terminate;
//                           
  FCyclicItems.Free;
  
//  if Rest <> nil then
//    for i := 0 to High(Rest) do   
//      if Rest[i] <> nil then
//	      Rest[i].Terminate;

  FCodes.Free;      

  FRestRes.Free;
	FRestReq.Free;
	FRESTClient.Free;

  FReqItems.Free;

  inherited;
end;  


function TExchange.GetCodeIndex( S : string ) : integer;
begin
  Result := ( FParent as TExchangeManager ).Codes.IndexOf(S);
end;

procedure TExchange.GetCodeList(var aList: TStringList);
var
  i : integer;
begin
  for I := 0 to ( FParent as TExchangeManager ).Codes.Count-1 do
    aList.Add( ( FParent as TExchangeManager ).Codes[i]);
end;

function TExchange.GetExKind: TExchangeKind;
begin
  Result := ( FParent as TExchangeManager ).ExchangeKind;
end;



function TExchange.GetReqItems: TRequest;
begin
	if FreqItems.Count <= 0 then Result := nil;
  
	if FReqIndex >= FReqItems.Count then
  	FReqIndex := 0;

  Result := TRequest( FReqItems.Items[ FReqIndex ]);
  inc( FReqIndex );
end;

function TExchange.IsAsyncWaiting: boolean;
begin
  Result := true;
  if ( FRestExet = nil ) or
   (( FRestExet <> nil ) and ( FRestExet.Finished ))  then
    Result := false;
end;

procedure TExchange.MakeRestItems( iCount : integer );
var
  aReq : TRequest;
  aItem: TCyclicItem;
  I: Integer;
  sUrl : string;
  bOpt : boolean;
begin
 
  FReqItems.Clear;
  sUrl :=  App.Engine.ApiConfig.GetBaseUrl( GetExKind, FMarketType );
  
//  for I := 0 to iCount-1 do
	for I := 0 to CyclicItems.Count-1 do    
  begin
  	aItem	:= CyclicItems.Cyclic[i];
    aReq := TRequest.Create;        
    aReq.init( sUrl, GetExKind = ekBithumb);
    aReq.SetParam( aItem.index, aItem.Interval );
    aReq.SetParam( aItem.Method, aItem.Resource, aItem.Name );
    aReq.Req.ReadTimeout := 3000;
    aReq.OnNotify := RestNotify;
    FReqItems.Add( aReq );
  end;
end;

procedure TExchange.MakeCyclicThread;
var
	aName : string;
begin
//	FCyclicThreads := TCyclicThread.Create(CyclicItems, CyclicNotify);   
	aName := Format('%s_%s', [ TExShortDesc[ GetExKind],  TMarketTypeDesc[FMarketType] ]);
	FCyclicThreads := TCyclicThread.Create(FReqItems, CyclicNotify, aName);   
end;

procedure TExchange.MakeRestThread(aInfo: TDivInfo);
begin
//	Rest[aInfo.Index]	:= TRestThread.Create( aInfo, ParseRequestData );
  FRestThread	:= TRestThread.Create( aInfo );
end;

procedure TExchange.OnHTTPProtocolError(Sender: TCustomRESTRequest);
begin
  if Sender <> nil  then
  begin
    App.Log( llError,  '%s Request Error : %s ( status : %d, %s)' , [ TExchangeKindDesc[GetExKind]
    ,  Sender.Response.Content ,  Sender.Response.StatusCode, Sender.Response.StatusText ]  );
  end;
end;

function TExchange.PrepareMaster: boolean;
var
  sTmp, sOut, sJson : string;
begin
  sTmp  := App.Engine.ApiConfig.GetBaseUrl( GetExKind , FMarketType );
  SetBaseUrl( sTmp );
  sTmp  := App.Engine.ApiConfig.GetPrepare(GetExKind , FMarketType );
  if Request( rmGET, sTmp , '', sJson, sOut ) then
  begin
    FMasterData := sJson ;
    ParsePrepareMaster;
  end else
  begin
    App.Log( llError, '', 'Failed %s %s PreparMaster (%s, %s)',
      [ TExchangeKindDesc[GetExKind], TMarketTypeDesc[FMarketType], sOut, sJson] );
    Exit( false );
  end;

  Result := FCodes.Count > 0 ;
end;


procedure TExchange.ProcCyclicWork;
var
	i , iSnd : integer;
//  aItem : TCyclicItem;
  aItem : TRequest;
  bSend : boolean;
  gap, nTick : DWORD;
  
begin
//  for I := 0 to FCyclicItems.Count-1 do
//  begin          
//    aItem := FCyclicItems.Cyclic[i];
//    if aItem = nil then continue;
//
//    bSend := false;
//    nTick := GetTickCount;
//
//    if aITem.LastTime <= 0 then
//    begin
//      gap := aItem.Count * INTERVAL;
//      if gap >= aItem.Interval then
//        bSend := true;
//      inc( aItem.Count );
//    end else
//    begin
//      gap   := nTick - aItem.LastTime ;
//      if gap >= aItem.Interval then
//        bSend := true;
//    end;
//
//    if bSend then begin
//      aItem.PrevTime  := aItem.LastTime;
//      aItem.LastTime  := nTick;
//     	cyclicNotify( aItem );
//    end;
//  end;

	iSnd := 0;
  for I := 0 to FReqItems.Count-1 do
  begin          
    aItem := TRequest(FReqItems.Items[i]);
    if aItem = nil then continue;

    bSend := false;
    nTick := GetTickCount;

    if aITem.LastTime <= 0 then
    begin
      gap := aItem.Count * INTERVAL;
      if gap >= aItem.Interval then
        bSend := true;
      inc( aItem.Count );
    end else
    begin
      gap   := nTick - aItem.LastTime ;
      if gap >= aItem.Interval then
        bSend := true;
    end;

    if (bSend) and ( aItem.State <> 1 ) then begin
      aItem.PrevTime  := aItem.LastTime;
      aItem.LastTime  := nTick;
     	cyclicNotify( aItem );
      inc( iSnd );
    end;

    if iSnd > 0 then break;          
  end;  
end;

function TExchange.Request(AMethod : TRESTRequestMethod;  AResource : string;  ABody : string;
      var OutJson, OutRes : string ): boolean;
begin

  with FRestReq do
  begin
    Method   := AMethod;
    Resource := AResource;
    if ABody <> '' then
      Body.Add( ABody);
//  Body.Add( ABody, ctAPPLICATION_JSON);
  end;


  try
    try

      FRestReq.Execute;

      if FRestRes.StatusCode <> 200 then
      begin
        OutRes := Format( 'status : %d, %s', [ FRestRes.StatusCode, FRestRes.StatusText ] );
        OutJson:= FRestRes.Content;
        Exit( false );
      end;



      OutJson := FRestRes.Content;
      Result := true;

    except
      on E: Exception do
      begin
        OutRes := E.Message;
        Exit(false);
      end
    end;
  finally
    FRestReq.Params.Clear;
  end;


end;

function TExchange.RequestAsync(aHandler: TCompletionHandler; AMethod : TRESTRequestMethod;  AResource : string ): boolean;
begin

  with FRestReq do
  begin
    OnHTTPProtocolError :=  OnHTTPProtocolError;
    Method   := AMethod;
    Resource := AResource;
    Result   := ExecuteAsync(aHandler) <> nil;
  end;

end;

// 인덱스를 리턴해준다..
procedure TExchange.RestNotify(Sender: TObject);
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

    if avg > 500 then
    begin
      App.Log(llInfo, 'bt_latency', 'avg : %05d : %d, %s %d  (%d, %d) ', [ avg,
         aReq.StatusCode, aReq.Name, {RestThread.QCount,}AccCnt, MaxVal, MinVal ]  );
    end;

    if MaxVal < gap then begin
      App.Log(llInfo, 'bt_latency', 'max : %05d : %d, %s %d (%d)', [ gap,
         aReq.StatusCode, aReq.Name,{ RestThread.QCount,} AccCnt, avg ]  );
      MaxVal := gap;
    end;

    if MinVal > gap then begin
      App.Log(llInfo, 'bt_latency', 'min : %05d : %d, %s  %d (%d)', [ gap,
         aReq.StatusCode, aReq.Name, {RestThread.QCount,} AccCnt, avg ]  );
      MinVal := gap;
    end;

    if True then


  finally
		aReq.State := 2;
  end;


end;

function TExchange.RestType(iType: integer): integer;
begin

  case GetExKind of
  	ekBinance : 
    	case FMarketType of
      	mtSpot : 
          case iType of
            PUB_REQ : Result := 0;
            PRI_REQ : Result := 0;
            ORD_REQ : Result := 1;
		      end;
        mtFutures : 
          case iType of
            PUB_REQ : Result := 0;
            PRI_REQ : Result := 0;
            ORD_REQ : Result := 1;
		      end;        
      end;  

  	ekBithumb : 
    	case iType of
        PUB_REQ 				:	Result := 0;
        PRI_REQ, ORD_REQ: Result := 1;
      end;

  	ekUpbit : 
    	case iType of
        PUB_REQ : Result := 0;
        PRI_REQ : Result := 1;
        ORD_REQ : Result := 2;
      end;            
  end;

end;

procedure TExchange.Set406;
begin
  FRestReq.Accept := '*/*';
  FRestReq.AcceptEncoding := 'gzip, deflate';
end;

procedure TExchange.SetBaseUrl(url: string);
begin
  FRESTClient.BaseURL := url;        
end;


procedure TExchange.SetParam(const aName, aValue: string;
  aKind: TRESTRequestParameterKind);
begin
  FRestReq.AddParameter(aName, aValue, aKind);
end;


end.
