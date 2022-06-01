unit UExchange;

interface

uses
  System.Classes,   System.SysUtils,    
  
  REST.Types, REST.Client ,

  URestRequests, URestThread,

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

    procedure FinRest( i : integer );

  public   

	  Rest : array of TRestThread;

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

//--------------------------------------------------------------
    procedure OnHTTPProtocolError(Sender: TCustomRESTRequest); virtual;
    function PrepareMaster : boolean;
//--------------------------------------------------------------
		procedure MakeRestThread( aInfo : TDivInfo );
		function  RestType( iType : integer ) : integer;    


    property Parent : TObject read FParent;

    property RESTClient: TRESTClient read FRESTClient;
    property RestReq: TRESTRequest read FRestReq;
    property RestRes: TRESTResponse read FRestRes;      

    property Info : TExchangeInfo read FInfo;
    property LastTime : TDateTime read FLastTime write FLastTime;

    property MarketType : TMarketType read FMarketType;
    property MarketIdx  : integer read FMarketIdx;

    // 종목마스터 데이타.
    property MasterData : string read FMasterData;
    // 공통 Base Currency Code 가 담겨 있음.
    property Codes  : TStrings read FCodes;
  end;

implementation

uses
  GApp,
  System.JSON,
  UExchangeManager, UApiConsts
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

  FParent := aObj;
  FMarketType := aMarketType;
  FMarketIdx  := integer(FMarketType);

  FCodes := TStringList.Create;

  Rest	:= nil;

  FRestReq.OnHTTPProtocolError :=  OnHTTPProtocolError;     
end;

destructor TExchange.Destroy;
var
  i : integer;
begin

  if Rest <> nil then
    for i := 0 to High(Rest) do   
      if Rest[i] <> nil then
      	FinRest(i);

  FCodes.Free;      
//  if FRestRes <> nil then
    FRestRes.Free;
  FRestReq.Free;
//  if FRestClient <> nil then
    FRESTClient.Free;
//  for I := 0 to High(Req) do
//    Req[i].Free;     
  inherited;
end;  


procedure TExchange.FinRest(i: integer);
begin
  Rest[i].Terminate;
  Rest[i].WaitFor;
  Rest[i].Free;
  Rest[i] := nil;
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

procedure TExchange.MakeRestThread(aInfo: TDivInfo);
begin
	Rest[aInfo.Index]	:= TRestThread.Create( aInfo, ParseRequestData );
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
