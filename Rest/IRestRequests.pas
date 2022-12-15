unit IRestRequests;

interface

uses
  System.Classes, System.SysUtils,

  REST.Client,  Rest.Types,

  UApiTypes
  ;

type
  IRestRequest = class
  private
    FRestReq: TRESTRequest;
    FParent: TObject;

  public
    constructor Create( aParent : TObject ; aReq : TRESTRequest ) ; overload;
    Destructor  Destroy; override;

    // common
    function Request( aExKind : TExchangeKind; AMethod : TRESTRequestMethod;  AResource : string;
       var outJson, outRes : string ) : boolean;

    function CheckShareddData(var sArr: TArray<string>; sData: string;
      iCount: integer; aPrcName: string): boolean;

    procedure PushData(aExKind: TExchangeKind; aMarket: TMarketType; c3: char;
      sData, sRef: string);
    //

    property RestReq : TRESTRequest read FRestReq;
    property Parent  : TObject read FParent ;
  end;

implementation

uses
  GApp
  , USharedConsts
  , URestManager
  ;

{ IRestRequest }

constructor IRestRequest.Create(aParent: TObject; aReq: TRESTRequest);
begin
  FRestReq := aReq;
  FParent  := aParent
end;

destructor IRestRequest.Destroy;
begin

  inherited;
end;


procedure IRestRequest.PushData( aExKind : TExchangeKind; aMarket : TMarketType;
   c3 : char; sData, sRef: string);
   var
    c1, c2 : char;
begin
  with ( FParent as TRestManager ) do
    if Assigned( OnPushData ) then
    begin
      case aExKind of
        ekBinance:c1 := EX_BN;
        ekUpbit:  c1 := EX_UP;
        ekBithumb:c1 := EX_BI;
      end;

      case aMarket of
        mtSpot:   c2 := 'S';
        mtFutures:c2 := 'F' ;
      end;

      OnPushData( c1, c2, c3, sData, sRef );
    end;
end;


function IRestRequest.CheckShareddData(var sArr : TArray<string>; sData: string;
   iCount : integer; aPrcName : string): boolean;
var
  iLen : integer;
begin

  sArr  := sData.Split(['|']);
  iLen  := high( sArr );

  if ( iLen < 0 ) or ( iLen + 1 <> iCount ) then
  begin
    App.Log(llError, '%s data is empty (%s) ', [ aPrcName, sData ] );
    Result := false;
  end else
    Result := true;
end;

function IRestRequest.Request(aExKind: TExchangeKind;
  AMethod: TRESTRequestMethod; AResource: string; var outJson,
  outRes: string): boolean;
begin
  Result := false;

  with FRestReq do
  begin
    Method   := AMethod;
    Resource := AResource;
  end;

  try
    try

      with FRestReq do
      begin
        Execute;

        OutJson:= Response.Content;

        if Response.StatusCode <> 200 then
        begin
          OutRes := Format( 'status : %d, %s', [ Response.StatusCode, Response.StatusText ] );
          Exit;
        end;

        Result  := true;
      end;

    except
      on E: Exception do
      begin
        OutRes := E.Message;
        Exit(false);
      end
    end;
  finally
    FRestReq.Params.Clear;
    FRestReq.Body.ClearBody;
  end;

end;

end.
