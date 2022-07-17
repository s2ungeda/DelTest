unit USharedManager;

interface

uses
  WinApi.Windows,
  System.Classes, System.SysUtils,  System.Variants
  , UApiTypes, UApiConsts ,

  USharedThread, USharedData, USharedConsts
  ;




type

  TSharedManager = class
  private
    FRefCnt : integer;
    FSharedThread: TSharedThread;
    FOnPushData: TSharedPushData;
    function GetMarketType(cMarket: char): TMarketType;
    function GetExchangeKind(cExKind: char): TExchangeKind;
  public
    constructor create;
    destructor  destroy; override;

    procedure RequestData( aExKind : TExchangeKind;  aMarket : TMarketType;
      aReqType : TRequestType; aData : string ); overload;

    procedure RequestData( aExKind : TExchangeKind;  aMarket : TMarketType;
      aReqType : TRequestType; aData : string; aRef : string ); overload;

    procedure OnSharedDataNotify( aData : TDataItem );

    function GetRequestType( cTrDiv : char ) : TRequestType;

    property SharedThread :  TSharedThread read FSharedThread;
    property OnPushData : TSharedPushData read FOnPushData write FOnPushData;
  end;

implementation

uses
  GApp
  ;

{ TSharedManasger }

constructor TSharedManager.create;
begin
  FRefCnt := 0;

  FSharedThread := TSharedThread.Create( OnSharedDataNotify, true);
end;

destructor TSharedManager.destroy;
begin
  if FSharedThread <> nil then
    FSharedThread.Terminate;
  inherited;
end;


procedure TSharedManager.OnSharedDataNotify(aData: TDataItem);
begin 
	App.Engine.ApiManager.ExManagers[ GetExchangeKind( aData.exKind) ].ReceivedData( 
    	GetMarketType(aData.market), GetRequestType(aData.trDiv), AnsiString( aData.data ), aData.ref )   ;  
end;

procedure TSharedManager.RequestData(aExKind: TExchangeKind;
  aMarket: TMarketType; aReqType: TRequestType; aData, aRef: string);
  var
    c1, c2, c3 : char;
begin

  case aExKind of
    ekBinance:  c1  := EX_BN;
    ekUpbit:    c1  := EX_UP;
    ekBithumb:  c1  := EX_BI;
  end;

  case aMarket of
    mtSpot:     c2 := 'S';
    mtFutures:  c2 := 'F';
  end;

  case aReqType of
    rtNewOrder: c3 := TR_NEW_ORD;
    rtCnlOrder: c3 := TR_CNL_ORD;
    rtOrderList: c3 := TR_REQ_ORD;
    rtPosition: c3 := TR_REQ_POS;
    rtBalance: c3 := TR_REQ_BAL;
    rtOrdDetail : c3 := TR_ORD_DETAIL;
  end;

  SharedThread.PushData( c1, c2, c3, aData, aRef )  ;

end;

procedure TSharedManager.RequestData(aExKind: TExchangeKind;
  aMarket: TMarketType; aReqType : TRequestType;  aData: string);
  var
    c1, c2, c3 : char;
begin
  inc( FRefCnt );

  case aExKind of
    ekBinance:  c1  := EX_BN;
    ekUpbit:    c1  := EX_UP;
    ekBithumb:  c1  := EX_BI;
  end;

  case aMarket of
    mtSpot:     c2 := 'S';
    mtFutures:  c2 := 'F';
  end;

  case aReqType of
    rtNewOrder: c3 := TR_NEW_ORD;
    rtCnlOrder: c3 := TR_CNL_ORD;
    rtOrderList: c3 := TR_REQ_ORD;
    rtPosition: c3 := TR_REQ_POS;
    rtBalance: c3 := TR_REQ_BAL;
    rtOrdDetail : c3 := TR_ORD_DETAIL;
  end;

  SharedThread.PushData( c1, c2, c3, aData, IntToStr( FRefCnt ) )  ;

end;

function TSharedManager.GetMarketType(cMarket: char): TMarketType;
begin
  case cMarket of
  	'S': Result := mtSpot;
    'F': Result := mtFutures;
  end;
end;


function TSharedManager.GetExchangeKind(cExKind: char): TExchangeKind;
begin
  case cExKind of
    EX_BN : Result := ekBinance;
    EX_UP : Result := ekUpbit;
    EX_BI : Result := ekBithumb;
  end;
end;

function TSharedManager.GetRequestType(cTrDiv: char): TRequestType;
begin  
  case cTrDiv of
    TR_NEW_ORD : Result := rtNewOrder;     // 신규 주문
    TR_CNL_ORD : Result := rtCnlOrder;     // 취소 주문
    TR_REQ_ORD : Result := rtOrderList;     // 주문 조회..
    TR_REQ_POS : Result := rtPosition;     // 포지션 조회..
    TR_REQ_BAL : Result := rtBalance;     // 잔고 조회...
    TR_ABLE_ORD: Result := rtAbleOrder;			// 주문가능금액..    
    TR_ORD_DETAIL : Result := rtOrdDetail;
  end;
end;

end.
