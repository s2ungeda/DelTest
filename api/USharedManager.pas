unit USharedManager;

interface

uses
  WinApi.Windows,
  System.Classes, System.SysUtils,  System.Variants
  , UApiTypes, UApiConsts ,

  USharedThread, USharedData, USharedConsts
  ;




type

  TSharedManasger = class
  private
    FRefCnt : integer;
    FSharedThread: TSharedThread;
    FOnPushData: TSharedPushData;
  public
    constructor create;
    destructor  destroy; override;

    procedure RequestData( aExKind : TExchangeKind;  aMarket : TMarketType;
      aReqType : TRequestType; aData : string );

    procedure OnSharedDataNotify( aData : TDataItem );

    property SharedThread :  TSharedThread read FSharedThread;
    property OnPushData : TSharedPushData read FOnPushData write FOnPushData;
  end;

implementation

{ TSharedManasger }

constructor TSharedManasger.create;
begin
  FRefCnt := 0;
end;

destructor TSharedManasger.destroy;
begin

  inherited;
end;

procedure TSharedManasger.OnSharedDataNotify(aData: TDataItem);
begin

end;

procedure TSharedManasger.RequestData(aExKind: TExchangeKind;
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
  end;

  SharedThread.PushData( c1, c2, c3, aData, IntToStr( FRefCnt ) )  ;

end;

end.
