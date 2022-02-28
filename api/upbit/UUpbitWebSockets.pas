unit UUpbitWebSockets;

interface

uses

  System.Classes, System.SysUtils, System.DateUtils

  , UWebSockets  , USymbols

  , UApiTypes

  ;

type

  TUpbitWebSocket = class( TWebSocket )
  private
    FMarketType: TMarketType;
    FSubList: TStrings;

    function GetDescript: string;
    procedure OnAfterConnect(Sender: TObject); override;
    procedure OnAfterDisconnect(Sender: TObject);  override;

    procedure SyncProc;  override;
    procedure SubScribe( aSymbol : TSymbol; bSub : boolean ) ; overload;
  public

    Constructor Create( iSockDiv : Integer; aMtType : TMarketType ); overload;
    destructor Destroy; override;

    procedure SubScribe( aSymbol : TSymbol ) ; overload;
    procedure UnSubScribe( aSymbol : TSymbol ) ;

    property MarketType  : TMarketType read FMarketType;
    property SubList  : TStrings  read FSubList;

    property Descript : string    read GetDescript;
  end;

implementation

uses
  GApp , GLibs
  , UApiConsts
  , UUpbitParse
  ;
{ TUpbitWebSocket }

constructor TUpbitWebSocket.Create(iSockDiv: Integer; aMtType: TMarketType);
begin
  inherited Create( iSockDiv );

  FMarketType := aMtType;
  FSubList    := TStringList.Create;

end;

destructor TUpbitWebSocket.Destroy;
begin
  FSubList.Free;
  inherited;
end;

function TUpbitWebSocket.GetDescript: string;
begin
  Result := Format('%s-%s-%d', [ 'UP', TMarketTypeDesc[FMarketType], Seq ]);
end;

procedure TUpbitWebSocket.OnAfterConnect(Sender: TObject);
begin
  App.Log(llInfo, ' %s Connected', [ Descript]);
end;

procedure TUpbitWebSocket.OnAfterDisconnect(Sender: TObject);
begin
  App.Log(llInfo, ' %s Disconnected', [ Descript]);
end;

procedure TUpbitWebSocket.SubScribe(aSymbol: TSymbol; bSub: boolean);
begin

end;

procedure TUpbitWebSocket.SubScribe(aSymbol: TSymbol);
begin

end;

procedure TUpbitWebSocket.UnSubScribe(aSymbol: TSymbol);
begin

end;

procedure TUpbitWebSocket.SyncProc;
begin
  gUpReceiver.ParseSocketData( FMarketType, Data.Packet );
end;



end.


