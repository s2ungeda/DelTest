unit UDalinEngine;

interface

uses

  System.Classes,

  UFormBroker , UTradeCore, USymbolCore, UApiConfigManager ,
  UApiManager,  UQuoteBroker, USymbolBroker, UTradeBroker  ,
  USharedManager
  ;

type
  TDalinEngine = class
  private
    FFormBroker: TFormBroker;
    FSymbolCore: TSymbolCore;
    FApiConfig: TApiConfigManager;
    FApiManager: TApimanager;
    FQuoteBroker: TQuoteBrokerManager;
    FSymbolBroker: TSymbolBroker;
    FTradeCore: TTradeCore;
    FTradeBroker: TTradeBroker;
    FSharedManager: TSharedManager;

  public
    constructor Create;
    destructor  Destroy; override;

    property FormBroker: TFormBroker read FFormBroker;
    property TradeCore : TTradeCore  read FTradeCore;
    property SymbolCore: TSymbolCore read FSymbolCore;
    property ApiManager: TApimanager read FApiManager;

    property QuoteBroker : TQuoteBrokerManager read FQuoteBroker;
    property TradeBroker: TTradeBroker read FTradeBroker;
    property SymbolBroker: TSymbolBroker read FSymbolBroker;

    property SharedManager : TSharedManager read FSharedManager;

    property ApiConfig : TApiConfigManager read FApiConfig;
  end;

implementation

{ TDalinCore }

constructor TDalinEngine.Create;
begin
  FApiConfig    := TApiConfigManager.Create;
  FQuoteBroker  := TQuoteBrokerManager.Create;
  FTradeBroker  := TTradeBroker.Create;
  FApiManager   := TApimanager.Create;
  FFormBroker   := TFormBroker.Create;
  FTradeCore	  := TTradeCore.Create;
  FSymbolCore   := TSymbolCore.Create;
  FSymbolBroker := TSymbolBroker.Create;


  FSharedManager:= TSharedManager.create;
end;

destructor TDalinEngine.Destroy;
begin
	FSharedManager.Free;
  FApiManager.Free;
  FFormBroker.Free;
  FTradeCore.Free;
  FSymbolCore.Free;
  FApiConfig.Free;
  FQuoteBroker.Free;
  FTradeBroker.Free;
  FSymbolBroker.Free;;
  inherited;
end;

end.
