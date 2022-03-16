unit UDalinEngine;

interface

uses

  System.Classes,

  UFormBroker , USymbolCore, UApiConfigManager      ,
  UApiManager, UQuoteBroker, USymbolBroker
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
  public
    constructor Create;
    destructor  Destroy; override;

    property FormBroker: TFormBroker read FFormBroker;
    property SymbolCore: TSymbolCore read FSymbolCore;
    property ApiManager: TApimanager read FApiManager;

    property QuoteBroker : TQuoteBrokerManager read FQuoteBroker;
    property SymbolBroker: TSymbolBroker read FSymbolBroker;

    property ApiConfig : TApiConfigManager read FApiConfig;
  end;

implementation

{ TDalinCore }

constructor TDalinEngine.Create;
begin
  FApiConfig  := TApiConfigManager.Create;
  FQuoteBroker:= TQuoteBrokerManager.Create;
  FApiManager := TApimanager.Create;
  FFormBroker := TFormBroker.Create;
  FSymbolCore := TSymbolCore.Create;
  FSymbolBroker := TSymbolBroker.Create;
end;

destructor TDalinEngine.Destroy;
begin
  FApiManager.Free;
  FFormBroker.Free;
  FSymbolCore.Free;
  FApiConfig.Free;
  FQuoteBroker.Free;
  FSymbolBroker.Free;;
  inherited;
end;

end.
