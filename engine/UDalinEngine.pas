unit UDalinEngine;

interface

uses

  System.Classes,

  UFormBroker , USymbolCore, UApiConfigManager      ,
  UApiManager
  ;

type
  TDalinEngine = class
  private
    FFormBroker: TFormBroker;
    FSymbolCore: TSymbolCore;
    FApiConfig: TApiConfigManager;
    FApiManager: TApimanager;
  public
    constructor Create;
    destructor  Destroy; override;

    property FormBroker: TFormBroker read FFormBroker;
    property SymbolCore: TSymbolCore read FSymbolCore;
    property ApiManager: TApimanager read FApiManager;

    property ApiConfig : TApiConfigManager read FApiConfig;
  end;

implementation

{ TDalinCore }

constructor TDalinEngine.Create;
begin
  FApiConfig  := TApiConfigManager.Create;
  FApiManager := TApimanager.Create;
  FFormBroker := TFormBroker.Create;
  FSymbolCore := TSymbolCore.Create;

end;

destructor TDalinEngine.Destroy;
begin
  FApiManager.Free;
  FFormBroker.Free;
  FSymbolCore.Free;
  FApiConfig.Free;
  inherited;
end;

end.
