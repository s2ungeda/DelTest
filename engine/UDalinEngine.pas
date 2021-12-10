unit UDalinEngine;

interface

uses

  System.Classes,

  UFormBroker , USymbolCore, UApiConfigManager
  ;

type
  TDalinEngine = class
  private
    FFormBroker: TFormBroker;
    FSymbolCore: TSymbolCore;
    FApiConfig: TApiConfigManager;
  public
    constructor Create;
    destructor  Destroy; override;

    property FormBroker: TFormBroker read FFormBroker;
    property SymbolCore: TSymbolCore read FSymbolCore;
    property ApiConfig : TApiConfigManager read FApiConfig;
  end;

implementation

{ TDalinCore }

constructor TDalinEngine.Create;
begin
  FApiConfig  := TApiConfigManager.Create;
  FFormBroker := TFormBroker.Create;
  FSymbolCore := TSymbolCore.Create;

end;

destructor TDalinEngine.Destroy;
begin
  FFormBroker.Free;
  FSymbolCore.Free;
  FApiConfig.Free;
  inherited;
end;

end.
