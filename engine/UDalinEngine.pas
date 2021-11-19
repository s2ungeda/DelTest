unit UDalinEngine;

interface

uses

  UFormBroker
  ;

type
  TDalinEngine = class
  private
    FFormBroker: TFormBroker;
  public
    constructor Create;
    destructor  Destroy; override;

    property FormBroker: TFormBroker read FFormBroker;
  end;

implementation

{ TDalinCore }

constructor TDalinEngine.Create;
begin
  FFormBroker := TFormBroker.Create;
end;

destructor TDalinEngine.Destroy;
begin
  FFormBroker.Free;
  inherited;
end;

end.
