unit UApiManager;

interface

type

  TApiManager = class
  public
    Constructor Create;
    Destructor  Destroy; override;

  end;

implementation

{ TApiManager }

constructor TApiManager.Create;
begin

end;

destructor TApiManager.Destroy;
begin

  inherited;
end;

end.
