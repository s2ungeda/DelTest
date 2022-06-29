unit URestManager;

interface

uses
  System.Classes, System.SysUtils,

  System.JSON,  Rest.Json , REST.Client,  Rest.Types,

  UApiTypes
  ;

type

  TRestManager = class
  public
     Constructor Create( bn, up, bt : TRESTRequest );
     Destructor  Destroy; override;
  end;

implementation

{ TRestManager }

constructor TRestManager.Create(bn, up, bt: TRESTRequest);
begin

end;

destructor TRestManager.Destroy;
begin

  inherited;
end;

end.
