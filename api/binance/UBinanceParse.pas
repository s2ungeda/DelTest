unit UBinanceParse;

interface

uses
  System.Classes, System.SysUtils
  , System.JSON
  ;

type
  TBinanceParse = class
  public

    constructor Create;
    destructor Destroy; override;

    procedure ParsePrepareMaster        ;
  end;

var
  gBinReceiver : TBinanceParse;

implementation

{ TBinanceParse }

constructor TBinanceParse.Create;
begin
  gBinReceiver := self;
end;

destructor TBinanceParse.Destroy;
begin
  gBinReceiver := nil;
  inherited;
end;

procedure TBinanceParse.ParsePrepareMaster;
begin

end;

end.
