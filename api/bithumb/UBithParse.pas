unit UBithParse;

interface

uses
  System.Classes, System.SysUtils, System.DateUtils
  , System.JSON , USymbols
  , UApiTypes
  ;

type

  TBithParse = class
  public
    constructor Create;
    destructor Destroy; override;

    procedure ParseSpotOrderBook( aData : string );
  end;

var
  gBithReceiver : TBithParse;
implementation

uses
  GApp
  ;

{ TBithParse }

constructor TBithParse.Create;
begin
  gBithReceiver := self;
end;

destructor TBithParse.Destroy;
begin
  gBithReceiver := nil;
  inherited;
end;

procedure TBithParse.ParseSpotOrderBook(aData: string);
var
  aObj, aSub, master : TJsonObject;
  aArr : TJsonArray;
  aPair : TJsonPair;
  aVal  : TJsonValue;
  i : Integer;
  sBase, sCode, sTmp : string;
  aSymbol : TSymbol;
  bNew : boolean;
  j: Integer;
begin
  master := TJsonObject.ParseJSONValue( aData ) as TJsonObject;
  aObj := master.GetValue('data') as TJsonObject;

  for I := 0 to aObj.Size-1 do
  begin
    aPair := aObj.Get(i);
    if aPair.JsonValue.ClassType <> TJSONObject then continue;
    sCode := aPair.JsonString.Value;

    aSymbol := App.Engine.SymbolCore.FindSymbol(ekBithumb, sCode );
    if aSymbol <> nil then
    begin
      aSub  := aPair.JsonValue as TJsonObject;
      aArr  := aSub.Get('bids').JsonValue as TJsonArray;

      if aArr.Count > 0 then begin
        aVal  := aArr.Get(aArr.Count-1);
        aSymbol.Bids[0].Price   := StrToFloatDef( aVal.GetValue<string>( 'price' ), 0.0 );
        aSymbol.Bids[0].Volume  := StrToFloatDef( aVal.GetValue<string>( 'quantity' ), 0.0 );
      end;

      aArr  := aSub.Get('asks').JsonValue as TJsonArray;

      if aArr.Count > 0 then
      begin
        aVal  := aArr.Get(aArr.Count-1);
        aSymbol.Asks[0].Price   := StrToFloatDef( aVal.GetValue<string>( 'price' ), 0.0 );
        aSymbol.Asks[0].Volume  := StrToFloatDef( aVal.GetValue<string>( 'quantity' ), 0.0 );
      end;
    end;

  end;
  //  if ( GetCodeIndex( sCode ) < 0 ) then Continue;

end;


end.
