unit UUpbitParse;

interface

uses
  System.Classes, System.SysUtils, System.DateUtils
  , System.JSON , USymbols
  , UApiTypes
  ;

type

  TUpbitParse = class
  public
    constructor Create;
    destructor Destroy; override;

    procedure ParseSpotTicker( aData : string );
    procedure ParseSocketData( aMarket : TMarketType; aData : string);
  end;

var
  gUpReceiver : TUpbitParse;


implementation

uses
  GApp
  ;

{ TUpbitParse }

constructor TUpbitParse.Create;
begin
  gUpReceiver := self;
end;

destructor TUpbitParse.Destroy;
begin

  gUpReceiver := nil;
  inherited;
end;

procedure TUpbitParse.ParseSocketData(aMarket: TMarketType; aData: string);
begin

end;

procedure TUpbitParse.ParseSpotTicker(aData: string);
var
  aArr : TJsonArray;
  aVal : TJsonValue;
  i : integer;
  sCode , sTmp : string;
  aSymbol : TSymbol;
  bNew : boolean;
  sts   : TArray<string>;
begin
  if aData = '' then
  begin
    App.Log(llError, 'Upbit ParseSpotTicker data is empty') ;
    Exit;
  end;

  aArr := TJsonObject.ParseJSONValue( aData) as TJsonArray;

  for I := 0 to aArr.Size-1 do
  begin
    aVal := aArr.Get(i);
    sCode:= aVal.GetValue<string>('market');

    sts := sCode.Split(['-']);
    aSymbol := App.Engine.SymbolCore.FindSymbol(ekUpbit, sts[1]);

    if aSymbol = nil then
    begin
      bNew := true;
      aSymbol := App.Engine.SymbolCore.RegisterSymbol(ekUpbit, mtSpot, sts[1] );
      if aSymbol = nil then Exit;
    end else
      bNew := false;

    with aSymbol do
    begin
      OrgCode     := sCode;
      Spec.BaseCode    := sts[1];   // BTC ...
      Spec.QuoteCode   := sts[0];   // KRW ...
      Spec.SettleCode  := sts[0];
    end;

    aSymbol.DayOpen := StrToFloatDef( aVal.GetValue<string>( 'opening_price' ), 0.0 );
    aSymbol.DayHigh := StrToFloatDef( aVal.GetValue<string>( 'high_price' ), 0.0 );
    aSymbol.DayLow  := StrToFloatDef( aVal.GetValue<string>( 'low_price' ), 0.0 );
    aSymbol.Last    := StrToFloatDef( aVal.GetValue<string>( 'trade_price' ), 0.0 );
    aSymbol.PrevClose   := StrToFloatDef( aVal.GetValue<string>( 'prev_closing_price' ), 0.0 );
    aSymbol.DayAmount   := StrToFloatDef( aVal.GetValue<string>( 'acc_trade_price_24h' ), 0.0 );
    aSymbol.DayVolume   := StrToFloatDef( aVal.GetValue<string>( 'acc_trade_volume_24h' ), 0.0 );

//    aSymbol.Time  := UnixToDateTime(  aVal.GetValue<int64>( 'date' ) );
    aSymbol.LocalTime := now;

    if bNew then
      App.Engine.SymbolCore.RegisterSymbol( ekUpbit, aSymbol );

  end;
end;

end.
