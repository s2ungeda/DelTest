unit USharedData;

interface

uses
  System.Classes  ,

  USharedConsts
  ;

type

  TDataItem = packed record
    exKind : char;      // B(binance), U(upbit), T(bithumb)
    market : char;      // S(Spot), F(futures)
    trDiv  : char;      // N(new order), C ( cancel order), L( 주문조회) P ( 포지션조회 ) B ( 잔고조회 )
    data   : array [0..DATA_SIZE-1] of ansichar;
    size   : array [0..4] of ansichar;
  end;

  PSharedData = ^TSharedData;
  TSharedData = record
    SharedData : array [0..Q_SIZE-1] of TDataItem;
    Front, Rear : integer;
    function Count : integer;
    function IsEmpty : boolean;
    procedure init;
    function IsFull: boolean;
  end;

  TSharedDataNotify = procedure ( aData : TDataItem ) of Object;

implementation

{ TSharedData }

function TSharedData.Count: integer;
begin
  Result :=((Rear - Front + 1) + Q_Size) mod Q_Size;
end;

procedure TSharedData.init;
begin
  Front := 0;
  Rear  := 0;
end;

function TSharedData.IsEmpty: boolean;
begin
  Result := Front = Rear;
end;

function TSharedData.IsFull: boolean;
begin
  Result := ( Rear + 1 ) mod Q_SIZE = Front;
end;


end.
