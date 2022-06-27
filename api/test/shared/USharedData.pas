unit USharedData;

interface

uses
  System.Classes
  ;

const
  DATA_SIZE = 1024 * 100;
  Q_SIZE = 5;

type

  TDataItem = packed record
    ex     : char;
    market : char;
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
