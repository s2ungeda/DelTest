unit FJungKopi;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs

  , UStorage, Vcl.Grids, Vcl.ExtCtrls
  ;

type
  TFrmJungKopi = class(TForm)
    sgVal: TStringGrid;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure sgValDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect;
      State: TGridDrawState);
  private
    { Private declarations }
    FPrecision : integer;
    FCol, FRow : integer;
  public
    { Public declarations }
    procedure SaveEnv( aStorage : TStorage );
    procedure LoadEnv( aStorage : TStorage );
  end;

var
  FrmJungKopi: TFrmJungKopi;

implementation

uses
  GApp  , GLibs , UConsts
  , UApiTypes
  ;

{$R *.dfm}

{ TFrmJungKopi }

procedure TFrmJungKopi.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Timer1.Enabled := false;
  Action := caFree;
end;

procedure TFrmJungKopi.FormCreate(Sender: TObject);
var
  I, iMod: Integer;
  s : string;
begin
  with sgVal do
    for I := 0 to ColCount-1 do
    begin
      if i = 0 then
        ColWidths[i] := 60
      else
        ColWidths[i] := 35;

      if i > 0  then  begin
        iMod := i Mod 2;
        if iMod = 0 then
          s := '30'
        else
          s := '00';

        Cells[i, 0 ]  := Format('%d:%s', [ (i-1) div 2, s] );
        Cells[i, 5 ] := Format('%d:%s', [ (i-1) div 2 + 12, s ]);

//        cells[i,1]    := intToStr( i-1 );
//        cells[i,3]    := intToStr( (i-1)+24 );
      end;
    end;

  with sgVal do
  begin
    Cells[0,0]  := '거래소';
    Cells[0,1]  := 'UB 중코피';
    Cells[0,2]  := 'BT 중코피';
    Cells[0,5]  := 'RealTime';
  end;

  FCol  := -1;
  FRow  := -1;

  FPrecision:= App.GetPrecision;
end;

procedure TFrmJungKopi.FormDestroy(Sender: TObject);
begin
  //
end;

procedure TFrmJungKopi.LoadEnv(aStorage: TStorage);
begin

end;

procedure TFrmJungKopi.SaveEnv(aStorage: TStorage);
begin

end;

procedure TFrmJungKopi.sgValDrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
  var
    stTxt : string;
    aBack , aFont : TColor;
    aRect : TRect;
    dFormat : word;
begin
  aFont   := clBlack;
  dFormat := DT_CENTER ;
  aRect   := Rect;
  aBack   := clWhite;

  with sgVal do
  begin

    stTxt := Cells[ ACol, ARow];

    if (ARow = 0 ) or  ( ARow = RowCount-1 ) then
    begin
      aBack := clBtnFace;
    end else
    begin

    end;

    Canvas.Font.Name    := '나눔고딕';
    Canvas.Font.Size    := 9;
    Canvas.Font.Color   := aFont;
    Canvas.Brush.Color  := aBack;

    aRect.Top := Rect.Top + 4;
    Canvas.FillRect( Rect);
    DrawText( Canvas.Handle, PChar( stTxt ), Length( stTxt ), aRect, dFormat );

    if ( ARow = 0 ) or  ( ARow = RowCount-1 ) then begin
      Canvas.Pen.Color := clBlack;
      Canvas.PolyLine([Point(Rect.Left,  Rect.Bottom),
                       Point(Rect.Right, Rect.Bottom),
                       Point(Rect.Right, Rect.Top)]);
      Canvas.Pen.Color := clWhite;
      Canvas.PolyLine([Point(Rect.Left,  Rect.Bottom),
                       Point(Rect.Left,  Rect.Top),
                       Point(Rect.Right, Rect.Top)]);
    end;
  end;
end;

procedure TFrmJungKopi.Timer1Timer(Sender: TObject);
var
  i  : integer;

  function GetString( a : double ) : string;
  begin
    if (a < DOUBLE_EPSILON) and ( a > (DOUBLE_EPSILON  * -1 ))  then
      Result := ''
    else
      Result := Format('%.*n', [ FPrecision,  a ] );
  end;

begin
  with sgVal do
  begin
    Cells[0, 3] := Format('%.*n', [ FPrecision, App.Engine.SymbolCore.MainKimp[ekUpbit] ]);
    Cells[0, 4] := Format('%.*n', [ FPrecision, App.Engine.SymbolCore.MainKimp[ekBithumb] ]);

    for I := 1 to ColCount-1 do
    begin
      Cells[i,1] := GetString(App.Engine.SymbolCore.JungKopi[ekUpbit][i-1 ] );
      Cells[i,2] := GetString(App.Engine.SymbolCore.JungKopi[ekBithumb][i-1] ) ;

      Cells[i,3] := GetString(App.Engine.SymbolCore.JungKopi[ekUpbit][i-1 + 24 ] );
      Cells[i,4] := GetString(App.Engine.SymbolCore.JungKopi[ekBithumb][i-1 + 24 ] );
    end;

//    for I := 0 to High( App.Engine.SymbolCore.JungKopi[ekBinance]) do
//    begin
//
//    end;

  end;
end;

end.
