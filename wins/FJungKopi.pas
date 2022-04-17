unit FJungKopi;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs

  , UStorage, Vcl.Grids, Vcl.ExtCtrls
  , UTypes
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
        ColWidths[i] := 45;

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
    Cells[0,0]  := '°Å·¡¼Ò';
    Cells[0,1]  := 'Upbit';
    Cells[0,2]  := 'Bithumb';
    Cells[0,5]  := 'RealTime';
  end;

  FCol  := -1;
  FRow  := -1;

  sgVal.Canvas.Font.Name := 'Arial';
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
    dTmp : double;
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
      if (ACol = 0 ) and ( ARow in [3..4] ) then
        aFont := GetColor( StrToFloatDef( stTxt, 0 ) )
      else if ( ACol = FCol ) and ( ARow in [ FRow..FRow+1]) then
        aFont := GetColor( StrToFloatDef( stTxt, 0 ) );
    end;

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
  iCol, i  : integer;


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

    with App.Engine.SymbolCore do
      iCol := JKIdx[ekUpbit];

    FRow := 1;
    FCol := iCol + 1;
    if iCol > 23 then begin
      FRow := 3;
      FCol := iCol - 24 + 1;
    end;

  end;
end;

end.
