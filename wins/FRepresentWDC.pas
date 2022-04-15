unit FRepresentWDC;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Grids, Vcl.ExtCtrls

  , UStorage
  ;

type
  TFrmRprsntWDC = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    sgWDC2: TStringGrid;
    sgWDC1: TStringGrid;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure sgWDC1DrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect;
      State: TGridDrawState);
    procedure Timer1Timer(Sender: TObject);
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
  FrmRprsntWDC: TFrmRprsntWDC;

implementation

uses
  GApp  , GLibs , UConsts
  , UApiTypes
  ;

{$R *.dfm}

{ TFrmRprsntWDC }

procedure TFrmRprsntWDC.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Timer1.Enabled := false;
  Action := caFree;
end;

procedure TFrmRprsntWDC.FormCreate(Sender: TObject);
var
  I, iMod: Integer;
  s : string;
begin

  with sgWDC2 do
    for I := 0 to ColCount-1 do
    begin
      if i = 0 then begin
        ColWidths[i] := 60;
        sgWDC1.ColWidths[i] := 60;
      end
      else begin
        ColWidths[i] := 40;
        sgWDC1.ColWidths[i] := 45;
      end;

      if i > 0  then  begin
        iMod := i Mod 2;
        if iMod = 0 then
          s := '30'
        else
          s := '00';

        sgWDC1.Cells[i,0] := Format('T-%d',[ 23-i]);

        Cells[i, 0 ]  := Format('%d:%s', [ (i-1) div 2, s] );
        Cells[i, 5 ] := Format('%d:%s', [ (i-1) div 2 + 12, s ]);
      end;
    end;
  with sgWDC1 do
  begin
    Cells[0,1]  := 'Upbit';
    Cells[0,2]  := 'Bithumb';
  end;

  with sgWDC2 do
  begin
    Cells[0,0]  := '°Å·¡¼Ò';
    Cells[0,1]  := 'Upbit';
    Cells[0,2]  := 'Bithumb';
    Cells[0,5]  := 'RealTime';
  end;

  FCol  := -1;
  FRow  := -1;

  sgWDC2.Canvas.Font.Name := 'Arial';
  FPrecision:= App.GetPrecision;

end;

procedure TFrmRprsntWDC.LoadEnv(aStorage: TStorage);
begin

end;

procedure TFrmRprsntWDC.SaveEnv(aStorage: TStorage);
begin

end;

procedure TFrmRprsntWDC.sgWDC1DrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
  var
    stTxt : string;
    aBack , aFont : TColor;
    aRect : TRect;
    dFormat : word;
    dTmp : double;
    bPoly : boolean;
begin
  aFont   := clBlack;
  dFormat := DT_CENTER ;
  aRect   := Rect;
  aBack   := clWhite;
  with Sender as TStringGrid do
  begin

    stTxt := Cells[ ACol, ARow];

    if (ARow = 0 )  then
      aBack := clBtnFace
    else if  ( ARow = RowCount-1 ) and ( Tag = 1) then
      aBack := clBtnFace
    else  begin
      if Tag = 1 then begin
        if (ACol = 0 ) and ( ARow in [3..4] ) then
          aFont := clGreen
        else if ( ACol = FCol ) and ( ARow in [ FRow..FRow+1]) then
          aFont := clGreen;
      end;
    end;

    Canvas.Font.Color   := aFont;
    Canvas.Brush.Color  := aBack;

    aRect.Top := Rect.Top + 4;
    Canvas.FillRect( Rect);
    DrawText( Canvas.Handle, PChar( stTxt ), Length( stTxt ), aRect, dFormat );

    bPoly := false;
    if (( ARow = 0 ) or ( ARow = RowCount-1 )) and (Tag=1) then begin
      bPoly := true;
    end else
    if ( ARow = 0 ) and ( Tag = 0 ) then
      bPoly := true;

    if bPoly then
    begin
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

procedure TFrmRprsntWDC.Timer1Timer(Sender: TObject);
var
  iCol, i  : integer;



  function GetString( a : double ) : string;
  begin
    if (a < DOUBLE_EPSILON) and ( a > (DOUBLE_EPSILON  * -1 ))  then
      Result := ''
    else
      Result := Format('%.*n', [ 1,  a ] );
  end;

begin
  with sgWDC2 do
  begin
    Cells[0, 3] := Format('%.*n', [ 1, App.Engine.SymbolCore.MainWDC[ekUpbit] ]);
    Cells[0, 4] := Format('%.*n', [ 1, App.Engine.SymbolCore.MainWDC[ekBithumb] ]);

    for I := 1 to ColCount-1 do
    begin
      Cells[i,1] := GetString(App.Engine.SymbolCore.RprsntWDC[ekUpbit][i-1 ] );
      Cells[i,2] := GetString(App.Engine.SymbolCore.RprsntWDC[ekBithumb][i-1] ) ;

      Cells[i,3] := GetString(App.Engine.SymbolCore.RprsntWDC[ekUpbit][i-1 + 24 ] );
      Cells[i,4] := GetString(App.Engine.SymbolCore.RprsntWDC[ekBithumb][i-1 + 24 ] );
    end;

    with App.Engine.SymbolCore do
      iCol := WDCIdx[ekUpbit];

    FRow := 1;
    FCol := iCol + 1;
    if iCol > 23 then begin
      FRow := 3;
      FCol := iCol - 24 + 1;
    end;

  end;
end;

end.
