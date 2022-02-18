unit FPriceTable;
interface
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.Buttons,
  Vcl.Grids, Vcl.ValEdit, Vcl.CategoryButtons

  , UStorage
  ;
type
  TFrmPriceTable = class(TForm)
    StatusBar1: TStatusBar;
    plLeft: TPanel;
    plLeftTop: TPanel;
    SpeedButtonRightPanel: TSpeedButton;
    plLeftClient: TPanel;
    sgKimp: TStringGrid;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure sgKimpDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect;
      State: TGridDrawState);
  private
    { Private declarations }
    procedure initControls;
  public
    { Public declarations }
    procedure SaveEnv( aStorage : TStorage );
    procedure LoadEnv( aStorage : TStorage );
  end;
var
  FrmPriceTable: TFrmPriceTable;
implementation
{$R *.dfm}
uses
  UTableConsts ,
  UConsts
  ;
{ TFrmPriceTable }

procedure TFrmPriceTable.FormCreate(Sender: TObject);
begin
  initControls;
end;

procedure TFrmPriceTable.initControls;
var
  i : integer;
begin

  for I := 0 to prcTbl1_TitleCnt - 1 do
  begin
    sgKimp.Cells[i,0] := prcTbll1_Title[i];
//    sgInOut.Cells[i,0]:= prcTbll1_Title[i];
  end;

//  for I := 0 to prcTbl2_TitleCnt - 1 do
//  begin
//    sgQuote.Cells[i,0] := prcTbll2_Title[i];
//
//  end;

end;

procedure TFrmPriceTable.LoadEnv(aStorage: TStorage);
begin

end;

procedure TFrmPriceTable.SaveEnv(aStorage: TStorage);
begin

end;

procedure TFrmPriceTable.sgKimpDrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
  var
    stTxt : string;
    aRect : TRect;
    aFont, aBack : TColor;
    dFormat : Word;
    iMode : integer;
begin

  aFont   := clBlack;
  dFormat := DT_CENTER or DT_VCENTER;
  aRect   := Rect;
  aBack   := clWhite;

  with sgKimp do
  begin

    if ARow = 0 then
    begin
      aBack := clBtnFace;
    end else
    begin

      iMode := (ARow-1) div 3;
      if iMode mod 2 = 0 then
        aBack := GRID_REVER_COLOR;

      stTxt := InttoStr( iMode );

    end;
    Canvas.Font.Color   := aFont;
    Canvas.Brush.Color  := aBack;
    Canvas.FillRect( Rect);


    DrawText( Canvas.Handle, PChar( stTxt ), Length( stTxt ), aRect, dFormat );
  end;
end;

procedure TFrmPriceTable.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TFrmPriceTable.FormDestroy(Sender: TObject);
begin
  //
end;

end.
