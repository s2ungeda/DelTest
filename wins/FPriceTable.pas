unit FPriceTable;
interface
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.Buttons,
  Vcl.Grids, Vcl.ValEdit, Vcl.CategoryButtons

  , UStorage, USymbols
  , UApiTypes
  ;
type
  TFrmPriceTable = class(TForm)
    StatusBar1: TStatusBar;
    plLeft: TPanel;
    plLeftTop: TPanel;
    plLeftClient: TPanel;
    sgKimp: TStringGrid;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure sgKimpDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect;
      State: TGridDrawState);
  private
    FFontSize: integer;
    { Private declarations }
    procedure initControls;
    procedure InitObject;
    procedure UpdateSymbol(aSymbol: TSymbol; iRow : integer);
    procedure SetFontSize(const Value: integer);


  public
    { Public declarations }
    procedure SaveEnv( aStorage : TStorage );
    procedure LoadEnv( aStorage : TStorage );

    property FontSize : integer read FFontSize  write SetFontSize;
  end;
var
  FrmPriceTable: TFrmPriceTable;
implementation
{$R *.dfm}
uses
  GApp
  , UTableConsts
  , UConsts , UApiConsts
  ;
{ TFrmPriceTable }

procedure TFrmPriceTable.FormCreate(Sender: TObject);
begin
  initControls;
  InitObject;
end;

procedure TFrmPriceTable.initControls;
var
  i : integer;
begin

  for I := 0 to prcTbl1_TitleCnt - 1 do
  begin
    sgKimp.Cells[i,0] := prcTbll1_Title[i];
    if i = 0 then
      sgKimp.RowHeights[i] := 20;
    sgKimp.ColWidths[i] :=  prcTbll1_Width[i];
//    sgInOut.Cells[i,0]:= prcTbll1_Title[i];
  end;

  FontSize := 9;

//  for I := 0 to prcTbl2_TitleCnt - 1 do
//  begin
//    sgQuote.Cells[i,0] := prcTbll2_Title[i];
//
//  end;

end;


procedure TFrmPriceTable.InitObject;
var
  i, iRow : integer;
  j : TExchangeKind;
  aSymbol : TSymbol;
begin
  for I := 0 to High(TMajorSymbol) do

    for j := ekBinance to High(TExchangeKind) do
    begin
      aSymbol := App.Engine.SymbolCore.FindQuoteSymbol( j, TMajorSymbol[i] );
      if aSymbol <> nil then
      begin
        iRow := GetMajorRow(i ) + integer(j) ;
        sgKimp.Objects[CoinCol, iRow ] := aSymbol;
        if j = ekUpbit then   sgKimp.Cells[CoinCol, iRow ] := TMajorSymbol[i];
        sgKimp.Cells[ExCol,   iRow ] := TExchangeKindShortDesc[j];

        UpdateSymbol( aSymbol, iRow );
      end;
    end;

end;

procedure TFrmPriceTable.UpdateSymbol( aSymbol : TSymbol; iRow : integer );
begin
  if aSymbol <> nil then

  with sgKimp do
  begin
    Cells[ CurCol - 4, iRow] := Format('%*.n', [ 0, aSymbol.Asks[0].Volume ]);
    Cells[ CurCol - 3, iRow] := Format('%*.n', [ 0, aSymbol.Asks[0].Price ]);
    Cells[ CurCol - 2, iRow] := Format('%*.n', [ 0, aSymbol.Bids[0].Price ]);
    Cells[ CurCol - 1, iRow] := Format('%*.n', [ 0, aSymbol.Bids[0].Volume ]);
  end;
end;

procedure TFrmPriceTable.LoadEnv(aStorage: TStorage);
begin

end;

procedure TFrmPriceTable.SaveEnv(aStorage: TStorage);
begin

end;

procedure TFrmPriceTable.SetFontSize(const Value: integer);
begin
  FFontSize := Value;
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
  dFormat := DT_CENTER ;
  aRect   := Rect;
  aBack   := clWhite;

  with sgKimp do
  begin

   {
  SELECTED_FONT_COLOR = $000000;

  GRID_SELECT_COLOR = $F0F0F0;
  GRID_REVER_COLOR  = $00EEEEEE;
  FUND_FORM_COLOR   = $00D8E5EE;

  DISABLED_COLOR  = $BBBBBB;
  ERROR_COLOR     = $008080FF;
  ODD_COLOR       = $FFFFFF;
  EVEN_COLOR      = $EEEEEE;
  }

    stTxt := Cells[ ACol, ARow];

    if ARow = 0 then
    begin
      aBack := clBtnFace;
    end else
    begin
      //  0,1,2 ->0    3,4,5 ->1   6,7,8 -> 2
      iMode := (ARow-1) div 3;
      if iMode mod 2 <> 0 then
        aBack := GRID_MOD_COLOR;

      if ACol in [ CurCol-4..CurCol] then
        dFormat := DT_RIGHT

    end;

    Canvas.Font.Name    := '±¼¸²Ã¼';
    Canvas.Font.Size    := FFontSize;
    Canvas.Font.Color   := aFont;
    Canvas.Brush.Color  := aBack;

//    if GetMajorRow( ARow ) > 3  then
//      stTxt := IntTostr(  GetMajorRow( ARow ) );
    aRect.Top := Rect.Top + 4;
    if ( ARow > 0 ) and ( dFormat = DT_RIGHT ) then
      aRect.Right := aRect.Right - 2;
    dFormat := dFormat or DT_VCENTER;

    Canvas.FillRect( Rect);
    DrawText( Canvas.Handle, PChar( stTxt ), Length( stTxt ), aRect, dFormat );

    if ARow = 0 then begin
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

procedure TFrmPriceTable.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TFrmPriceTable.FormDestroy(Sender: TObject);
begin
  //
end;




end.
