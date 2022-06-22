unit FOrderList;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Grids, Vcl.ExtCtrls

  , UApiTypes, UApiConsts
  , UOrders, USymbols
  , UStorage, Vcl.Menus
  
  ;

type
  TFrmOrderList = class(TForm)
    Panel1: TPanel;
    sgOrder: TStringGrid;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    PopupMenu1: TPopupMenu;
    N1: TMenuItem;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure sgOrderDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect;
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
  FrmOrderList: TFrmOrderList;

implementation

uses
	GApp, GLibs
  , UTableConsts
  ;

{$R *.dfm}

procedure TFrmOrderList.FormClose(Sender: TObject; var Action: TCloseAction);
begin
	Action := caFree;
end;

procedure TFrmOrderList.FormCreate(Sender: TObject);
begin
	initControls;
end;

procedure TFrmOrderList.FormDestroy(Sender: TObject);
begin
	//
end;

procedure TFrmOrderList.initControls;
var
  I: integer;
begin
	with sgOrder do
  begin
		ColCount	:= orderList_TitleCnt;	
  	RowCount	:= 1;

    for I := 0 to orderList_TitleCnt-1 do
		begin
    	Cells[i,0]	:= orderList_Title[i];
      ColWidths[i]:= orderList_Width[i];
    end;
    
  end;
end;

procedure TFrmOrderList.LoadEnv(aStorage: TStorage);
begin

end;

procedure TFrmOrderList.SaveEnv(aStorage: TStorage);
begin

end;

procedure TFrmOrderList.sgOrderDrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
  var
  	aRect : TRect;
    aFont, aBack : TColor;    
    dFormat	: WORD;
    stTxt	: string;
begin

  aFont   := clBlack;
  dFormat := DT_CENTER ;
  aRect   := Rect;
  aBack   := clWhite;
  
	with sgOrder do
  begin

  	stTxt := Cells[ ACol, ARow];

  	if ARow = 0 then
    begin
			aBack := clBtnFace;    
    end else
    begin

    	if ACol in [4..7] then
				dFormat := DT_RIGHT;    
    end;
  
    Canvas.Font.Color   := aFont;
    Canvas.Brush.Color  := aBack;

    aRect.Top := Rect.Top + 2;
    if ( ARow > 0 ) and ( dFormat = DT_RIGHT ) then
      aRect.Right := aRect.Right - 2;
    dFormat := dFormat or DT_VCENTER;

    Canvas.FillRect( Rect);
    DrawText( Canvas.Handle, PChar( stTxt ), Length( stTxt ), aRect, dFormat );
  
  end;
end;

end.


