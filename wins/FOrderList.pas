unit FOrderList;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Grids, Vcl.ExtCtrls

  , UApiTypes, UApiConsts
  , UOrders, USymbols
  , UStorage, Vcl.Menus
  , UDistributor
  ;

const
  ORD_COL = 0;


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
    procedure ComboBox1Change(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
  private
    { Private declarations }
    FExIndex : integer;
    FStIndex : integer;
    procedure initControls;
    procedure DoOrder(aOrder: TOrder; EventID: TDistributorID);
    function Fillter(aOrder: TOrder): boolean;
    procedure AddOrder(aOrder: TOrder);

    procedure DelOrder(aOrder: TOrder);
    procedure UpdateOrder(aOrder: TOrder);
    procedure PutData(aOrder: TOrder; iRow: integer); overload;
    procedure PutData( var iCol, iRow : integer; sData : string); overload;
    { Public declarations }
  public
    procedure SaveEnv( aStorage : TStorage );
    procedure LoadEnv( aStorage : TStorage );

    procedure TradeProc(Sender, Receiver: TObject; DataID: Integer;
      DataObj: TObject; EventID: TDistributorID) ;
    
  end;

var
  FrmOrderList: TFrmOrderList;

implementation

uses
	GApp, GLibs
  , UTableConsts
  , UConsts
  ;

{$R *.dfm}

procedure TFrmOrderList.FormClose(Sender: TObject; var Action: TCloseAction);
begin
	Action := caFree;
end;

procedure TFrmOrderList.FormCreate(Sender: TObject);
begin
	initControls;

  FExIndex  := 0;
  FStIndex  := 1;

  App.Engine.TradeBroker.Subscribe( Self, TradeProc);
end;

procedure TFrmOrderList.FormDestroy(Sender: TObject);
begin
	//
  App.Engine.TradeBroker.Unsubscribe( Self );
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

procedure TFrmOrderList.TradeProc(Sender, Receiver: TObject; DataID: Integer;
  DataObj: TObject; EventID: TDistributorID);
begin
  if (Receiver <> Self) or (DataID <> TRD_DATA) then Exit;

  case Integer(EventID) of
    ORDER_ACCEPTED,
    ORDER_REJECTED,
    ORDER_CANCELED,
    ORDER_FILLED: DoOrder(DataObj as TOrder, EventID);
  end;
end;

procedure TFrmOrderList.DoOrder(aOrder : TOrder;EventID: TDistributorID);
var
  bRes : boolean;
begin
  bRes := Fillter( aOrder );

  if not bRes then
    DelOrder( aOrder )
  else begin
    if aOrder.State = osActive then
      AddOrder( aOrder )
    else
      UpdateOrder( aOrder );
  end;
end;


procedure TFrmOrderList.AddOrder(aOrder : TOrder );
begin
  InsertLine( sgOrder, 1 );
  PutData( aOrder, 1 );
end;

procedure TFrmOrderList.UpdateOrder(aOrder : TOrder );
var
  iRow : integer;
begin
  iRow := sgOrder.Cols[ORD_COL].IndexOfObject(aOrder);
  if iRow >= 0 then
    PutData(aOrder, iRow );
end;

procedure TFrmOrderList.DelOrder(aOrder : TOrder );
var
  iRow : integer;
begin
  iRow := sgOrder.Cols[ORD_COL].IndexOfObject(aOrder);
  if iRow >= 0 then
    DeleteLine(sgOrder, iRow );
end;

procedure TFrmOrderList.PutData( aOrder : TOrder; iRow : integer);
var
  iCol : integer;
begin
  iCol := 0;
  with sgOrder do
  begin
    Objects[ORD_COL, iRow]  := aOrder;
    PutData( iCol, iRow, ExKindToStr( aOrder.Account.ExchangeKind ) );
    PutData( iCol, iRow, '' );
    PutData( iCol, iRow, aOrder.Symbol.Code );
    PutData( iCol, iRow, ifThenStr( aOrder.Side > 0 ,'매수','매도') );
    PutData( iCol, iRow, aOrder.Symbol.PriceToStr( aOrder.Price) );
    PutData( iCol, iRow, aOrder.Symbol.QtyToStr( aOrder.OrderQty) );
    PutData( iCol, iRow, aOrder.Symbol.PriceToStr( aOrder.AvgPrice ) );
    PutData( iCol, iRow, aOrder.Symbol.QtyToStr( aOrder.FilledQty) );
    PutData( iCol, iRow, aOrder.StateToStr );
    PutData( iCol, iRow, FormatDateTime('hh:nn:ss', aOrder.AcptTime) );
    PutData( iCol, iRow, aOrder.OrderNo );
  end;
end;

procedure TFrmOrderList.PutData( var iCol, iRow : integer; sData : string);
begin
  with sgOrder do
  begin
    Cells[ iCol, iRow] := sData;
  end;
  inc( iCol );
end;

procedure TFrmOrderList.ComboBox1Change(Sender: TObject);
begin
  FExIndex := ComboBox1.ItemIndex;
end;

procedure TFrmOrderList.ComboBox2Change(Sender: TObject);
begin
  //
  FStIndex  := ComboBox2.ItemIndex ;
end;

function TFrmOrderList.Fillter( aOrder : TOrder ) : boolean;
begin
  case FExIndex of
    0 : Result := true;
    else  Result := aOrder.Account.ExchangeKind = TExchangeKind( FExIndex + 1 );
  end;
  if not Result then Exit;

  case FStIndex of
    0 : Result := true;
    1 : Result := aOrder.State = osActive;
    2 : Result := aOrder.State = osFilled;
    3 : Result := aORder.State = osCanceled;
    else Result := false;
  end;
end;



end.


