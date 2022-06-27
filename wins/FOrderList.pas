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
    PopupMenu1: TPopupMenu;
    N1: TMenuItem;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure sgOrderDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect;
      State: TGridDrawState);
    procedure ComboBox1Change(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure sgOrderMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
    FExIndex : integer;
    FState   : array [0..4] of boolean;
    FRow     : integer;

    //FStIndex : integer;
    procedure initControls;
    procedure DoOrder(aOrder: TOrder; EventID: TDistributorID);
    function Fillter(aOrder: TOrder): boolean;
    procedure AddOrder(aOrder: TOrder);
    procedure DelOrder(aOrder: TOrder);
    procedure UpdateOrder(aOrder: TOrder);
    procedure PutData(aOrder: TOrder; iRow: integer); overload;
    procedure PutData( var iCol, iRow : integer; sData : string); overload;
    procedure UpdateData;
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
  FState[0] := true;
  FState[1] := true;
  Fstate[2] := false;    Fstate[3] := false;     Fstate[4] := false;
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

  FRow := -1;
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
procedure TFrmOrderList.sgOrderMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
  var
    ACol : integer;
    aOrder : TOrder;
begin

  sgOrder.MouseToCell(X,Y, ACol, FRow);

  if Button = mbRight then
  begin
    aOrder  := TOrder( sgOrder.Objects[ORD_COL, FRow] );
    if ( aOrder <> nil ) and ( aOrder.State = osActive ) and ( not aORder.Modify ) then
      sgOrder.PopupMenu := PopupMenu1;
  end;

  sgOrder.Invalidate;

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

  if ( sgOrder.RowCount > 1) and ( sgOrder.FixedRows <= 0 ) then
    sgOrder.FixedRows := 1;
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

procedure TFrmOrderList.CheckBox1Click(Sender: TObject);
var
  iTag : integer;
begin
  iTag := TCheckBox(Sender).Tag ;
  FState[iTag]  := TCheckBox(Sender).Checked;

  UpdateData;
end;

procedure TFrmOrderList.ComboBox1Change(Sender: TObject);
begin
  FExIndex := ComboBox1.ItemIndex;
end;

procedure TFrmOrderList.ComboBox2Change(Sender: TObject);
begin
  //
//  FStIndex  := ComboBox2.ItemIndex ;
end;

function TFrmOrderList.Fillter( aOrder : TOrder ) : boolean;
begin
  case FExIndex of
    0 : Result := true;
    else  Result := aOrder.Account.ExchangeKind = TExchangeKind( FExIndex + 1 );
  end;
  if not Result then Exit;

  case aOrder.State of
    osReady,  osSent,   osSrvAcpt : Result := FState[4] ;
    osSrvRjt, osFailed, osRejected: Result := FState[3] ;
    osActive: Result  := FState[0] ;
    osFilled: Result  := FState[1];
    osCanceled:Result := FState[2] ;
  end;
end;


procedure TFrmOrderList.UpdateData;
var
  aOrder : TOrder;
  i : integer;
begin

  InitGrid( sgOrder, true, 1 );

  for I := 0 to App.Engine.TradeCore.TotalOrders.Count-1 do
  begin
    aOrder  := App.Engine.TradeCore.TotalOrders.Orders[i];
    if not Fillter( aOrder ) then Continue;
    AddOrder( aOrder );
  end;
end;


end.

