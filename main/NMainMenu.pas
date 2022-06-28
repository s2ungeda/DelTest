unit NMainMenu;

interface

uses
  System.SysUtils, System.Classes,
  Vcl.Forms,  Vcl.Menus,

  UStorage

  ;

type
  TDataModule1 = class(TDataModule)
    MainMenu1: TMainMenu;
    nFile: TMenuItem;
    nAccount: TMenuItem;
    nOrder: TMenuItem;
    nQuote: TMenuItem;
    Kimp1: TMenuItem;
    N6: TMenuItem;
    N1: TMenuItem;
    nExchange: TMenuItem;
    N2: TMenuItem;
    MainWDC1: TMenuItem;
    test2: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
    N5: TMenuItem;
    procedure Kimp1Click(Sender: TObject);
    procedure nExchangeClick(Sender: TObject);
    procedure DataModuleCreate(Sender: TObject);
    procedure N5Click(Sender: TObject);

  private
    { Private declarations }
    procedure FormLoad(iFormID: Integer; aStorage: TStorage; var aForm: TForm);
    procedure FormOpen(iFormID, iVar: Integer; var aForm: TForm);
    procedure FormReLoad(iFormID: integer; aForm: TForm);
    procedure FormSave(iFormID: Integer; aStorage: TStorage; aForm: TForm);
  public
    { Public declarations }
  end;

var
  DataModule1: TDataModule1;

implementation

uses
  GApp, GAppForms ,

  DalinMain, FPriceTable, FJungKopi , FDnwStates, FQuoteMonitors,
  FRepresentWDC, FDataTest , FOrderList, UNormalOrder
  ;

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

// Quote 메뉴 처리
procedure TDataModule1.DataModuleCreate(Sender: TObject);
begin
  App.Engine.FormBroker.OnOpen := FormOpen;
  App.Engine.FormBroker.OnSave := FormSave;
  App.Engine.FormBroker.OnLoad := FormLoad;
  App.Engine.FormBroker.OnReLoad := FormReLoad;
end;

procedure TDataModule1.FormLoad(iFormID: Integer; aStorage: TStorage;
  var aForm: TForm);
var
  bForm : TForm;
begin

  if iFormID = ID_DALIN_MAIN then
  begin
    if FrmDalinMain <> nil then
      FrmDalinMain.LoadEnv( aStorage );
    Exit;
  end;


  FormOpen(iFormID, 0, aForm);
    //
  if aForm = nil then Exit;

  case iFormID of
    ID_KIMP_TABLE :
      if aForm is TFrmPriceTable then
        (aForm as TFrmPriceTable).LoadEnv( aStorage );
    ID_JUNG_KOPI :
      if aForm is TFrmJungKopi then
        (aForm as TFrmJungKopi).LoadEnv( aStorage );
    ID_DNW_STATE :
      if aForm is TFrmDnwStates then
        (aForm as TFrmDnwStates).LoadEnv( aStorage );

    ID_QUOTE_MONITOR :
      if aForm is TFrmQuoteMonitors then
        (aForm as TFrmQuoteMonitors).LoadEnv( aStorage );

    ID_RPRSNT_WDC :
      if aForm is TFrmRprsntWDC then
        (aForm as TFrmRprsntWDC).LoadEnv( aStorage);

    //
    ID_ORDER_LIST :
    	if aForm is TFrmOrderList then
      	(aForm as TFrmOrderList).LoadEnv( aStorage );
    
    ID_ORDER :
      if aForm is TFrmNormalOrder then
        (aForm as TFrmNormalOrder).LoadEnv( aStorage );
  end;

end;

procedure TDataModule1.FormSave(iFormID: Integer; aStorage: TStorage;
  aForm: TForm);
begin
  if aForm = nil then Exit;
    //
  case iFormID of
    ID_KIMP_TABLE :
      if aForm is TFrmPriceTable then
        (aForm as TFrmPriceTable).SaveEnv( aStorage );
    ID_DALIN_MAIN :
      if FrmDalinMain <> nil then
        FrmDalinMain.SaveEnv(aStorage);
    ID_JUNG_KOPI :
      if aForm is TFrmJungKopi then
        (aForm as TFrmJungKopi).SaveEnv( aStorage );
    ID_DNW_STATE :
      if aForm is TFrmDnwStates then
        (aForm as TFrmDnwStates).SaveEnv( aStorage );
    ID_QUOTE_MONITOR :
      if aForm is TFrmQuoteMonitors then
        (aForm as TFrmQuoteMonitors).SaveEnv( aStorage );
    ID_RPRSNT_WDC :
      if aForm is TFrmRprsntWDC then
        (aForm as TFrmRprsntWDC).SaveEnv( aStorage);

    ID_ORDER_LIST :
    	if aForm is TFrmOrderList then
      	(aForm as TFrmOrderList).SaveEnv( aStorage );

    ID_ORDER :
      if aForm is TFrmNormalOrder then
        (aForm as TFrmNormalOrder).SaveEnv( aStorage );
  end;
end;

procedure TDataModule1.FormOpen(iFormID, iVar: Integer; var aForm: TForm);
begin
  aForm := nil;

  case iFormID of
    ID_KIMP_TABLE  		: aForm := TFrmPriceTable.Create( FrmDalinMain );
    ID_JUNG_KOPI  		: aForm := TFrmJungKopi.Create( FrmDalinMain );
    ID_DNW_STATE 			: aForm := TFrmDnwStates.Create( FrmDalinMain );
    ID_QUOTE_MONITOR 	: aForm := TFrmQuoteMonitors.Create( FrmDalinMain );
    ID_RPRSNT_WDC 		: aForm := TFrmRprsntWDC.Create( FrmDalinMain);
    ID_TEST 					: aForm := TFrmTest.Create( FrmDalinMain );
    ID_ORDER_LIST 		: aForm := TFrmOrderList.Create( FrmDalinMain );
    ID_ORDER          : aForm := TFrmNormalOrder.Create( FrmDalinMain );
  end;
end;

procedure TDataModule1.FormReLoad(iFormID: integer; aForm: TForm);
begin

end;



// Quote wins open
procedure TDataModule1.Kimp1Click(Sender: TObject);
begin
  //
  if (Sender = nil) or not (Sender is TComponent) then Exit;

  case (Sender as TComponent).Tag of
    0 : App.Engine.FormBroker.Open(ID_KIMP_TABLE, 0);
    1 : App.Engine.FormBroker.Open(ID_QUOTE_MONITOR, 0);
    2 : App.Engine.FormBroker.Open(ID_JUNG_KOPI, 0);
    3 : App.Engine.FormBroker.Open(ID_RPRSNT_WDC, 0);
    4 : App.Engine.FormBroker.Open(ID_TEST, 0);
  end;


end;

procedure TDataModule1.N5Click(Sender: TObject);
begin
  if (Sender = nil) or not (Sender is TComponent) then Exit;      

  case (Sender as TComponent).Tag of
    0 : App.Engine.FormBroker.Open(ID_ORDER, 0);
    1 : App.Engine.FormBroker.Open(ID_ORDER_LIST, 0);
    2 : ;
  end;
end;

// Exchange & Account wins open
procedure TDataModule1.nExchangeClick(Sender: TObject);
begin
  if (Sender = nil) or not (Sender is TComponent) then Exit;


  case (Sender as TComponent).Tag of
    0 :App.Engine.FormBroker.Open(ID_DNW_STATE, 0);
    1 : ;
    2 : ;
  end;

end;



end.
