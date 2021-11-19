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
    procedure Kimp1Click(Sender: TObject);
    procedure nExchangeClick(Sender: TObject);
    procedure DataModuleCreate(Sender: TObject);
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

  DalinMain, FPriceTable
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
begin

end;

procedure TDataModule1.FormOpen(iFormID, iVar: Integer; var aForm: TForm);
begin
  case iFormID of
    ID_KIMP_TABLE  : aForm := TFrmPriceTable.Create( FrmDalin );

    ID_QUOTE_MONITOR :
      begin

      end;
  end;
end;

procedure TDataModule1.FormReLoad(iFormID: integer; aForm: TForm);
begin

end;

procedure TDataModule1.FormSave(iFormID: Integer; aStorage: TStorage;
  aForm: TForm);
begin

end;

// Quote wins open
procedure TDataModule1.Kimp1Click(Sender: TObject);
begin
  //
  if (Sender = nil) or not (Sender is TComponent) then Exit;

  case (Sender as TComponent).Tag of
    0 : App.Engine.FormBroker.Open(ID_KIMP_TABLE, 0);
   // 1 : App.Engine.FormBroker.Open(ID_QUOTE_MONITOR, 0);
  end;


end;

// Exchange & Account wins open
procedure TDataModule1.nExchangeClick(Sender: TObject);
begin
  if (Sender = nil) or not (Sender is TComponent) then Exit;


  case (Sender as TComponent).Tag of
    0 : ;
    1 : ;
    2 : ;
  end;

end;

end.
