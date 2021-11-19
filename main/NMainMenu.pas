unit NMainMenu;

interface

uses
  System.SysUtils, System.Classes, Vcl.Menus;

type
  TDataModule1 = class(TDataModule)
    MainMenu1: TMainMenu;
    nFile: TMenuItem;
    nAccount: TMenuItem;
    nOrder: TMenuItem;
    nQuote: TMenuItem;
    Kimp1: TMenuItem;
    N5: TMenuItem;
    N6: TMenuItem;
    N1: TMenuItem;
    procedure Kimp1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DataModule1: TDataModule1;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

// Quote 메뉴 처리
procedure TDataModule1.Kimp1Click(Sender: TObject);
begin
  //
  if (Sender = nil) or not (Sender is TComponent) then Exit;


  case (Sender as TComponent).Tag of
    0 : ;
    1 : ;
    2 : ;
  end;


end;

end.
