unit FWinConfig;
interface
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls

  , UTypes
  ;
type

  TFrmWinConfig = class(TForm)
    GroupBox1: TGroupBox;
    cbFont: TComboBox;
    Label1: TLabel;
    ButtonCancel: TButton;
    ButtonOK: TButton;
    Label2: TLabel;
    edtSize: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
  private

    procedure initControls;
    { Private declarations }
  public
    { Public declarations }
    function Open( aParam : TWinParam ): Boolean;
    function GetParam : TWinParam;

  end;
var
  FrmWinConfig: TFrmWinConfig;
implementation
uses
  GApp
  , GLibs
  , UConsts
  ;
{$R *.dfm}
procedure TFrmWinConfig.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  ACtion := caFree;
end;

procedure TFrmWinConfig.FormCreate(Sender: TObject);
begin
  initControls;
end;

procedure TFrmWinConfig.FormDestroy(Sender: TObject);
begin
  gWinCfg := nil;
end;

function TFrmWinConfig.GetParam: TWinParam;
begin
  with Result do
  begin
    FontName := cbFont.Items[cbFont.ItemIndex];
    FontSize := StrToInt( edtSize.Text );
  end;
end;

procedure TFrmWinConfig.initControls;
var
  sList: TStringList;
  i: Integer;
begin

  sList:=TStringList.Create;
  try
    EnumFontFamilies(Canvas.Handle, nil, @EnumFamToLines, LongInt(sList));
    sList.Sort;
    for i:=0 to sList.Count-1 do cbFont.Items.Add(sList[i]);
  finally
    FreeAndNil(sList);
  end;

  if cbFont.Items.Count > 0 then
  begin
    cbFont.ItemIndex := 0;
    ComboBox_AutoWidth(cbFont);
  end;
end;

function TFrmWinConfig.Open( aParam : TWinParam ) : Boolean;
var
  idx : integer;
begin
  idx := cbFont.Items.IndexOf( aParam.FontName);
  if idx >= 0 then
    cbFont.ItemIndex := idx;
  edtSize.Text := IntToStr( aParam.FontSize );

  Result := (ShowModal = mrOK);
end;

end.
