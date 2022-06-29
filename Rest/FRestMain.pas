unit FRestMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,

  REST.Types, REST.Client, Data.Bind.Components, Data.Bind.ObjectScope,

  USharedThread, UApiTypes
  ;

type
  TFrmRestMain = class(TForm)
    m: TMemo;
    RESTClient1: TRESTClient;
    BinReq: TRESTRequest;
    RESTResponse1: TRESTResponse;
    RESTClient2: TRESTClient;
    UpbReq: TRESTRequest;
    RESTResponse2: TRESTResponse;
    RESTClient3: TRESTClient;
    BitReq: TRESTRequest;
    RESTResponse3: TRESTResponse;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    procedure initControls;

    { Private declarations }
  public
    { Public declarations }
    mt : TSharedThread;
    procedure OnNotify(const S: string);
  end;

var
  FrmRestMain: TFrmRestMain;

implementation

uses
  system.IniFiles
  , GApp
  , GLibs


  ;

{$R *.dfm}

procedure TFrmRestMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TFrmRestMain.FormCreate(Sender: TObject);
begin
  App := TApp.Create;

  if not App.LoadConfig then
  begin
    ShowMessage('환경설정 파일을 읽을수 없음');
    Application.Terminate;
  end;

  initControls;

  mt := TSharedThread.Create( OnNotify );
end;

procedure TFrmRestMain.FormDestroy(Sender: TObject);
begin
  mt.Terminate;

  App.Free;
end;

procedure TFrmRestMain.initControls;
begin
  BinReq.Client.BaseURL := App.ApiConfig.GetBaseUrl( ekBinance, mtFutures);
  UpbReq.Client.BaseURL := App.ApiConfig.GetBaseUrl( ekUpbit, mtSpot);
  BitReq.Client.BaseURL := App.ApiConfig.GetBaseUrl( ekBithumb, mtSpot );
end;

procedure TFrmRestMain.OnNotify(const S: string);
begin
  m.Lines.Add( Copy(s, 1, 100 ) );
end;




end.
