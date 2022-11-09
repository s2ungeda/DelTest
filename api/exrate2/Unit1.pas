unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, PythonEngine,
  system.Threading, Vcl.Menus
  ;

type
  TFrmExRate = class(TForm)
    pthnEngine: TPythonEngine;
    RtnValue: TPythonDelphiVar;
    EndValue: TPythonDelphiVar;
    Timer1: TTimer;
    Button1: TButton;
    Label1: TLabel;
    Label2: TLabel;
    PopupMenu1: TPopupMenu;
    N2: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
    TrayIcon1: TTrayIcon;
    Label3: TLabel;
    Label4: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure RtnValueSetData(Sender: TObject; Data: Variant);
    procedure EndValueSetData(Sender: TObject; Data: Variant);
    procedure N2Click(Sender: TObject);
    procedure N4Click(Sender: TObject);
  private
    { Private declarations }

    FStrings: TStringList;
    FNum   : integer;
    FStart : integer;
    FCount : int64;

    FLastTime : TDateTime;
    procedure start;
    procedure SaveFile(sData: string);
  public
    { Public declarations }
    property Strings : TStringList read FStrings;

  end;

var
  FrmExRate: TFrmExRate;

implementation

{$R *.dfm}

procedure TFrmExRate.Button1Click(Sender: TObject);
begin
  Hide;
end;



procedure TFrmExRate.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  action := caFree;
end;

procedure TFrmExRate.FormCreate(Sender: TObject);
begin
  FStrings  := TStringList.Create;
  FStrings.LoadFromFile('Data/exRate.py');

  FNum := 0;
  FStart  := 0;
  FCount  := 0;
  FLastTime := now;

  MaskFPUExceptions(True);

  start;
end;

procedure TFrmExRate.FormDestroy(Sender: TObject);
begin
  FStrings.Free;
end;

procedure TFrmExRate.N2Click(Sender: TObject);
begin
  Show
end;

procedure TFrmExRate.N4Click(Sender: TObject);
begin
  close;
end;

procedure TFrmExRate.EndValueSetData(Sender: TObject; Data: Variant);
var
  sData : string;
begin

  sData := Data;

  if sData <> '1' then
  begin
    if sData = '-1' then
      Label3.Caption := Format('%s request error', [ FormatDateTime('yyyy-mm-dd hh:nn:ss', now) ]);
    Timer1.Enabled := true;
  end;
end;

procedure TFrmExRate.RtnValueSetData(Sender: TObject; Data: Variant);
var
  sData : string;
begin

  sData := Data;
  inc( FCount );

  if ( Length(sData) > 3 ) and (sData <> '0') then begin
    Label1.Caption :=  Format('%s %05d', [ FormatDateTime('hh:nn:ss', now) , FCount]);
    Label4.Caption :=  sData;
    FLastTime := now;
    SaveFile( Format('%s,%s', [ FormatDateTime('hh:nn:ss', FLastTime), sData]) );
  end;

  if FCount > (High( int64 ) - 10) then
    FCount := 0;

end;

procedure TFrmExRate.start;
begin
  TTask.Run(
    procedure
    begin
      if FStrings.Capacity > 0 then
        pthnEngine.ExecStrings( FStrings )
    end
    );
  Label2.Caption := Format('%s %03d.th start', [ FormatDateTime('yyyy-mm-dd hh:nn:ss', now) , FStart]);
  inc(FStart);
end;

procedure TFrmExRate.Timer1Timer(Sender: TObject);
var
  s : string;
begin

  if EndValue.ValueAsString <> '1' then
    inc( FNum );

  if (FNum > 5) and ( EndValue.ValueAsString <> '1') then
  begin
    Timer1.Enabled := false;
    FNum := 0;
    Start;
  end;

end;

procedure TFrmExRate.SaveFile( sData : string);
var
  LogFileName : string;
  OutFile: TextFile;

  function IsFileUse(fName: String): Boolean;
  var
    HFile: THandle;
  begin
    Result := false;
    if not FileExists(fName) then exit;
    HFile := CreateFile(PChar(fName), GENERIC_READ or GENERIC_WRITE, 0, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
    Result := (HFile = INVALID_HANDLE_VALUE);
    if not Result then begin
      try
        //Memo_Log.Lines.Add('Value = ' + IntToStr(HFile));
      finally
        CloseHandle(HFile);
      end;
    end;
  end;
begin

  LogFileName :='data\exchangeRate.log';

  try

    if not IsFileUse(LogFileName) then begin
      AssignFile(OutFile, LogFileName);
      ReWrite(OutFile);
      try
        Writeln(OutFile,sData);
      finally
        CloseFile(OutFile);
      end;
    end;
  Except
  end;

end;

end.
