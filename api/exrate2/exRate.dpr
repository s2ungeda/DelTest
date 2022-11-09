program exRate;

uses
  Vcl.Forms,
  Unit1 in 'Unit1.pas' {FrmExRate};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFrmExRate, FrmExRate);
  Application.Run;
end.
