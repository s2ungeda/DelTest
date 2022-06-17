program seconds;

uses
  Vcl.Forms,
  Unit3 in 'Unit3.pas' {Form2},
  Unit1 in 'Unit1.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
