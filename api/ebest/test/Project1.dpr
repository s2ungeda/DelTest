program Project1;

uses
  Vcl.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  XA_DATASETLib_TLB in '..\XA_DATASETLib_TLB.pas',
  XA_SESSIONLib_TLB in '..\XA_SESSIONLib_TLB.pas';

{$R *.res}

begin
  Application.Initialize;
//  Application.MainFormOnTaskbar := true;
//  Application.ShowMainForm      := false;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
