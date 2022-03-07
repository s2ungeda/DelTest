program ExRate;

uses
  Vcl.Forms,
  Unit1 in 'Unit1.pas' {FrmExRate},
  XA_DATASETLib_TLB in '..\XA_DATASETLib_TLB.pas',
  XA_SESSIONLib_TLB in '..\XA_SESSIONLib_TLB.pas';

{$R *.res}

begin
  Application.Initialize;
//  Application.MainFormOnTaskbar := true;
  Application.ShowMainForm      := false;
  Application.CreateForm(TFrmExRate, FrmExRate);
  Application.Run;
end.
