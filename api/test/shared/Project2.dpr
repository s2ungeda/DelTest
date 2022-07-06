program Project2;

uses
  Vcl.Forms,
  Unit2 in 'Unit2.pas' {Form2},
  uNamedShareMemory in 'uNamedShareMemory.pas',
  USharedThread in '..\..\..\Rest\USharedThread.pas',
  USharedConsts in '..\..\..\Rest\USharedConsts.pas',
  USharedData in '..\..\..\Rest\USharedData.pas',
  GLibs in '..\..\..\engine\common\GLibs.pas',
  UConsts in '..\..\..\engine\common\UConsts.pas',
  UTypes in '..\..\..\engine\common\UTypes.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
