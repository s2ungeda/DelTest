program Dalin;
uses
  Vcl.Forms,
  NMainMenu in 'main\NMainMenu.pas' {DataModule1: TDataModule},
  DalinMain in 'main\DalinMain.pas' {FrmMain},
  GApp in 'main\GApp.pas',
  UDalinEngine in 'engine\UDalinEngine.pas',
  GLibs in 'engine\common\GLibs.pas',
  UConfig in 'engine\common\UConfig.pas',
  UTypes in 'engine\common\UTypes.pas',
  ULogThread in 'engine\utils\ULogThread.pas',
  UParsers in 'engine\utils\UParsers.pas',
  UStorage in 'engine\utils\UStorage.pas';

{$R *.res}
begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TDataModule1, DataModule1);
  Application.CreateForm(TFrmMain, FrmMain);
  Application.Run;

end.
