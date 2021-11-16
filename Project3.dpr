program Project3;

uses
  Vcl.Forms,
  Unit3 in 'Unit3.pas' {eSaac},
  ULogThread in 'ULogThread.pas',
  UTypes in 'UTypes.pas',
  UStorage in 'UStorage.pas',
  UParsers in 'UParsers.pas',
  UPriceTable in 'UPriceTable.pas' {FrmPriceTable},
  UTableConsts in 'UTableConsts.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFrmPriceTable, FrmPriceTable);
  Application.CreateForm(TeSaac, eSaac);
  Application.Run;

end.
