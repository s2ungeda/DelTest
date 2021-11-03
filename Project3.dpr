program Project3;

uses
  Vcl.Forms,
  Unit3 in 'Unit3.pas' {eSaac},
  ULogThread in 'ULogThread.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TeSaac, eSaac);
  Application.Run;
end.
