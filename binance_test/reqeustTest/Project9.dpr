program Project9;

uses
  Vcl.Forms,
  Unit10 in 'Unit10.pas' {Form10},
  URestRequests in '..\..\api\URestRequests.pas',
  URestThread in 'URestThread.pas',
  UCyclicThreads in 'UCyclicThreads.pas',
  UCyclicItems in 'UCyclicItems.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm10, Form10);
  Application.Run;
end.
