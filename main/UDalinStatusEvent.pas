unit UDalinStatusEvent;

interface

uses
  UTypes
  ;

procedure AppStatusEvent( asType : TAppStatus );

implementation

uses
  GApp, GLibs
  ;

procedure AppStatusEvent( asType : TAppStatus );
begin
  case asType of
    asNone: ;
    asInit:
        if not App.Engine.ApiManager.GetMaster then
          App.Log(llError, '', 'Failed PrepareMaster');

    asLoad :
          App.Engine.FormBroker.Load(ComposeFilePath([App.DataDir, App.Config.DATA_FILE]))
          ;

  end;
end;

end.
