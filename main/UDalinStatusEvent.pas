unit UDalinStatusEvent;

interface

uses
  UTypes
  ;

procedure AppStatusEvent( asType : TAppStatus );

implementation

uses
  GApp, GLibs
  , DalinMain
  ;

procedure AppStatusEvent( asType : TAppStatus );
begin
  case asType of
    asNone: ;
    asInit:
        if not App.Engine.ApiManager.GetMaster then
          App.Log(llError, '', 'Failed PrepareMaster')
        else
          App.AppStatus := asSetValue;//asLoad;

    asSetValue :
      FrmDalin.SetValue;

    asLoad :
          App.Engine.FormBroker.Load(ComposeFilePath([App.DataDir, App.Config.DATA_FILE]))
          ;
  end;
end;

end.
