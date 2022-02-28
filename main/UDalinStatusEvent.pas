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
      begin
        if not App.Engine.ApiManager.InitMarketWebSocket then
        begin
          App.Log(llError, '', 'Failed InitMarketWebSocket') ;
          Exit;
        end;

        if not App.Engine.ApiManager.ConnectAll then
        begin
          App.Log(llError, '', 'Failed ConnectAll') ;
          Exit;
        end;

        FrmDalin.SetValue;

        App.AppStatus := asRecovery;
      end;

    asRecovery :
      begin
        App.Engine.SymbolCore.PreSubscribe;
        App.AppStatus := asLoad;
      end;
    asLoad :
          App.Engine.FormBroker.Load(ComposeFilePath([App.DataDir, App.Config.DATA_FILE]))
          ;
  end;
end;

end.
