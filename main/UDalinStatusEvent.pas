unit UDalinStatusEvent;

interface

uses
  UTypes
  ;

procedure AppStatusEvent( asType : TAppStatus );

implementation

uses
  GApp, GLibs  , shellapi, Dialogs
  , DalinMain
  ;

procedure AppStatusEvent( asType : TAppStatus );
begin
  case asType of
//    asError :
//      begin
//        ShowMessage( App.ErrorString);
//        App.ErrorString := '';
//      end;
    asNone: ;
    asInit:
        if not App.Engine.ApiManager.GetMaster then begin
          App.Log(llError, '', 'Failed PrepareMaster');
          ShowMessage('Failed PrepareMaster');
          //AppStatusEvent( asError);
        end
        else
          App.AppStatus := asSetValue;//asLoad;

    asSetValue :
      begin
        if not App.Engine.ApiManager.InitMarketWebSocket then
        begin
          App.Log(llError, '', 'Failed InitMarketWebSocket') ;
          ShowMessage('Failed InitMarketWebSocket');
          Exit;
        end;

        if not App.Engine.ApiManager.ConnectAll then
        begin
          App.Log(llError, '', 'Failed ConnectAll') ;
          ShowMessage('Failed ConnectAll');
          Exit;
        end;

        FrmDalinMain.SetValue;

        App.AppStatus := asRecovery;
      end;

    asRecovery :
      begin
        App.Engine.ApiManager.MakeCloseData;

        App.Engine.SymbolCore.PreSubscribe;

//        // 전종목 구독...
//        if not App.Engine.ApiManager.SubscribeAll then
//        begin
//          App.Log(llError, '', 'Failed SubscribeAll') ;
//          ShowMessage('Failed SubscribeAll');
//          Exit;
//        end;
        App.AppStatus := asLoad;
      end;
    asLoad :
      begin
          App.Engine.FormBroker.Load(ComposeFilePath([App.DataDir, App.Config.DATA_FILE]))
          ;
          App.AppStatus := asShow;
      end;
    asShow :
      begin
        //
        //App.Engine.ApiManager.StartRequest;
        App.Engine.ApiManager.SubscribeAll;
        FrmDalinMain.Show;
      end;
  end;
end;

end.
