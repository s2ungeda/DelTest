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
//          ShowMessage('Failed PrepareMaster');
          //AppStatusEvent( asError);
        end
        else
          App.AppStatus := asSetValue;//asLoad;

    asSetValue :
      begin
        if not App.Engine.ApiManager.InitMarketWebSocket then
        begin
          App.Log(llError, '', 'Failed InitMarketWebSocket') ;
//          ShowMessage('Failed InitMarketWebSocket');
          Exit;
        end;

//        if not App.Engine.ApiManager.ConnectAll then
//        begin
//          App.Log(llError, '', 'Failed ConnectAll') ;
//          ShowMessage('Failed ConnectAll');
//          Exit;
//        end;

        FrmDalinMain.SetValue;

        App.AppStatus := asRecovery;
      end;

    asRecovery :
      begin
        App.Engine.ApiManager.MakeCloseData;

        // 계좌 생성
        App.Engine.TradeCore.AccountLoad;

        // 잔고 조회
//        if not App.Engine.ApiManager.RequestBalance then
//        begin
//          App.Log(llError, '', 'Failed RequestBalance') ;
//          ShowMessage('Failed RequestBalance');
//          Exit;
//        end;
//        // 포지션 조회
//        if not App.Engine.ApiManager.RequestPositons then
//        begin
//          App.Log(llError, '', 'Failed RequestPositons') ;
//          ShowMessage('Failed RequestPositons');
//          Exit;
//        end;
        // 미체결 주문 조회.
//        if not App.Engine.ApiManager.RequestOrders then
//        begin
//          App.Log(llError, '', 'Failed RequestOrders') ;
//          ShowMessage('Failed RequestOrders');
//          Exit;
//        end;

        if not App.Engine.ApiManager.ConnectAll then
        begin
          App.Log(llError, '', 'Failed ConnectAll') ;
//          ShowMessage('Failed ConnectAll');
          Exit;
        end;

        App.Engine.SymbolCore.PreSubscribe;
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
        App.Engine.ApiManager.SubscribeAll;
//        App.Engine.ApiManager.StartRequest;
        FrmDalinMain.ExecuteSubApp;
        FrmDalinMain.Show;

      end;
  end;
end;

end.
