unit Unit10;
interface
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs
  , URestThread, Vcl.StdCtrls, REST.Types, REST.Client, Data.Bind.Components,
  Data.Bind.ObjectScope, Vcl.ExtCtrls ,

  UCyclicThreads, UCyclicItems
  ;
type
  TForm10 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Button4: TButton;
    RESTClient1: TRESTClient;
    req: TRESTRequest;
    RESTResponse1: TRESTResponse;
    Button5: TButton;
    Timer1: TTimer;
    CheckBox1: TCheckBox;
    Memo1: TMemo;
    Edit4: TEdit;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    Button10: TButton;
    cbDiv: TCheckBox;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);

    procedure CheckBox1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure Button10Click(Sender: TObject);
  private
    { Private declarations }
    FThread : TRestThread;
    FCclThr: TCyclicThread;
    FIndex, FCount : integer;
    FList  : TList;
    procedure PushData;
    procedure findres(Sender: TObject);
    function GetSig(idx: Integer): string;
  public
    { Public declarations }
    Cyclics : TCyclicItems;
    procedure OnNotify(Sender: TObject) ;
    procedure OnNotify2(Sender: TObject) ;
  end;
var
  Form10: TForm10;
implementation       

uses
   URestRequests   
   , UEncrypts
   ,   JOSE.Core.JWT
  , JOSE.Core.JWA
  , JOSE.Core.Builder
   
   ;
{$R *.dfm}
procedure TForm10.Button10Click(Sender: TObject);
begin
  FCclThr.doJob;
end;

procedure TForm10.Button1Click(Sender: TObject);
begin
  //
  FThread := TRestThread.Create( OnNotify, FList );
end;

procedure TForm10.Button2Click(Sender: TObject);
begin
  if FThread <> nil then
  begin
    FThread.Terminate;
//    FThread.WaitFor;
//    FThread.Free;
//    FThread := nil;
  end;
end;

procedure TForm10.Button3Click(Sender: TObject);
var
	aReq : TRequest;
  iTag : integer;
  sNm, sRsrc  : string;


  i : integer;
  aItem : TCyclicItem;
  bSend : boolean;
  gap, nTick : DWORD;
begin
//	aReq := TRequest.Create;
//
//	case ( Sender as TButton).Tag of
//  	0 : begin sNm := 'orderbook';  sRsrc:= edit2.Text; end;
//    1 : begin sNm := 'ticker';  sRsrc:= edit3.Text; end;
//    2 : begin sNm := 'status';  sRsrc:= edit4.Text; end;
//  end;
//
//  aReq.init( rmGET, edit1.Text, sRsrc, sNm, true );
//  aReq.OnNotify := OnNotify2;
//  if aReq.RequestAsync then
//    FList.Add( aReq );


 for I := 0 to Cyclics.Count-1 do
    begin
      aItem := Cyclics.Cyclic[i];
      if aItem = nil then continue;

      bSend := false;
      nTick := GetTickCount;

      if aITem.LastTime <= 0 then
      begin
        gap := aItem.Count * INTERVAL;
        if gap >= aItem.Interval then
          bSend := true;
        inc( aItem.Count );
      end else
      begin
        gap   := nTick - aItem.LastTime ;
        if gap >= aItem.Interval then
          bSend := true;
      end;

      if bSend then begin
        aItem.PrevTime  := aItem.LastTime;
        aItem.LastTime  := nTick;

        OnNotify( aItem );
//        FData := aItem;
//        Synchronize( SyncProc );
      end;
    end;
end;

procedure TForm10.Button5Click(Sender: TObject);
var
	aReq : TReqeustItem;
  iTag : integer;
  sJson, sOut : string;
begin
	aReq := TReqeustItem.Create;
  aReq.AMethod	:= rmGET;        
    
  aReq.Req.init( edit1.Text, true );

	case ( Sender as TButton).Tag of
  	0 : aReq.AResource:= edit2.Text;
    1 : aReq.AResource:= edit3.Text;	  
  end;

  aReq.Req.Request( rmGET, aReq.AResource, '',sJson, sOut );
 // OnNotify( aReq.Req.GetResponse );
end;



procedure TForm10.Button7Click(Sender: TObject);
var
  aItem : TCyclicItem;
begin

  FIndex := 0;

  Cyclics.Clear;

//	bithumb
//  aItem := Cyclics.New('orderbook');
//  aItem.Interval  := 100;
//  aItem.Index     := 1;
//
//  aItem := Cyclics.New('ticker');
//  aItem.Interval  := 500;
//  aItem.Index     := 2;
//
//  aItem := Cyclics.New('assetsstatus');
//  aItem.Interval  := 2000;
//  aItem.Index     := 3;

//  aItem := Cyclics.New('orderbook');
//  aItem.Interval  := 200;
//  aItem.Index     := 1;
//
//  aItem := Cyclics.New('ticker');
//  aItem.Interval  := 200;
//  aItem.Index     := 2;

  aItem := Cyclics.New('assetsstatus');
  aItem.Interval  := 3000;
  aItem.Index     := 3;

  FCclThr:= TCyclicThread.Create(Cyclics, OnNotify);
  FCclThr.Resume;
end;

procedure TForm10.Button8Click(Sender: TObject);
begin
  FCclThr.Terminate;
  FCclThr := nil;

  FList.Clear;
end;

procedure TForm10.Button9Click(Sender: TObject);
var
  aReq : TRequest;
  I: Integer;
begin
  // 미리 100 개 만들어둔다.
  FList.Clear;
  for I := 0 to 49 do
  begin
    aReq := TRequest.Create;
    aReq.init( edit1.Text, true);
    aReq.OnNotify := OnNotify2;
    FList.Add( aReq );
  end;

end;

procedure TForm10.CheckBox1Click(Sender: TObject);
begin
	Timer1.Enabled	:= CheckBox1.Checked;
end;

procedure TForm10.FormCreate(Sender: TObject);
begin
  FThread := nil;
  FCclThr := nil;
  FList   := TList.Create;
  FCount  := 0;
  Button1Click( nil );

  Cyclics := TCyclicItems.Create;

//	bithumb
//  Edit1.Text := 'https://api.bithumb.com';
//  Edit2.Text := '/public/orderbook/ALL_KRW';
//  Edit3.Text := '/public/ticker/ALL_KRW';
//  Edit4.Text := '/public/assetsstatus/ALL';

	// upbit
  Edit1.Text := 'https://api.upbit.com';
  Edit2.Text := '/v1/orderbook';
  Edit3.Text := '/v1/ticker';
  Edit4.Text := '/v1/status/wallet';
end;

procedure TForm10.FormDeactivate(Sender: TObject);
begin
//	Button2Click( nil );
end;

procedure TForm10.FormDestroy(Sender: TObject);
begin
  if FCclThr <> nil then
    FCclThr.Terminate;
	Button2Click(nil);
  FList.Free;
  Cyclics.Free;
end;

procedure TForm10.findres( Sender : TObject);
var
  I: Integer;
	aReq : TReqeustItem;
begin
  for I := FList.Count-1 downto 0 do
  begin
    aReq :=  TReqeustItem( FList.Items[i] );
    if aReq <> nil then begin
    end;
  end;
end;

function TForm10.GetSig( idx : Integer ): string;
var
  LToken: TJWT;
  guid : TGUID;
  sSig, sID : string;
begin

  LToken:= TJWT.Create(TJWTClaims);

  try

    sID := GetUUID;

    LToken.Claims.SetClaimOfType<string>('access_key', 'pPutaXMQMoY3wzyhe2B4ZxNBKd0Fbb4DyaVDQrNN');
    LToken.Claims.SetClaimOfType<string>('nonce', sID );

    sSig := TJOSE.SerializeCompact(  'vKE178MrOBDu5CsjoNtEW7N6Kg4qYK8BiqNsxoux'  ,  TJOSEAlgorithmId.HS256, LToken);
    Result := Format('Bearer %s', [sSig ]);   
  finally
    LToken.Free;
  end;
end;

procedure TForm10.OnNotify(Sender: TObject);
var
  sTmp, ss : string;
  aItem : TCyclicItem;
	aReq : TRequest;
  iTag : integer;
  sNm, sRsrc  : string;
begin

  if Sender = nil then Exit;

  aItem := Sender as TCyclicItem;

//  memo1.Lines.Add(  Format('%d (%d)(%d) : %s', [ aItem.LastTime, aItem.LastTime - aItem.PrevTime,
//      aItem.Interval, aItem.Name ] ) );

//  exit;

  try

    if FIndex >= FList.Count then
      FIndex := 0;

    aReq :=  TRequest( FList.Items[Findex] );
    if ( aReq <> nil ) and ( aReq.State <> 1 ) then
    begin

      aReq.Req.Params.Clear;

      case aItem.Index of
        1 : begin  sRsrc:= edit2.Text; end;
        2 : begin  sRsrc:= edit3.Text; end;
        3 : begin  sRsrc:= edit4.Text; end;
      end;
      
      
      if aItem.index in [1..2] then
      begin
			//	if cbDiv.Checked then

					sTmp :='KRW-BTC,KRW-ETH,KRW-NEO,KRW-MTL,KRW-LTC,KRW-XRP,KRW-ETC,KRW-OMG,KRW-WAVES,KRW-XEM,KRW-QTUM,KRW-XLM,'+
        	'KRW-STORJ,KRW-ADA,KRW-ICX,KRW-EOS,KRW-TRX,KRW-SC,KRW-ONT,KRW-ZIL,KRW-ZRX,KRW-BCH,KRW-BAT,KRW-IOST,KRW-CVC,'+
          'KRW-IOTA,KRW-KNC,KRW-THETA,KRW-ENJ,KRW-MANA,KRW-ANKR,KRW-ATOM,KRW-HBAR,KRW-VET,KRW-CHZ,KRW-STMX,KRW-KAVA,'+
          'KRW-LINK,KRW-XTZ,KRW-SXP,KRW-DOT,KRW-SRM,KRW-SAND,KRW-DOGE,KRW-FLOW,KRW-AXS,KRW-SOL,KRW-MATIC,KRW-AAVE,KRW-1INCH,KRW-ALGO,KRW-NEAR,KRW-AVAX,KRW-CELO,KRW-GMT';
//        else
//        
//      		sTmp := 'KRW-BTC';
	    	aReq.Req.AddParameter('markets', sTmp, pkGETorPOST);          
      end  else
      begin
        var sToken : string;
        sToken := GetSig(0);
        aReq.Req.AddParameter('Authorization', sToken, TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode] );      

        memo1.Lines.Add( format('status  thread id : %d' , [  GetCurrentThreadID ]  )   );        
      end;

      aReq.SetParam(rmGET, sRsrc, aITem.Name);

      if FThread <> nil then
        FThread.PushQueue( aReq );

//      if aReq.RequestAsync then
//        aReq.State := 1;
    end;
  finally
      inc( FIndex );
  end;

end;

procedure TForm10.OnNotify2(Sender: TObject);
var
   aReq : TRequest;
   sTmp : string;
begin
  if Sender = nil then Exit;
  aReq := Sender as TRequest;

//  if aReq.Name = 'ticker' then
//


	if  aReq.StatusCode <> 200 then
  begin
	// ubpit
  sTmp := aReq.Req.Response.Headers.Values['Remaining-Req'];
//  App.DebugLog('remain-req', [ sTmp] );
  
  memo1.Lines.Add( Format('%d(%03d) : %d, %s, %s, %100.100s' , [ aReq.GetID, FIndex,
    aReq.StatusCode, aReq.Name, stmp,  aReq.Content
   ] )   );
  end else
  if aReq.Name = 'assetsstatus' then  
	  memo1.Lines.Add( format('status respose thread id : %d' , [  GetCurrentThreadID ]  )   );

  aReq.State := 2;

//  FList.Remove( Sender );
//  aReq.Free;
end;

procedure TForm10.PushData;
var
	aReq : TReqeustItem;
  iTag : integer;
  I: Integer;
begin

	for I := 0 to 2 do
	begin

    aReq := TReqeustItem.Create;
    aReq.AMethod	:= rmGET;
    aReq.Req.init( edit1.Text, true );

    case i of
      0 : begin aReq.AResource:= edit2.Text;  aReq.Name := 'orderb'; end;
      1 : begin aReq.AResource:= edit3.Text;	aReq.Name := 'ticker'; end;
      2 : begin aReq.AResource:= edit4.Text;	aReq.Name := 'assets';end;
    end;

//    if FThread <> nil then
//      FThread.PushQueue( aReq);
  end;

end;

procedure TForm10.Timer1Timer(Sender: TObject);
var
  iMod : integer;
begin

  iMod := FCount mod 3 ;

  case iMod of
    0 :  Button3Click( Button3);
    1 :  Button3Click( Button4);
    2 :  Button3Click( Button5);
  end;

//	if FCount mod 2 = 0 then
//  	Button5Click( Button3)
//  else
//	 	Button5Click( Button4)   ;

  inc( FCount );
  		
end;

end.

