unit Unit10;
interface
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs
  , URestThread, Vcl.StdCtrls, REST.Types, REST.Client, Data.Bind.Components,
  Data.Bind.ObjectScope, Vcl.ExtCtrls
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
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);

    procedure CheckBox1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    FThread : TRestThread;
    FCount : integer;
    FList  : TList;
    procedure PushData;
    procedure findres(Sender: TObject);
  public
    { Public declarations }
    procedure OnNotify(Sender: TObject) ;
  end;
var
  Form10: TForm10;
implementation       

uses
   URestRequests
   ;
{$R *.dfm}
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
    FThread.WaitFor;
    FThread.Free;
    FThread := nil;
  end;
end;

procedure TForm10.Button3Click(Sender: TObject);
var
	aReq : TRequest;
  iTag : integer;
  sNm, sRsrc  : string;
begin
	aReq := TRequest.Create;

	case ( Sender as TButton).Tag of
  	0 : begin sNm := 'orderbook';  sRsrc:= edit2.Text; end;
    1 : begin sNm := 'ticker';  sRsrc:= edit3.Text; end;
    2 : begin sNm := 'status';  sRsrc:= edit4.Text; end;
  end;

  aReq.init( rmGET, edit1.Text, sRsrc, sNm, true );
  aReq.OnNotify := OnNotify;
  if aReq.RequestAsync then
    FList.Add( aReq );

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



procedure TForm10.CheckBox1Click(Sender: TObject);
begin
	Timer1.Enabled	:= CheckBox1.Checked;
end;

procedure TForm10.FormCreate(Sender: TObject);
begin
  FThread := nil;
  FList   := TList.Create;
  FCount  := 0;
  Button1Click( nil );


  Edit1.Text := 'https://api.bithumb.com';
  Edit2.Text := '/public/orderbook/ALL_KRW';
  Edit3.Text := '/public/ticker/ALL_KRW';
  Edit4.Text := '/public/assetsstatus/ALL';
end;

procedure TForm10.FormDeactivate(Sender: TObject);
begin
//	Button2Click( nil );
end;

procedure TForm10.FormDestroy(Sender: TObject);
begin
	Button2Click(nil);
  FList.Free;
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

procedure TForm10.OnNotify(Sender: TObject);
var
	iLen : integer;
  sTmp, ss : string;
  aReq : TRequest;
begin

  if Sender = nil then Exit;
  aReq := Sender as TRequest;

  memo1.Lines.Add( Format('%d(%03d) : %d, %s, %100.100s' , [ aReq.GetID, FList.Count,
    aReq.StatusCode, aReq.Name, aReq.Content
   ] )   );

  FList.Remove( Sender );
  aReq.Free;

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

    if FThread <> nil then
      FThread.PushQueue( aReq);
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

