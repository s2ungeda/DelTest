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
    procedure PushData;
  public
    { Public declarations }
    procedure OnNotify(const S: string) ;
  end;
var
  Form10: TForm10;
implementation       

//uses
//	 REST.Types
//   ;
{$R *.dfm}
procedure TForm10.Button1Click(Sender: TObject);
begin
  //
  FThread := TRestThread.Create( OnNotify );
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
	aReq : TReqeustItem;
  iTag : integer;
begin
	aReq := TReqeustItem.Create;
  aReq.AMethod	:= rmGET;
  aReq.Req.init( edit1.Text );

	case ( Sender as TButton).Tag of
  	0 : aReq.AResource:= edit2.Text;
    1 : aReq.AResource:= edit3.Text;	  
  end;

  if FThread <> nil then
  	FThread.PushQueue( aReq);

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
  OnNotify( aReq.Req.GetResponse );   
end;



procedure TForm10.CheckBox1Click(Sender: TObject);
begin
	Timer1.Enabled	:= CheckBox1.Checked;
end;

procedure TForm10.FormCreate(Sender: TObject);
begin
  FThread := nil;
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
end;

procedure TForm10.OnNotify(const S: string);
var
	iLen : integer;
begin
	iLen := Length( S );
  if iLen > 200 then  	
	  memo1.Lines.Add( Copy(S, 1, 200 ) )
  else
	  memo1.Lines.Add( S)  ;
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
begin
	PushData;

  Exit;
  
	if FCount mod 2 = 0 then
  	Button3Click( Button3)
  else
	 	Button3Click( Button4)   ;

//	if FCount mod 2 = 0 then
//  	Button5Click( Button3)
//  else
//	 	Button5Click( Button4)   ;    

  inc( FCount );
  		
end;

end.

