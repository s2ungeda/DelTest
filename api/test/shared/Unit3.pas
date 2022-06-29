unit Unit3;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,

  Unit1, REST.Types, REST.Client, Data.Bind.Components, Data.Bind.ObjectScope
  ;

type
  TForm2 = class(TForm)
    m: TMemo;
    RESTClient1: TRESTClient;
    RESTRequest1: TRESTRequest;
    RESTResponse1: TRESTResponse;
    RESTClient2: TRESTClient;
    RESTRequest2: TRESTRequest;
    RESTResponse2: TRESTResponse;
    RESTClient3: TRESTClient;
    RESTRequest3: TRESTRequest;
    RESTResponse3: TRESTResponse;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    function LoadExchangeConfig: boolean;
    { Private declarations }
  public
    { Public declarations }
    mt : TMmfThread;
    procedure OnNotify(const S: string);
  end;

var
  Form2: TForm2;

implementation

uses
  system.IniFiles

  ;

{$R *.dfm}

procedure TForm2.FormCreate(Sender: TObject);
begin
  mt := TMmfThread.Create( OnNotify );
end;

procedure TForm2.FormDestroy(Sender: TObject);
begin
  mt.Terminate;
end;

procedure TForm2.OnNotify(const S: string);
begin
  m.Lines.Add( Copy(s, 1, 100 ) );
end;


function TForm2.LoadExchangeConfig: boolean;
var
  pIniFile : TIniFile;
  stDir : string;
  iCnt : integer;
  I: Integer;
  j : TMarketType;
begin
  result := true;

  try
    try
      stDir := ExtractFilePath( paramstr(0) )+'Config\';
      pIniFile := TIniFile.Create(stDir + 'exchange.ini' );

      /////////////////////////////////////////////////////////////

      iCnt := pIniFile.ReadInteger('Exchange', 'Count', 0);

      SetLength( ExchangeInfo, iCnt );

      for I := 0 to iCnt-1 do
      begin
        stDir := Format('Exchange_%d', [i]);
        ExchangeInfo[i].SetInfo(  i,
          pIniFile.ReadString(stDir, 'name', 'Sauri')
          , pIniFile.ReadInteger(stDir, 'domestic',0 ) = 1
          , pIniFile.ReadInteger(stDir, 'margin', 0) = 1
          , pIniFile.ReadInteger(stDir, 'fut', 0 ) = 1
        ) ;

        if ExchangeInfo[i-1].IsDomestic then inc( FDomesticCnt )
        else inc( FOverseasCnt );
      end;

      for I := 0 to iCnt-1 do
      begin


        for j := mtSpot to High(TMarketType) do
          if ( j = mtSpot ) or ((  j = mtFutures ) and ( ExchangeInfo[i].IsFuture ))  then
          begin
            stDir := Format( '%s_%s', [ ExchangeInfo[i].Name, ifThenStr( j = mtSpot,'spot', 'future')  ] );

            ExchangeInfo[i].MarketInfo[j].BaseUrl  :=  pIniFile.ReadString( stDir, 'Url', 'Sauri');
            ExchangeInfo[i].MarketInfo[j].Prepare  :=  pIniFile.ReadString( stDir, 'PrePare', 'Sauri');
            ExchangeInfo[i].MarketInfo[j].Port     :=  pIniFile.ReadInteger( stDir, 'Port', 443 );
            ExchangeInfo[i].MarketInfo[j].Key      :=  pIniFile.ReadString( stDir, 'ApiKey', 'Sauri' );
            ExchangeInfo[i].MarketInfo[j].Secret   :=  pIniFile.ReadString( stDir, 'SecretKey', 'Sauri' );
          end;
      end;

      //App.Log(llInfo, '', '---start---');
      /////////////////////////////////////////////////////////////
      if App.Config.VERBOSE then
        for I := 0 to iCnt-1 do
          for j := mtSpot to mtFutures do
          begin
            App.Log( llDebug, '', '%d %s[%s] %s, %s, %s', [i, ExchangeInfo[i].Name
                , TMarketTypeDesc[j]
                , ExchangeInfo[i].MarketInfo[j].BaseUrl
                , ExchangeInfo[i].MarketInfo[j].Key
                , ExchangeInfo[i].MarketInfo[j].Secret ]   );
          end;


    except
      result := false;
    end;
  finally
    pIniFile.Free;
  end;
end;


end.
