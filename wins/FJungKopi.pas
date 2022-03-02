unit FJungKopi;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs

  , UStorage, Vcl.Grids
  ;

type
  TFrmJungKopi = class(TForm)
    sgVal: TStringGrid;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure SaveEnv( aStorage : TStorage );
    procedure LoadEnv( aStorage : TStorage );
  end;

var
  FrmJungKopi: TFrmJungKopi;

implementation

{$R *.dfm}

{ TFrmJungKopi }

procedure TFrmJungKopi.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TFrmJungKopi.FormCreate(Sender: TObject);
var
  I, iMod: Integer;
  s : string;
begin
  with sgVal do
    for I := 0 to ColCount-1 do
    begin
      if i = 0 then
        ColWidths[i] := 60
      else
        ColWidths[i] := 35;

      if i > 0  then  begin
        iMod := i Mod 2;
        if iMod = 0 then
          s := '30'
        else
          s := '00';

        Cells[i, 0 ]  := Format('%d:%s', [ (i-1) div 2, s] );
        Cells[i, 5 ] := Format('%d:%s', [ (i-1) div 2 + 12, s ]);
      end;
    end;

  with sgVal do
  begin
    Cells[0,0]  := '거래소';
    Cells[0,1]  := 'UB 중코피';
    Cells[0,2]  := 'BT 중코피';
    Cells[0,5]  := 'RealTime';

  end;
end;

procedure TFrmJungKopi.FormDestroy(Sender: TObject);
begin
  //
end;

procedure TFrmJungKopi.LoadEnv(aStorage: TStorage);
begin

end;

procedure TFrmJungKopi.SaveEnv(aStorage: TStorage);
begin

end;

end.
