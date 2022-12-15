unit GLibs;

interface

uses
  Windows, System.Classes, system.SysUtils, system.Math,
  Vcl.StdCtrls, system.DateUtils, Winapi.messages,
  Vcl.Forms , vcl.Dialogs, Vcl.Graphics,
  ShellApi , UConsts,
  vcl.Grids

  ;
type
  TmpGrid = class( TCustomGrid );

function AppDir : String;
function ComposeFilePath(stDirs: array of String; cDelimiter: Char = '/'): String;
function FileVersionToStr(FileName: String): String;

//----------------------------------------
function GetPrecision( aText : string ) : integer;
function Get30TermIdx : integer;

function IfThenStr(AValue: Boolean; const ATrue: string; const AFalse: string): string;
function IfThenFloat(AValue: Boolean; const ATrue: double): string;
function FmtString( iPre : integer; dVal : double; iDiv : integer = 0 ): string;

//---------------------------------------- file

function GetTimestamp(len: Integer = 13): string;
function UnixTimeToDateTime( UnixTime : int64;  len: Integer = 13 ) : TDateTime;
function GetStrToTime(sTime : string; iType: integer = 0) : TDateTime;
procedure ExecuteApp( aHandle : HWND; sClassName , sAppName : string );
procedure CloseApp( sClassName : string );

//--------------------------------------- controls

procedure DeleteLine( aGrid : TStringGrid; iline: Integer);
procedure InsertLine( aGrid : TStringGrid; iline: Integer);
procedure InitGrid( aGrid : TStringGrid; bClear : boolean; FixedCnt : integer = 0);
procedure InvalidateRow( aGrid : TStringGrid; iline : integer );
procedure SetColor(dVal: double; aGrid: TStringGrid; iCol,  iRow: integer);
// 0 : back,  1 : font
function GetColor(iType, iColorFlag : integer) : TColor; overload;
function GetColor( d : double ) : TColor; overload;
//procedure GetColor(dVal: double; aGrid: TStringGrid; iCol,  iRow: integer);
procedure ComboBox_AutoWidth(const theComboBox: TCombobox);           
function EnumFamToLines(lplf: PLOGFONT; lpntm: PNEWTEXTMETRIC; FontType: DWORD; Lines: LPARAM): Integer; stdcall;


//------------------------------------------math
function CheckZero( dVal : double ) : boolean;



//--------------------------------------------- http
function EncodePath(sRrsc, sPoint, sTime : string): string;


implementation

uses
	Web.HTTPApp
  ;


function AppDir : String;
begin
  Result := ExtractFileDir(Application.ExeName);
end;

function ComposeFilePath(stDirs: array of String; cDelimiter: Char = '/'): String;
var
  i, iCount: Integer;
begin
  Result := '';

    // make path

  iCount := 0;

  for i := Low(stDirs) to High(stDirs) do
  begin
    if Length(stDirs[i]) = 0 then Continue;

    if iCount > 0 then
      Result := Result + cDelimiter + stDirs[i]
    else
      Result := Result + stDirs[i];

    Inc(iCount);
  end;

    // make sure only one type of delimter is used
  if cDelimiter <> '\' then
    Result := StringReplace(Result, '\', cDelimiter, [rfReplaceAll]);
  if cDelimiter <> '/' then
    Result := StringReplace(Result, '/', cDelimiter, [rfReplaceAll]);

    // remove double delimiters
  Result := StringReplace(Result, cDelimiter + cDelimiter, cDelimiter, [rfReplaceAll]);
end;


function FileVersionToStr(FileName: String): String;
var
  Size, Size2: DWord;
  pT, pT2: Pointer;
begin
  Result := '';
  Size := GetFileVersionInfoSize(PChar(FileName), Size2);
  if Size > 0 then
  begin
    GetMem(Pt, Size);
    try
      GetFileVersionInfo(PChar(FileName), 0, Size, Pt);
      VerQueryValue(Pt, '\', Pt2, Size2);
      with TVSFixedFileInfo(Pt2^) do
      begin
        Result := IntToStr(HiWord(dwFileVersionMS)) + '.' +//major version
                  IntToStr(LoWord(dwFileVersionMS)) + '.' +//minor version
                  IntToStr(HiWord(dwFileVersionLS)) + '.' +//release
                  IntToStr(LoWord(dwFileVersionLS));       //build
      end;
    finally
      FreeMem(Pt);
    end;//try
  end;//if

end;


function IfThenStr(AValue: Boolean; const ATrue: string; const AFalse: string): string;
begin
  if AValue then
    Result := ATrue
  else
    Result := AFalse;
end;

function IfThenFloat(AValue: Boolean; const ATrue: double ): string;
begin
  if AValue then
    Result := Format('%.2f', [ ATrue ])
  else
    Result := ''
end;

function FmtString( iPre : integer; dVal : double; iDiv : integer  ): string;
begin
  if IsZero( dVal ) then
    Result := '0'
  else begin
  	if iDiv = 0 then    
	    Result := Format('%.*n', [ iPre, dVal ])
    else
	    Result := Format('%.*f', [ iPre, dVal ]);
  end
    
end;

function GetPrecision( aText : string ) : integer;
var
  iLen, iPos, iPos2 : integer;
begin
  iLen  := Length( aText );
  iPos  := Pos( '.', aText);
  if iPos > 0 then
  begin
    iPos2 := Pos('1', aText);
    if ( iPos2 < 0 ) or ( iPos2 <= iPos ) then
      Result := iLen - iPos - 1
    else
      Result := iPos2 - iPos;
  end else
    Result := 0;
end;

function Get30TermIdx : integer;
var
  yy, mm, dd, hh, nn, ss, zz : word;
begin
  DecodeDateTime( now, yy, mm, dd, hh, nn, ss, zz );
  Result  := ( hh mod 24 * 2 ) + ( nn div 30 );
end;


function GetTimestamp(len: Integer): string;
var
  ss: string;
begin
  if len = 13 then
  begin
    ss := DateTimeToTimeStamp(now).time .ToString;
    Result := IntToStr(DateTimeToUnix(Now,false)) + Copy(ss,Length(ss) - 2,Length(ss) );
  end
  else if len = 10 then
  begin
    Result := IntToStr(DateTimeToUnix(Now,false));
  end
end;

function UnixTimeToDateTime( UnixTime : int64; len : integer ) : TDateTime;
var
  dtTime : TDateTime;
  sTmp   : string;
  iDiv, iMod : Integer;
begin
// 1645961186247
	if len = 16 then
		iDiv := 1000000  
  else if len = 13 then 
  	iDiv := 1000
  else if len = 10 then
		iDiv := 1;

  dtTime:= UnixToDateTime(UnixTime div iDiv , false);
  sTmp 	:= Format( '%s.%03d', [ FormatDateTime('yyyy-mm-dd hh:nn:ss', dtTime ),  UnixTime mod iDiv ]);
  Result:= EncodeDate(  StrToInt(copy(sTmp, 1, 4 ) ) , StrToInt( copy(sTmp, 6, 2 )) , StrToInt( copy(sTmp, 9,2)) )
          + EnCodeTime(StrToInt(copy(sTmp, 12,2))
                    ,StrToInt(copy(sTmp, 15,2))
                    ,StrToInt(copy(sTmp, 18,2))
                    ,StrToInt(copy(sTmp, 21,3)) )  ;
end;


{
// itype = 0 : "2022-03-25T20:00:00"  no millisecond
// itype = 1 : "11:22:33"
}
function GetStrToTime(sTime : string; iType: integer) : TDateTime;
begin

  if iType = 0 then begin
  //  "2022-03-25T20:00:00"
  REsult := EncodeDate(  StrToInt(copy(sTime, 1, 4 ) ) , StrToInt( copy(sTime, 6, 2 )) , StrToInt( copy(sTime, 9,2)) )
          + EnCodeTime(StrToInt(copy(sTime, 12,2))
                    ,StrToInt(copy(sTime, 15,2))
                    ,StrToInt(copy(sTime, 18,2))
                    ,0 )  ;
  end else
  if iType = 1 then
  begin
  //  "20:00:00"
  REsult := Date + EnCodeTime(StrToInt(copy(sTime, 1,2))
                    ,StrToInt(copy(sTime, 4,2))
                    ,StrToInt(copy(sTime, 7,2))
                    ,0 )  ;
  end;
end;


procedure ExecuteApp( aHandle : HWND; sClassName, sAppName : string );
var
  aH : THandle;
begin
  aH := FindWindow( PChar(sClassName), nil );
  if aH <= 0 then
    ShellExecute(aHandle, nil, PChar(sAppName), nil, nil, SW_NORMAL);
end;

procedure CloseApp( sClassName : string );
var
  aH : THandle;
begin
  aH := FindWindow( PChar(sClassName), nil );
  if aH > 0 then
    SendMessage( aH, WM_CLOSE, 0, 0 )
end;

procedure DeleteLine( aGrid : TStringGrid; iline: Integer);
begin
  with aGrid do begin
    TmpGrid(aGrid).DeleteRow(iline);
    Rows[rowcount].Clear;
  end;
end;

procedure InsertLine( aGrid : TStringGrid; iline: Integer);
begin
  with aGrid do begin
    RowCount := Succ( RowCount );
    TmpGrid( aGrid ).MoveRow( ( RowCount - 1 ), iline );
    Rows[iline].Clear;
  end;
end;

procedure InvalidateRow( aGrid : TStringGrid; iline : integer );
begin
  TmpGrid( aGrid ).InvalidateRow( iline );
end;

procedure InitGrid( aGrid : TStringGrid; bClear : boolean; FixedCnt : integer );
var
  i : integer;
begin
  for I := FixedCnt to aGrid.RowCount-1 do
    aGrid.Rows[i].Clear;

  if bClear then
    aGrid.RowCount := FixedCnt;
end;

procedure SetColor(dVal: double; aGrid: TStringGrid; iCol,
  iRow: integer);
begin
  with aGrid do
    if dVal > 0  then
      Objects[ iCol, iRow] := Pointer( LONG_FLAG )
    else if dVal < 0 then
      Objects[ iCol, iRow] := Pointer( SHORT_FLAG )
    else
      Objects[ iCol, iRow] := Pointer(NONE_FLAG);
//    if dVal > 0  then
//      Objects[ iCol, iRow] := Pointer( TColor( LONG_COLOR ))
//    else if dVal < 0 then
//      Objects[ iCol, iRow] := Pointer( TColor( SHORT_COLOR ))
//    else
//      Objects[ iCol, iRow] := Pointer(TColor( clBlack ));
end;

// 0 : back,  1 : font  , 2 :  red or blue 만 선택되는..
function  GetColor(iType, iColorFlag : integer) : TColor;
begin
//  case iColorFlag of
//    LONG_FLAG :  if iType = 0 then  Result := LONG_COLOR else Result := clRed;
//    SHORT_FLAG :  if iType = 0 then  Result := SHORT_COLOR else Result := clBlue;
//    else if iType = 0 then Result := clWhite else Result := clBlack;
//  end;

//  case iColorFlag of
//    LONG_FLAG :  if iType = 0 then  Result :=  clRed  else Result := clRed;
//    SHORT_FLAG :  if iType = 0 then  Result := $00F0B000 else Result := clBlue;
//    else if iType = 0 then Result := clWhite else Result := clBlack;
//  end;


  case iColorFlag of
    LONG_FLAG :
      case iType of
        0 : Result := clWhite;
        1 : Result := clBlack;
        2 : Result := clRed;
      end;
    SHORT_FLAG :
       case iType of
        0 : Result := clGray;
        1 : Result := clWhite;
        2 : Result := clBlue;
      end;
    else
      case iType of
        0 : Result := clWhite;
        1 : Result := clBlack;
        2 : Result := clBlack;
      end;
  end;
end;

function EnumFamToLines(lplf: PLOGFONT; lpntm: PNEWTEXTMETRIC; FontType: DWORD; Lines: LPARAM): Integer; stdcall;
begin
  with lplf^ do // 한글 폰트와 @붙지 않은 폰트만 검색
    if {(lfCharSet=HANGEUL_CHARSET) and} Pos('@', lplf^.lfFaceName)=0 then
      TStrings(Lines).Add(lplf.lfFaceName);
  Result := 1;

//  TStrings(Lines).Add(lplf.lfFaceName);
//  Result:=1;
end;


procedure ComboBox_AutoWidth(const theComboBox: TCombobox);
const
  HORIZONTAL_PADDING = 4;
var
  itemsFullWidth: integer;
  idx: integer;
  itemWidth: integer;
begin

  itemsFullWidth := 0;
  for idx := 0 to -1 + theComboBox.Items.Count do
  begin
    itemWidth := theComboBox.Canvas.TextWidth(theComboBox.Items[idx]);
    Inc(itemWidth, 2 * HORIZONTAL_PADDING);
    if (itemWidth > itemsFullWidth) then itemsFullWidth := itemWidth;
  end;

  if (itemsFullWidth > theComboBox.Width) then
  begin

    if theComboBox.DropDownCount < theComboBox.Items.Count then
      itemsFullWidth := itemsFullWidth + GetSystemMetrics(SM_CXVSCROLL);

    SendMessage(theComboBox.Handle, CB_SETDROPPEDWIDTH, itemsFullWidth, 0);
  end;
end;




function GetColor( d : double ) : TColor;
begin
  Result := clBlack;
  if d > EPSILON  then
    Result := clRed
  else if d < 0 then
    Result := clBlue;
end;


function CheckZero( dVal : double ) : boolean;
begin
	Result := IsZero( dVal );
end;



function EncodePath(sRrsc, sPoint, sTime : string): string;
var
	sValue : string;
begin
	sValue := HTTPEncode(UTF8Encode(sPoint));
  sValue := StringReplace(sValue, '+', '%20', [rfReplaceAll]);
  sValue := StringReplace(sValue, '%21', '!', [rfReplaceAll]);
  sValue := StringReplace(sValue, '%27', '''', [rfReplaceAll]);
  sValue := StringReplace(sValue, '%28', '(', [rfReplaceAll]);
  sValue := StringReplace(sValue, '%29', ')', [rfReplaceAll]);
  sValue := StringReplace(sValue, '%26', '&', [rfReplaceAll]);
  sValue := StringReplace(sValue, '%3D', '=', [rfReplaceAll]);
  sValue := StringReplace(sValue, '%7E', '~', [rfReplaceAll]);

  Result := sRrsc + chr(0) + sValue +  chr(0 ) + sTime;
end;


end.
