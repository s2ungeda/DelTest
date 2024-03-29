unit UStorage;

interface

uses
  Classes, SysUtils, Math,

  UParsers;

type
  TStorageField = class(TCollectionItem)
  private
    FName: String;
    FValue: String;
    function GetText: String;
    procedure SetText(const Value: String);

    function GetBoolean: Boolean;
    function GetDouble: Double;
    function GetInteger: Integer;
    function GetString: String;
    procedure SetBoolean(const Value: Boolean);
    procedure SetDouble(const Value: Double);
    procedure SetInteger(const Value: Integer);
    procedure SetString(const Value: String);
  public
      // 'name=value'
    property Text: String read GetText write SetText;
      // 'value' only
    property AsString: String read GetString write SetString;
    property AsFloat: Double read GetDouble write SetDouble;
    property AsInteger: Integer read GetInteger write SetInteger;
    property AsBoolean: Boolean read GetBoolean write SetBoolean;

    function AsTimeDef( const dValue : Double ) : Double;
    function AsStringDef( const stValue : string ) : string;
    function AsBooleanDef( const bValue : Boolean ) : Boolean;
    // 0 이 돼선 안될때..사용
    function AsIntegerDef( const iValue : integer ) : integer;
  end;

  TStorageFields = class(TCollection)
  private
    function GetText: String;
    procedure SetText(const Value: String);
  public
    constructor Create;

    function FieldByName(stField: String): TStorageField;
    property Text: String read GetText write SetText;
  end;

  TStorageItem = class(TCollectionItem)
  private
    FFields: TStorageFields;
  public
    constructor Create(Coll: TCollection); override;
    destructor Destroy; override;
  end;

  TStorage = class(TCollection)
  private
    FRecords: TStringList;
    FPosition: Integer;
  public
    constructor Create;
    destructor Destroy; override;

      // file I/O
    function Load(stFile: String): Boolean;
    function Save(stFile: String): Boolean;

      // navigate
    procedure First;
    procedure Prev;
    procedure Next;
    procedure Last;
    function EOF: Boolean;

      // add
    function New: TStorageItem;

      // value access
    function FieldByName(stField: String): TStorageField;
  end;

implementation

const
  STORAGE_FORMAT_TOKEN = 'Sweet Life Engine Storage File Version 1.0';

{ TStorageField }

function TStorageField.AsBooleanDef(const bValue: Boolean): Boolean;
begin
  if FValue = '' then
    Result := bValue
  else
    Result := CompareStr(FValue, 'True') = 0;
end;

function TStorageField.AsIntegerDef(const iValue: integer): integer;
begin
  Result := GetInteger;
  if Result = 0  then
    Result := iValue;
end;

function TStorageField.AsStringDef(const stValue: string): string;
begin
  Result := FValue;
  if Result = ''  then
    Result := stValue;
end;

function TStorageField.AsTimeDef(const dValue: Double): Double;
begin
  Result  := StrToFloatDef(FValue, 0.0);
  if Result = 0  then
    Result := dValue;
end;

function TStorageField.GetBoolean: Boolean;
begin
  Result := CompareStr(FValue, 'True') = 0;
end;

procedure TStorageField.SetBoolean(const Value: Boolean);
begin
  if Value then
    FValue := 'True'
  else
    FValue := 'False';
end;

function TStorageField.GetDouble: Double;
begin
  Result := StrToFloatDef(FValue, 0.0);
end;

procedure TStorageField.SetDouble(const Value: Double);
begin
  FValue := FloatToStr(Value);
end;

function TStorageField.GetInteger: Integer;
begin
  Result := StrToIntDef(FValue, 0);
end;

procedure TStorageField.SetInteger(const Value: Integer);
begin
  FValue := IntToStr(Value);
end;

function TStorageField.GetString: String;
begin
  Result := FValue;
end;

procedure TStorageField.SetString(const Value: String);
begin
  FValue := Value;
end;

procedure TStorageField.SetText(const Value: String);
var
  iPos: Integer;
begin
    // find separator
  iPos := Pos('=', Value);

    // set name & value
  if iPos > 0 then
  begin
    FName := Copy(Value, 1, iPos-1);
    FValue := Copy(Value, iPos+1, Length(Value) - iPos);
  end else
  begin
    FName := Value;
    FValue := '';
  end;

    // regulate
  FName := UpperCase(Trim(FName));
  FValue := Trim(FValue);
end;

function TStorageField.GetText: String;
begin
  Result := FName + '=' + FValue;
end;

{ TStorageFields }

constructor TStorageFields.Create;
begin
  inherited Create(TStorageField);
end;

function TStorageFields.FieldByName(stField: String): TStorageField;
var
  i: Integer;
begin
  Result := nil;

    // regulate name
  stField := UpperCase(Trim(stField));

    // find
  for i := 0 to Count - 1 do
  begin
    if CompareStr((Items[i] as TStorageField).FName, stField) = 0 then
    begin
      Result := Items[i] as TStorageField;
      Break;
    end;
  end;

  //
  if Result = nil then
  begin
    Result := Add as TStorageField;
    Result.FName := stField;
    Result.FValue := '';
  end;
end;

function TStorageFields.GetText: String;
var
  i: Integer;
begin
  Result := '';

  for i := 0 to Count - 1 do
  begin
    if i > 0 then
      Result := Result + '&';

    Result := Result + (Items[i] as TStorageField).Text;
  end;
end;

procedure TStorageFields.SetText(const Value: String);
var
  aParser: TParser;
  i: Integer;
begin
    // clear fields
  Clear;

    // create parser object
  aParser := TParser.Create(['&']);
  try
      // parse
    aParser.Parse(Value);

      // generage fields
    for i := 0 to aParser.Count - 1 do
      (Add as TStorageField).Text := aParser[i];
  finally
    aParser.Free;
  end;
end;

{ TStorageItem }

constructor TStorageItem.Create(Coll: TCollection);
begin
  inherited Create(Coll);

  FFields := TStorageFields.Create;
end;

destructor TStorageItem.Destroy;
begin
  FFields.Free;

  inherited;
end;

//===========================================================================//

{ TStorage }

constructor TStorage.Create;
begin
  inherited Create(TStorageItem);

  FRecords := TStringList.Create;

  FPosition := -1;
end;

destructor TStorage.Destroy;
begin
  FRecords.Free;

  inherited;
end;

//--------------------------------------------------------------------< value >

function TStorage.FieldByName(stField: String): TStorageField;
var
  iPosition: Integer;
begin
  iPosition := Min(Count-1, FPosition);

  if (iPosition >= 0) and (iPosition <= Count-1) then
    Result := (Items[iPosition] as TStorageItem).FFields.FieldByName(stField)
  else
    Result := nil;
end;

//---------------------------------------------------------------< navigation >

procedure TStorage.First;
begin
  FPosition := 0;
end;

procedure TStorage.Last;
begin
  FPosition := Count-1;
end;

procedure TStorage.Next;
begin
  FPosition := Min(Count, FPosition+1);
end;

procedure TStorage.Prev;
begin
  FPosition := Max(0, FPosition-1);
end;

function TStorage.EOF: Boolean;
begin
  Result := FPosition > Count-1;
end;

//---------------------------------------------------------------------< edit >

function TStorage.New;
begin
  Result := Add as TStorageItem;

  FPosition := Count-1;
end;

//----------------------------------------------------------------< load/save >

function TStorage.Load(stFile: String): Boolean;
var
  i: Integer;
  aItem: TStorageItem;
begin
  Result := False;

    // reset data
  Clear;

    // check the file
  if not FileExists(stFile) then Exit;

    // read text strings from the file
  FRecords.LoadFromFile(stFile);

    // check format
  {if (FRecords.Count = 0) or
     (CompareStr(FRecords[0], STORAGE_FORMAT_TOKEN) <> 0) then
    Exit;}
  if (FRecords.Count = 0) then
    exit;

    // generate storage items
  for i := 1 to FRecords.Count - 1 do
  begin
    aItem := New;

    aItem.FFields.Text := FRecords[i];
  end;

    // move to the first position
  First;

    //
  Result := True;
end;

function TStorage.Save(stFile: String): Boolean;
var
  i: Integer;
begin
  Result := False;

    // reset string list
  FRecords.Clear;

    // add format description
  FRecords.Add(STORAGE_FORMAT_TOKEN);

    // generate string list
  for i := 0 to Count - 1 do
    FRecords.Add((Items[i] as TStorageItem).FFields.Text);

    // save to file
  try
    FRecords.SaveToFile(stFile);

    Result := True;
  except

  end;
end;

end.
