unit UFormBroker;

interface

uses
  Classes, SysUtils, Forms,
    // lemon: utils
  UStorage, Menus, Dialogs, UTypes;

const
  MAX_CNT = 20;

type
  TFormTagItem = class(TCollectionItem)
  private
    FFormID : integer;
    FFormTagList : TStringList;
  public
    constructor Create(aColl : TCollection); override;
    destructor Destroy; override;
  end;

  TFormTags = class(TCollection)
  public
    constructor Create;
    destructor Destroy; override;
    procedure Del( iID, iTag : integer );
    function Find( iID : integer ) : TFormTagItem;
    function GetTag( iID : integer ) : integer;
  end;

  TFormBrokerItem = class(TCollectionItem)
  public
    FFormID: Integer;
    FClassName: String;
    FForm : TForm;
    FSubMenu : TMenuItem;
    constructor Create(aColl : TCollection); override;
    destructor Destroy; override;
  end;

  TFormOpenEvent = procedure(iFormID, iVar: Integer; var aForm: TForm) of object;
  TFormLoadEvent = procedure(iFormID: Integer; aStorage: TStorage; var aForm: TForm) of object;
  TFormSaveEvent = procedure(iFormID: Integer; aStorage: TStorage; aForm: TForm) of object;
  TFormReLoadEvent = procedure( iFormID : integer; aForm : TForm ) of object;

  TFormBroker = class(TCollection)
  private
    FStorage: TStorage;

    FOnOpen: TFormOpenEvent;
    FOnLoad: TFormLoadEvent;
    FOnSave: TFormSaveEvent;
    FSelected: TForm;
    FOnReLoad: TFormReLoadEvent;

    FFormTags : TFormTags;

    function FindClassName(stFormName: String): TFormBrokerItem;
    function FindFormID(iFormID: Integer): TFormBrokerItem;
    function FindForm( aForm : TForm ) : TFormBrokerItem;
    function FindIndex( aForm : TForm ) : integer;
    function FindFormTagID(iFormID, iTag : integer) : TForm;
  public
    constructor Create;
    destructor Destroy; override;

    function Open(iFormID, iVar: Integer): TForm;
    function Load(stFile: String): Boolean;
    function Save(stFile: String): Boolean;

    property OnOpen: TFormOpenEvent read FOnOpen write FOnOpen;
    property OnLoad: TFormLoadEvent read FOnLoad write FOnLoad;
    property OnSave: TFormSaveEvent read FOnSave write FOnSave;
    property OnReLoad : TFormReLoadEvent read FOnReLoad write FOnReLoad;

    //
    property Storage: TStorage read FStorage write FStorage;
    property FormTags : TFormTags read FFormTags;

    property Selected : TForm read FSelected;
    procedure FormActivate( Sender : TObject );
    function FindFormID2(iFormID: Integer): TForm;
    function FindFormLastTag(iFormID: Integer): integer;
    function FindFormMenu( aItem : TMenuItem ) : TForm;

    procedure ReLoad;
    procedure CloseWindow;
    procedure ResetCloseWindow;
    procedure OnFormClosed (Sender: TObject; var Action: TCloseAction);

    procedure DoActivae;
  end;

implementation

uses GApp, GAppForms, GLibs    ;

{ TFormBroker }

procedure TFormBroker.CloseWindow;
var
  i : integer;
  aForm: TForm;
  aItem: TFormBrokerItem;
begin
  for i := 0 to Application.MainForm.ComponentCount - 1 do
  begin
    if not (Application.MainForm.Components[i] is TForm) then Continue;
    aItem := FindClassName((Application.MainForm.Components[i] as TForm).ClassName);
    if aItem = nil then Continue;
    aForm := Application.MainForm.Components[i] as TForm;
      // save only visible forms
    if not aForm.Visible then Continue;
    if aForm <> nil then
      aForm.Close;
  end;
end;

constructor TFormBroker.Create;
begin
  inherited Create(TFormBrokerItem);
  FSelected := nil;
  FStorage := TStorage.Create;
  FFormTags := TFormTags.Create;
end;

destructor TFormBroker.Destroy;
begin
  FStorage.Free;
  FFormTags.Free;
  inherited;
end;

procedure TFormBroker.DoActivae;
var
  aItem : TFormBrokerItem;
begin
  if FSelected = nil then
    Exit;

  aItem := FindForm( FSelected );
  if aItem = nil then
    FSelected := nil
  else
    FSelected.SetFocus;
end;

function TFormBroker.FindForm(aForm: TForm): TFormBrokerItem;
var
  i: Integer;
begin
  Result := nil;

  for i := 0 to Count - 1 do
    if (Items[i] as TFormBrokerItem).FForm = aForm then
    begin
      Result := Items[i] as TFormBrokerItem;
      Break;
    end;
end;

function TFormBroker.FindFormID(iFormID: Integer): TFormBrokerItem;
var
  i: Integer;
begin
  Result := nil;

  for i := 0 to Count - 1 do
    if (Items[i] as TFormBrokerItem).FFormID = iFormID then
    begin
      Result := Items[i] as TFormBrokerItem;
      Break;
    end;
end;

function TFormBroker.FindFormID2(iFormID: Integer): TForm;
var
  i: Integer;
begin
  Result := nil;

  for i := 0 to Count - 1 do
    if (Items[i] as TFormBrokerItem).FFormID = iFormID then
    begin
      Result := (Items[i] as TFormBrokerItem).FForm;
      Break;
    end;
end;

function TFormBroker.FindFormLastTag(iFormID: Integer): integer;
var
  i: Integer;
begin
  Result := 0;

  for i := Count - 1 downto 0 do
    if (Items[i] as TFormBrokerItem).FFormID = iFormID then
    begin
      Result := (Items[i] as TFormBrokerItem).FForm.Tag;
      Break;
    end;
end;

function TFormBroker.FindFormMenu(aItem: TMenuItem): TForm;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if (Items[i] as TFormBrokerItem).FSubMenu = aItem then
    begin
      Result := (Items[i] as TFormBrokerItem).FForm;
      Break;
    end;
end;

function TFormBroker.FindFormTagID(iFormID, iTag: integer): TForm;
var
  i : integer;
begin
  Result := nil;
  for i := Count - 1 downto 0 do
    if ((Items[i] as TFormBrokerItem).FFormID = iFormID) and
       ((Items[i] as TFormBrokerItem).FForm.Tag = iTag) then
    begin
      Result := (Items[i] as TFormBrokerItem).FForm;
      Break;
    end;
end;

function TFormBroker.FindIndex(aForm: TForm): integer;
var
  i: Integer;
begin
  Result := -1;

  for i := 0 to Count - 1 do
    if (Items[i] as TFormBrokerItem).FForm = aForm then
    begin
      Result := i;
      Break;
    end;
end;

procedure TFormBroker.FormActivate(Sender: TObject);

begin
  if Sender <> nil then
  begin
    FSelected := TForm( Sender );

  end;
end;

function TFormBroker.FindClassName(stFormName: String): TFormBrokerItem;
var
  i: Integer;
begin
  Result := nil;

  for i := 0 to Count - 1 do
    if CompareStr(stFormName, (Items[i] as TFormBrokerItem).FClassName) = 0 then
    begin
      Result := Items[i] as TFormBrokerItem;
      Break;
    end;
end;

procedure TFormBroker.OnFormClosed(Sender: TObject; var Action: TCloseAction);
var
  aForm : TForm;
  aItem : TFormBrokerItem;
  aMenu : TMenuItem;
  iIndex : integer;
begin
  aForm := TForm(Sender);
  if aForm = nil then
  begin
    App.Log(llError, '', 'Closed Form ��ã��');
    exit;
  end;

//  if aForm = FrmMain then
//    gEnv.MainBoard := nil;

  aItem := FindForm( aForm );
  if aItem = nil then exit;
  //aMenu := OrpMainForm.Menu.Items.Find('Window');
  //aMenu.Remove(aItem.FSubMenu);
  iIndex := FindIndex(aForm);
  Action := cafree;
  Delete(iIndex);

  FFormTags.Del(aItem.FFormID, aForm.Tag);
end;

function TFormBroker.Open(iFormID, iVar: Integer): TForm;
var
  aItem: TFormBrokerItem;
  aMenu : TMenuItem;
  i, iCnt : integer;
  stMsg : string;
begin
  iCnt := 0;

   // create a form or get existing form
  if Assigned(FOnOpen) then
  try
    FOnOpen(iFormID, iVar, Result);
  except

  end;

    // show and register
  if Result <> nil then
  begin
    aITem := FindForm( Result );
      // register
    if aItem = nil then
    begin
      aItem := Add as TFormBrokerItem;
      with aItem do
      begin
        FFormID := iFormID;
        FClassName := Result.ClassName;
        FSubMenu.Caption := Result.Caption;
        Result.OnClose := OnFormClosed;

        FForm := Result;

      end;
    end;
    Result.Tag := FFormTags.GetTag(aItem.FFormID);
    Result.Show;
  end;
end;

procedure TFormBroker.ReLoad;
var
  i : integer;
  aItem :  TFormBrokerItem;
begin
  for i := 0 to Count - 1 do
  begin
    aItem := Items[i] as TFormBrokerItem;
    if Assigned( FOnReLoad )  then
      FOnReLoad( aItem.FFormID, aItem.FForm );
  end;
end;

procedure TFormBroker.ResetCloseWindow;
var
  i : integer;
  aForm: TForm;
  aItem: TFormBrokerItem;
begin
  for i := Application.MainForm.ComponentCount - 1 downto 0 do
  begin
    if Application.MainForm.Components[i] is TForm then
    begin
      aItem := FindClassName((Application.MainForm.Components[i] as TForm).ClassName);
      if aItem = nil then Continue;
      aForm := Application.MainForm.Components[i] as TForm;
        // save only visible forms
      if not aForm.Visible then Continue;
      if (aForm <> nil) then
        aForm.Free;
    end;
  end;
end;

function TFormBroker.Load(stFile: String): Boolean;
var
  i, iFormID: Integer;
  aForm: TForm;
  aItem: TFormBrokerItem;
  aMenu : TMenuItem;
begin
    // load storage
  FStorage.Load(stFile);
    // move to the start of the storage
  FStorage.First;

  Result := false;
    //
  while not FStorage.EOF do
  begin
    Result := true;
    iFormID := FStorage.FieldByName('FormID').AsInteger;

    if Assigned(FOnLoad) then
    begin
      aForm := nil;

      try
        FOnLoad(iFormID, FStorage, aForm);
      except

      end;

      if aForm <> nil then
      begin
          // register
        //aItem := FindFormID(iFormID);

        aITem := FindForm( aForm );
        if aItem = nil then
        begin
          aItem := Add as TFormBrokerItem;
          with aItem do
          begin
            FFormID := iFormID;
            FClassName := aForm.ClassName;
            //aMenu := OrpMainForm.Menu.Items.Find('Window');
            FSubMenu.Caption := aForm.Caption;
            //aMenu.Add(FSubMenu);
            aForm.OnClose := OnFormClosed;
            FForm := aForm;

          end;
        end;

        aForm.Tag := FFormTags.GetTag(aItem.FFormID);
          // show form
        aForm.Show;
          // set default
        aForm.Left := FStorage.FieldByName('Left').AsInteger;
        aForm.Top := FStorage.FieldByName('Top').AsInteger;
        aForm.Width := FStorage.FieldByName('width').AsInteger;
        aForm.Height := FStorage.FieldByName('Height').AsInteger;

        case FStorage.FieldByName('WindowState').AsInteger of
          0: aForm.WindowState := wsNormal;
          1: aForm.WindowState := wsMinimized;
          2: aForm.WindowState := wsMaximized;
        end;

      end;
    end;

    FStorage.Next;
  end;
end;

function TFormBroker.Save(stFile: String): Boolean;
var
  i: Integer;
  aForm: TForm;
  aItem: TFormBrokerItem;
begin
    // reset storage
  FStorage.Clear;

  if Application.MainForm = nil then
    Exit;
    // build storage

      // common
  if Assigned(FOnSave) then
  try
    FStorage.New;
    FStorage.FieldByName('FormID').AsInteger := 9999;

  except

  end;

  for i := 0 to Application.MainForm.ComponentCount - 1 do
  begin
    if not (Application.MainForm.Components[i] is TForm) then Continue;

    aItem := FindClassName((Application.MainForm.Components[i] as TForm).ClassName);
    if aItem = nil then Continue;

    aForm := Application.MainForm.Components[i] as TForm;
      // save only visible forms
    if not aForm.Visible then Continue;

      // new storage record
    FStorage.New;

      // common
    FStorage.FieldByName('FormID').AsInteger := aItem.FFormID;
    FStorage.FieldByName('Left').AsInteger := aForm.Left;
    FStorage.FieldByName('Top').AsInteger := aForm.Top;
    FStorage.FieldByName('width').AsInteger := aForm.Width;
    FStorage.FieldByName('Height').AsInteger := aForm.Height;
    case aForm.WindowState of
      wsNormal: FStorage.FieldByName('WindowState').AsInteger := 0;
      wsMinimized: FStorage.FieldByName('WindowState').AsInteger := 1;
      wsMaximized: FStorage.FieldByName('WindowState').AsInteger := 2;
    end;

      // get custom parameter
    if Assigned(FOnSave) then
    try
      FOnSave(aItem.FFormID, FStorage, aForm);
    except

    end;
  end;

  if Assigned(FOnSave) then
  try
    FStorage.New;
    FStorage.FieldByName('FormID').AsInteger := ID_DALIN_MAIN;
    FOnSave(ID_DALIN_MAIN, FStorage, aForm);
  except

  end;
    // save storage
  FStorage.Save(stFile);
end;

{ TFormBrokerItem }

constructor TFormBrokerItem.Create(aColl: TCollection);
begin
  inherited Create(aColl);
  FSubMenu := TMenuItem.Create(nil);
end;

destructor TFormBrokerItem.Destroy;
begin
  FSubMenu.Free;
  inherited;
end;

{ TFormTagItem }

constructor TFormTagItem.Create(aColl: TCollection);
begin
  inherited Create(aColl);
  FFormTagList := TStringList.Create;
end;

destructor TFormTagItem.Destroy;
begin
  FFormTagList.Free;
  inherited;
end;

{ TFormTags }

constructor TFormTags.Create;
begin
  inherited Create(TFormTagItem);
end;

procedure TFormTags.Del(iID, iTag: integer);
var
  aItem : TFormTagItem;
begin
  aItem := Find(iID);
  if aItem = nil then exit;
  if (iTag <= 0) or (iTag >= Count) then exit;
  aItem.FFormTagList.Strings[iTag-1] := 'X';
end;

destructor TFormTags.Destroy;
begin

  inherited;
end;

function TFormTags.Find(iID: integer): TFormTagItem;
var
  i : integer;
  aItem : TFormTagItem;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    aItem := Items[i] as TFormTagItem;
    if aItem.FFormID = iID then
    begin
      Result := aItem;
      break;
    end;
  end;
end;

function TFormTags.GetTag(iID: integer): integer;
var
  i : integer;
  aItem : TFormTagItem;
  bFind : boolean;
begin
  Result := 1;
  aItem := Find(iID);
  if aItem = nil then
  begin
    aItem := Add as TFormTagItem;
    aItem.FFormID := iID;
    for i := 0 to MAX_CNT-1 do
      aItem.FFormTagList.Add('X');
    aItem.FFormTagList.Strings[0] := 'O';
  end else
  begin
    bFind := false;
    for i := 0 to aItem.FFormTagList.Count - 1 do
    begin
      if aItem.FFormTagList.Strings[i] = 'X' then
      begin
        Result := i +1;
        aItem.FFormTagList.Strings[i] := 'O';
        bFind := true;
        break;
      end;
    end;

    if not bFind  then
      Result := MAX_CNT + 1;
  end;
end;
end.
