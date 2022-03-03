unit XA_DATASETLib_TLB;

// ************************************************************************ //
// WARNING                                                                    
// -------                                                                    
// The types declared in this file were generated from data read from a       
// Type Library. If this type library is explicitly or indirectly (via        
// another type library referring to this type library) re-imported, or the   
// 'Refresh' command of the Type Library Editor activated while editing the   
// Type Library, the contents of this file will be regenerated and all        
// manual modifications will be lost.                                         
// ************************************************************************ //

// $Rev: 98336 $
// File generated on 2022-03-03 오후 9:51:18 from Type Library described below.

// ************************************************************************  //
// Type Lib: C:\Dalin\Bin\XA_DataSet.dll (1)
// LIBID: {CAA15009-CF22-4D1D-AF40-093DBC5A6A0F}
// LCID: 0
// Helpfile: 
// HelpString: eBest Xing DataSet Lib
// DepndLst: 
//   (1) v2.0 stdole, (C:\Windows\SysWOW64\stdole2.tlb)
// SYS_KIND: SYS_WIN32
// ************************************************************************ //
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers. 
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}
{$VARPROPSETTER ON}
{$ALIGN 4}

interface

uses Winapi.Windows, System.Classes, System.Variants, System.Win.StdVCL, Vcl.Graphics, Vcl.OleCtrls, Vcl.OleServer, Winapi.ActiveX;
  


// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:        
//   Type Libraries     : LIBID_xxxx                                      
//   CoClasses          : CLASS_xxxx                                      
//   DISPInterfaces     : DIID_xxxx                                       
//   Non-DISP interfaces: IID_xxxx                                        
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  XA_DATASETLibMajorVersion = 1;
  XA_DATASETLibMinorVersion = 0;

  LIBID_XA_DATASETLib: TGUID = '{CAA15009-CF22-4D1D-AF40-093DBC5A6A0F}';

  DIID__IXAQueryEvents: TGUID = '{AAF89E20-1F84-4B1F-B6EE-617B6F2C9CD4}';
  IID_IXAQuery: TGUID = '{255B43AE-B290-4435-9BA7-37FCAAD04D77}';
  IID_IXAReal: TGUID = '{ED0FC93A-7879-4C0D-BA8F-71A7E2B5A737}';
  CLASS_XAQuery: TGUID = '{781520A9-4C8C-433B-AA6E-EE9E94108639}';
  DIID__IXARealEvents: TGUID = '{16602768-2C96-4D93-984B-E36E7E35BFBE}';
  CLASS_XAReal: TGUID = '{4D654021-F9D9-49F7-B2F9-6529A19746F7}';
type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  _IXAQueryEvents = dispinterface;
  IXAQuery = interface;
  IXAQueryDisp = dispinterface;
  IXAReal = interface;
  IXARealDisp = dispinterface;
  _IXARealEvents = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  XAQuery = IXAQuery;
  XAReal = IXAReal;


// *********************************************************************//
// DispIntf:  _IXAQueryEvents
// Flags:     (4096) Dispatchable
// GUID:      {AAF89E20-1F84-4B1F-B6EE-617B6F2C9CD4}
// *********************************************************************//
  _IXAQueryEvents = dispinterface
    ['{AAF89E20-1F84-4B1F-B6EE-617B6F2C9CD4}']
    procedure ReceiveData(const szTrCode: WideString); dispid 1;
    procedure ReceiveMessage(bIsSystemError: WordBool; const nMessageCode: WideString; 
                             const szMessage: WideString); dispid 2;
    procedure ReceiveChartRealData(const szTrCode: WideString); dispid 3;
    procedure ReceiveSearchRealData(const szTrCode: WideString); dispid 4;
  end;

// *********************************************************************//
// Interface: IXAQuery
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {255B43AE-B290-4435-9BA7-37FCAAD04D77}
// *********************************************************************//
  IXAQuery = interface(IDispatch)
    ['{255B43AE-B290-4435-9BA7-37FCAAD04D77}']
    function GetFieldData(const szBlockName: WideString; const szFieldName: WideString; 
                          nRecordIndex: Integer): WideString; safecall;
    function Request(bNext: WordBool): Integer; safecall;
    function Get_ResFileName: WideString; safecall;
    procedure Set_ResFileName(const pVal: WideString); safecall;
    function LoadFromResFile(const szFileName: WideString): WordBool; safecall;
    function GetTrCode: WideString; safecall;
    function GetTrDesc: WideString; safecall;
    procedure GetBlockInfo(const szFieldName: WideString; var szNameK: WideString; 
                           var szNameE: WideString; var nRecordType: Integer); safecall;
    procedure SetFieldData(const szBlockName: WideString; const szFieldName: WideString; 
                           nOccursIndex: Integer; const szData: WideString); safecall;
    procedure GetFieldInfo(const szFieldName: WideString; const szItemName: WideString; 
                           var nItemType: Integer; var nDataSize: Integer; var nDotPoint: Integer; 
                           var nOffSet: Integer); safecall;
    function GetBlockType(const szBlockName: WideString): Integer; safecall;
    function GetResData: WideString; safecall;
    function GetBlockSize(const szBlockName: WideString): Integer; safecall;
    function GetFieldDescList(const szBlockName: WideString): WideString; safecall;
    function Get_IsNext: WordBool; safecall;
    function Get_ContinueKey: WideString; safecall;
    function GetBlockCount(const szBlockName: WideString): Integer; safecall;
    procedure SetBlockCount(const szBlockName: WideString; nCount: Integer); safecall;
    procedure ClearBlockdata(const szFieldName: WideString); safecall;
    function GetLastError: Integer; safecall;
    function GetErrorMessage(nErrorCode: Integer): WideString; safecall;
    function GetAccountList(nIndex: Integer): WideString; safecall;
    function GetAccountListCount: Integer; safecall;
    function GetBlockData(const szBlockName: WideString): WideString; safecall;
    function RequestService(const szCode: WideString; const szData: WideString): Integer; safecall;
    function RemoveService(const szCode: WideString; const szData: WideString): Integer; safecall;
    function RequestLinkToHTS(const szLinkName: WideString; const szData: WideString; 
                              const szFiller: WideString): WordBool; safecall;
    function Decompress(const szBlockName: WideString): Integer; safecall;
    function GetTRCountPerSec(const szCode: WideString): Integer; safecall;
    function GetAccountName(const szAcc: WideString): WideString; safecall;
    function GetAcctDetailName(const szAcc: WideString): WideString; safecall;
    function GetAcctNickname(const szAcc: WideString): WideString; safecall;
    function GetFieldChartRealData(const szBlockName: WideString; const szFieldName: WideString): WideString; safecall;
    function GetAttribute(const szBlockName: WideString; const szFieldName: WideString; 
                          const szAttribute: WideString; nRecordIndex: Integer): WideString; safecall;
    function GetTRCountBaseSec(const szCode: WideString): Integer; safecall;
    function GetTRCountRequest(const szCode: WideString): Integer; safecall;
    function GetTRCountLimit(const szCode: WideString): Integer; safecall;
    function GetFieldSearchRealData(const szBlockName: WideString; const szFieldName: WideString): WideString; safecall;
    procedure SetProgramOrder(bProgramOrder: WordBool); safecall;
    function GetProgramOrder: WordBool; safecall;
    property ResFileName: WideString read Get_ResFileName write Set_ResFileName;
    property IsNext: WordBool read Get_IsNext;
    property ContinueKey: WideString read Get_ContinueKey;
  end;

// *********************************************************************//
// DispIntf:  IXAQueryDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {255B43AE-B290-4435-9BA7-37FCAAD04D77}
// *********************************************************************//
  IXAQueryDisp = dispinterface
    ['{255B43AE-B290-4435-9BA7-37FCAAD04D77}']
    function GetFieldData(const szBlockName: WideString; const szFieldName: WideString; 
                          nRecordIndex: Integer): WideString; dispid 2;
    function Request(bNext: WordBool): Integer; dispid 3;
    property ResFileName: WideString dispid 5;
    function LoadFromResFile(const szFileName: WideString): WordBool; dispid 6;
    function GetTrCode: WideString; dispid 7;
    function GetTrDesc: WideString; dispid 8;
    procedure GetBlockInfo(const szFieldName: WideString; var szNameK: WideString; 
                           var szNameE: WideString; var nRecordType: Integer); dispid 14;
    procedure SetFieldData(const szBlockName: WideString; const szFieldName: WideString; 
                           nOccursIndex: Integer; const szData: WideString); dispid 15;
    procedure GetFieldInfo(const szFieldName: WideString; const szItemName: WideString; 
                           var nItemType: Integer; var nDataSize: Integer; var nDotPoint: Integer; 
                           var nOffSet: Integer); dispid 17;
    function GetBlockType(const szBlockName: WideString): Integer; dispid 18;
    function GetResData: WideString; dispid 19;
    function GetBlockSize(const szBlockName: WideString): Integer; dispid 23;
    function GetFieldDescList(const szBlockName: WideString): WideString; dispid 24;
    property IsNext: WordBool readonly dispid 29;
    property ContinueKey: WideString readonly dispid 30;
    function GetBlockCount(const szBlockName: WideString): Integer; dispid 31;
    procedure SetBlockCount(const szBlockName: WideString; nCount: Integer); dispid 32;
    procedure ClearBlockdata(const szFieldName: WideString); dispid 33;
    function GetLastError: Integer; dispid 34;
    function GetErrorMessage(nErrorCode: Integer): WideString; dispid 35;
    function GetAccountList(nIndex: Integer): WideString; dispid 36;
    function GetAccountListCount: Integer; dispid 37;
    function GetBlockData(const szBlockName: WideString): WideString; dispid 38;
    function RequestService(const szCode: WideString; const szData: WideString): Integer; dispid 39;
    function RemoveService(const szCode: WideString; const szData: WideString): Integer; dispid 40;
    function RequestLinkToHTS(const szLinkName: WideString; const szData: WideString; 
                              const szFiller: WideString): WordBool; dispid 41;
    function Decompress(const szBlockName: WideString): Integer; dispid 42;
    function GetTRCountPerSec(const szCode: WideString): Integer; dispid 43;
    function GetAccountName(const szAcc: WideString): WideString; dispid 44;
    function GetAcctDetailName(const szAcc: WideString): WideString; dispid 45;
    function GetAcctNickname(const szAcc: WideString): WideString; dispid 46;
    function GetFieldChartRealData(const szBlockName: WideString; const szFieldName: WideString): WideString; dispid 47;
    function GetAttribute(const szBlockName: WideString; const szFieldName: WideString; 
                          const szAttribute: WideString; nRecordIndex: Integer): WideString; dispid 48;
    function GetTRCountBaseSec(const szCode: WideString): Integer; dispid 49;
    function GetTRCountRequest(const szCode: WideString): Integer; dispid 50;
    function GetTRCountLimit(const szCode: WideString): Integer; dispid 51;
    function GetFieldSearchRealData(const szBlockName: WideString; const szFieldName: WideString): WideString; dispid 52;
    procedure SetProgramOrder(bProgramOrder: WordBool); dispid 53;
    function GetProgramOrder: WordBool; dispid 54;
  end;

// *********************************************************************//
// Interface: IXAReal
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {ED0FC93A-7879-4C0D-BA8F-71A7E2B5A737}
// *********************************************************************//
  IXAReal = interface(IDispatch)
    ['{ED0FC93A-7879-4C0D-BA8F-71A7E2B5A737}']
    function Get_ResFileName: WideString; safecall;
    procedure Set_ResFileName(const pVal: WideString); safecall;
    function GetTrCode: WideString; safecall;
    function LoadFromResFile(const szFileName: WideString): WordBool; safecall;
    procedure SetFieldData(const szBlockName: WideString; const szFieldName: WideString; 
                           const szData: WideString); safecall;
    function GetFieldData(const szBlockName: WideString; const szFieldName: WideString): WideString; safecall;
    procedure AdviseRealData; safecall;
    procedure UnadviseRealData; safecall;
    procedure UnadviseRealDataWithKey(const szCode: WideString); safecall;
    procedure AdviseLinkFromHTS; safecall;
    procedure UnAdviseLinkFromHTS; safecall;
    function GetBlockData(const szBlockName: WideString): WideString; safecall;
    property ResFileName: WideString read Get_ResFileName write Set_ResFileName;
  end;

// *********************************************************************//
// DispIntf:  IXARealDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {ED0FC93A-7879-4C0D-BA8F-71A7E2B5A737}
// *********************************************************************//
  IXARealDisp = dispinterface
    ['{ED0FC93A-7879-4C0D-BA8F-71A7E2B5A737}']
    property ResFileName: WideString dispid 1;
    function GetTrCode: WideString; dispid 6;
    function LoadFromResFile(const szFileName: WideString): WordBool; dispid 7;
    procedure SetFieldData(const szBlockName: WideString; const szFieldName: WideString; 
                           const szData: WideString); dispid 8;
    function GetFieldData(const szBlockName: WideString; const szFieldName: WideString): WideString; dispid 11;
    procedure AdviseRealData; dispid 12;
    procedure UnadviseRealData; dispid 13;
    procedure UnadviseRealDataWithKey(const szCode: WideString); dispid 14;
    procedure AdviseLinkFromHTS; dispid 15;
    procedure UnAdviseLinkFromHTS; dispid 16;
    function GetBlockData(const szBlockName: WideString): WideString; dispid 17;
  end;

// *********************************************************************//
// DispIntf:  _IXARealEvents
// Flags:     (4096) Dispatchable
// GUID:      {16602768-2C96-4D93-984B-E36E7E35BFBE}
// *********************************************************************//
  _IXARealEvents = dispinterface
    ['{16602768-2C96-4D93-984B-E36E7E35BFBE}']
    procedure ReceiveRealData(const szTrCode: WideString); dispid 1;
    procedure RecieveLinkData(const szLinkName: WideString; const szData: WideString; 
                              const szFiller: WideString); dispid 2;
  end;


// *********************************************************************//
// OLE Control Proxy class declaration
// Control Name     : TXAQuery
// Help String      : XAQuery Class
// Default Interface: IXAQuery
// Def. Intf. DISP? : No
// Event   Interface: _IXAQueryEvents
// TypeFlags        : (2) CanCreate
// *********************************************************************//
  TXAQueryReceiveData = procedure(ASender: TObject; const szTrCode: WideString) of object;
  TXAQueryReceiveMessage = procedure(ASender: TObject; bIsSystemError: WordBool; 
                                                       const nMessageCode: WideString; 
                                                       const szMessage: WideString) of object;
  TXAQueryReceiveChartRealData = procedure(ASender: TObject; const szTrCode: WideString) of object;
  TXAQueryReceiveSearchRealData = procedure(ASender: TObject; const szTrCode: WideString) of object;

  TXAQuery = class(TOleControl)
  private
    FOnReceiveData: TXAQueryReceiveData;
    FOnReceiveMessage: TXAQueryReceiveMessage;
    FOnReceiveChartRealData: TXAQueryReceiveChartRealData;
    FOnReceiveSearchRealData: TXAQueryReceiveSearchRealData;
    FIntf: IXAQuery;
    function  GetControlInterface: IXAQuery;
  protected
    procedure CreateControl;
    procedure InitControlData; override;
  public
    function GetFieldData(const szBlockName: WideString; const szFieldName: WideString; 
                          nRecordIndex: Integer): WideString;
    function Request(bNext: WordBool): Integer;
    function LoadFromResFile(const szFileName: WideString): WordBool;
    function GetTrCode: WideString;
    function GetTrDesc: WideString;
    procedure GetBlockInfo(const szFieldName: WideString; var szNameK: WideString; 
                           var szNameE: WideString; var nRecordType: Integer);
    procedure SetFieldData(const szBlockName: WideString; const szFieldName: WideString; 
                           nOccursIndex: Integer; const szData: WideString);
    procedure GetFieldInfo(const szFieldName: WideString; const szItemName: WideString; 
                           var nItemType: Integer; var nDataSize: Integer; var nDotPoint: Integer; 
                           var nOffSet: Integer);
    function GetBlockType(const szBlockName: WideString): Integer;
    function GetResData: WideString;
    function GetBlockSize(const szBlockName: WideString): Integer;
    function GetFieldDescList(const szBlockName: WideString): WideString;
    function GetBlockCount(const szBlockName: WideString): Integer;
    procedure SetBlockCount(const szBlockName: WideString; nCount: Integer);
    procedure ClearBlockdata(const szFieldName: WideString);
    function GetLastError: Integer;
    function GetErrorMessage(nErrorCode: Integer): WideString;
    function GetAccountList(nIndex: Integer): WideString;
    function GetAccountListCount: Integer;
    function GetBlockData(const szBlockName: WideString): WideString;
    function RequestService(const szCode: WideString; const szData: WideString): Integer;
    function RemoveService(const szCode: WideString; const szData: WideString): Integer;
    function RequestLinkToHTS(const szLinkName: WideString; const szData: WideString; 
                              const szFiller: WideString): WordBool;
    function Decompress(const szBlockName: WideString): Integer;
    function GetTRCountPerSec(const szCode: WideString): Integer;
    function GetAccountName(const szAcc: WideString): WideString;
    function GetAcctDetailName(const szAcc: WideString): WideString;
    function GetAcctNickname(const szAcc: WideString): WideString;
    function GetFieldChartRealData(const szBlockName: WideString; const szFieldName: WideString): WideString;
    function GetAttribute(const szBlockName: WideString; const szFieldName: WideString; 
                          const szAttribute: WideString; nRecordIndex: Integer): WideString;
    function GetTRCountBaseSec(const szCode: WideString): Integer;
    function GetTRCountRequest(const szCode: WideString): Integer;
    function GetTRCountLimit(const szCode: WideString): Integer;
    function GetFieldSearchRealData(const szBlockName: WideString; const szFieldName: WideString): WideString;
    procedure SetProgramOrder(bProgramOrder: WordBool);
    function GetProgramOrder: WordBool;
    property  ControlInterface: IXAQuery read GetControlInterface;
    property  DefaultInterface: IXAQuery read GetControlInterface;
    property IsNext: WordBool index 29 read GetWordBoolProp;
    property ContinueKey: WideString index 30 read GetWideStringProp;
  published
    property Anchors;
    property ResFileName: WideString index 5 read GetWideStringProp write SetWideStringProp stored False;
    property OnReceiveData: TXAQueryReceiveData read FOnReceiveData write FOnReceiveData;
    property OnReceiveMessage: TXAQueryReceiveMessage read FOnReceiveMessage write FOnReceiveMessage;
    property OnReceiveChartRealData: TXAQueryReceiveChartRealData read FOnReceiveChartRealData write FOnReceiveChartRealData;
    property OnReceiveSearchRealData: TXAQueryReceiveSearchRealData read FOnReceiveSearchRealData write FOnReceiveSearchRealData;
  end;


// *********************************************************************//
// OLE Control Proxy class declaration
// Control Name     : TXAReal
// Help String      : XAReal Class
// Default Interface: IXAReal
// Def. Intf. DISP? : No
// Event   Interface: _IXARealEvents
// TypeFlags        : (2) CanCreate
// *********************************************************************//
  TXARealReceiveRealData = procedure(ASender: TObject; const szTrCode: WideString) of object;
  TXARealRecieveLinkData = procedure(ASender: TObject; const szLinkName: WideString; 
                                                       const szData: WideString; 
                                                       const szFiller: WideString) of object;

  TXAReal = class(TOleControl)
  private
    FOnReceiveRealData: TXARealReceiveRealData;
    FOnRecieveLinkData: TXARealRecieveLinkData;
    FIntf: IXAReal;
    function  GetControlInterface: IXAReal;
  protected
    procedure CreateControl;
    procedure InitControlData; override;
  public
    function GetTrCode: WideString;
    function LoadFromResFile(const szFileName: WideString): WordBool;
    procedure SetFieldData(const szBlockName: WideString; const szFieldName: WideString; 
                           const szData: WideString);
    function GetFieldData(const szBlockName: WideString; const szFieldName: WideString): WideString;
    procedure AdviseRealData;
    procedure UnadviseRealData;
    procedure UnadviseRealDataWithKey(const szCode: WideString);
    procedure AdviseLinkFromHTS;
    procedure UnAdviseLinkFromHTS;
    function GetBlockData(const szBlockName: WideString): WideString;
    property  ControlInterface: IXAReal read GetControlInterface;
    property  DefaultInterface: IXAReal read GetControlInterface;
  published
    property Anchors;
    property ResFileName: WideString index 1 read GetWideStringProp write SetWideStringProp stored False;
    property OnReceiveRealData: TXARealReceiveRealData read FOnReceiveRealData write FOnReceiveRealData;
    property OnRecieveLinkData: TXARealRecieveLinkData read FOnRecieveLinkData write FOnRecieveLinkData;
  end;

procedure Register;

resourcestring
  dtlServerPage = 'ActiveX';

  dtlOcxPage = 'ActiveX';

implementation

uses System.Win.ComObj;

procedure TXAQuery.InitControlData;
const
  CEventDispIDs: array [0..3] of DWORD = (
    $00000001, $00000002, $00000003, $00000004);
  CControlData: TControlData2 = (
    ClassID:      '{781520A9-4C8C-433B-AA6E-EE9E94108639}';
    EventIID:     '{AAF89E20-1F84-4B1F-B6EE-617B6F2C9CD4}';
    EventCount:   4;
    EventDispIDs: @CEventDispIDs;
    LicenseKey:   nil (*HR:$80004002*);
    Flags:        $00000000;
    Version:      500);
begin
  ControlData := @CControlData;
  TControlData2(CControlData).FirstEventOfs := UIntPtr(@@FOnReceiveData) - UIntPtr(Self);
end;

procedure TXAQuery.CreateControl;

  procedure DoCreate;
  begin
    FIntf := IUnknown(OleObject) as IXAQuery;
  end;

begin
  if FIntf = nil then DoCreate;
end;

function TXAQuery.GetControlInterface: IXAQuery;
begin
  CreateControl;
  Result := FIntf;
end;

function TXAQuery.GetFieldData(const szBlockName: WideString; const szFieldName: WideString; 
                               nRecordIndex: Integer): WideString;
begin
  Result := DefaultInterface.GetFieldData(szBlockName, szFieldName, nRecordIndex);
end;

function TXAQuery.Request(bNext: WordBool): Integer;
begin
  Result := DefaultInterface.Request(bNext);
end;

function TXAQuery.LoadFromResFile(const szFileName: WideString): WordBool;
begin
  Result := DefaultInterface.LoadFromResFile(szFileName);
end;

function TXAQuery.GetTrCode: WideString;
begin
  Result := DefaultInterface.GetTrCode;
end;

function TXAQuery.GetTrDesc: WideString;
begin
  Result := DefaultInterface.GetTrDesc;
end;

procedure TXAQuery.GetBlockInfo(const szFieldName: WideString; var szNameK: WideString; 
                                var szNameE: WideString; var nRecordType: Integer);
begin
  DefaultInterface.GetBlockInfo(szFieldName, szNameK, szNameE, nRecordType);
end;

procedure TXAQuery.SetFieldData(const szBlockName: WideString; const szFieldName: WideString; 
                                nOccursIndex: Integer; const szData: WideString);
begin
  DefaultInterface.SetFieldData(szBlockName, szFieldName, nOccursIndex, szData);
end;

procedure TXAQuery.GetFieldInfo(const szFieldName: WideString; const szItemName: WideString; 
                                var nItemType: Integer; var nDataSize: Integer; 
                                var nDotPoint: Integer; var nOffSet: Integer);
begin
  DefaultInterface.GetFieldInfo(szFieldName, szItemName, nItemType, nDataSize, nDotPoint, nOffSet);
end;

function TXAQuery.GetBlockType(const szBlockName: WideString): Integer;
begin
  Result := DefaultInterface.GetBlockType(szBlockName);
end;

function TXAQuery.GetResData: WideString;
begin
  Result := DefaultInterface.GetResData;
end;

function TXAQuery.GetBlockSize(const szBlockName: WideString): Integer;
begin
  Result := DefaultInterface.GetBlockSize(szBlockName);
end;

function TXAQuery.GetFieldDescList(const szBlockName: WideString): WideString;
begin
  Result := DefaultInterface.GetFieldDescList(szBlockName);
end;

function TXAQuery.GetBlockCount(const szBlockName: WideString): Integer;
begin
  Result := DefaultInterface.GetBlockCount(szBlockName);
end;

procedure TXAQuery.SetBlockCount(const szBlockName: WideString; nCount: Integer);
begin
  DefaultInterface.SetBlockCount(szBlockName, nCount);
end;

procedure TXAQuery.ClearBlockdata(const szFieldName: WideString);
begin
  DefaultInterface.ClearBlockdata(szFieldName);
end;

function TXAQuery.GetLastError: Integer;
begin
  Result := DefaultInterface.GetLastError;
end;

function TXAQuery.GetErrorMessage(nErrorCode: Integer): WideString;
begin
  Result := DefaultInterface.GetErrorMessage(nErrorCode);
end;

function TXAQuery.GetAccountList(nIndex: Integer): WideString;
begin
  Result := DefaultInterface.GetAccountList(nIndex);
end;

function TXAQuery.GetAccountListCount: Integer;
begin
  Result := DefaultInterface.GetAccountListCount;
end;

function TXAQuery.GetBlockData(const szBlockName: WideString): WideString;
begin
  Result := DefaultInterface.GetBlockData(szBlockName);
end;

function TXAQuery.RequestService(const szCode: WideString; const szData: WideString): Integer;
begin
  Result := DefaultInterface.RequestService(szCode, szData);
end;

function TXAQuery.RemoveService(const szCode: WideString; const szData: WideString): Integer;
begin
  Result := DefaultInterface.RemoveService(szCode, szData);
end;

function TXAQuery.RequestLinkToHTS(const szLinkName: WideString; const szData: WideString; 
                                   const szFiller: WideString): WordBool;
begin
  Result := DefaultInterface.RequestLinkToHTS(szLinkName, szData, szFiller);
end;

function TXAQuery.Decompress(const szBlockName: WideString): Integer;
begin
  Result := DefaultInterface.Decompress(szBlockName);
end;

function TXAQuery.GetTRCountPerSec(const szCode: WideString): Integer;
begin
  Result := DefaultInterface.GetTRCountPerSec(szCode);
end;

function TXAQuery.GetAccountName(const szAcc: WideString): WideString;
begin
  Result := DefaultInterface.GetAccountName(szAcc);
end;

function TXAQuery.GetAcctDetailName(const szAcc: WideString): WideString;
begin
  Result := DefaultInterface.GetAcctDetailName(szAcc);
end;

function TXAQuery.GetAcctNickname(const szAcc: WideString): WideString;
begin
  Result := DefaultInterface.GetAcctNickname(szAcc);
end;

function TXAQuery.GetFieldChartRealData(const szBlockName: WideString; const szFieldName: WideString): WideString;
begin
  Result := DefaultInterface.GetFieldChartRealData(szBlockName, szFieldName);
end;

function TXAQuery.GetAttribute(const szBlockName: WideString; const szFieldName: WideString; 
                               const szAttribute: WideString; nRecordIndex: Integer): WideString;
begin
  Result := DefaultInterface.GetAttribute(szBlockName, szFieldName, szAttribute, nRecordIndex);
end;

function TXAQuery.GetTRCountBaseSec(const szCode: WideString): Integer;
begin
  Result := DefaultInterface.GetTRCountBaseSec(szCode);
end;

function TXAQuery.GetTRCountRequest(const szCode: WideString): Integer;
begin
  Result := DefaultInterface.GetTRCountRequest(szCode);
end;

function TXAQuery.GetTRCountLimit(const szCode: WideString): Integer;
begin
  Result := DefaultInterface.GetTRCountLimit(szCode);
end;

function TXAQuery.GetFieldSearchRealData(const szBlockName: WideString; 
                                         const szFieldName: WideString): WideString;
begin
  Result := DefaultInterface.GetFieldSearchRealData(szBlockName, szFieldName);
end;

procedure TXAQuery.SetProgramOrder(bProgramOrder: WordBool);
begin
  DefaultInterface.SetProgramOrder(bProgramOrder);
end;

function TXAQuery.GetProgramOrder: WordBool;
begin
  Result := DefaultInterface.GetProgramOrder;
end;

procedure TXAReal.InitControlData;
const
  CEventDispIDs: array [0..1] of DWORD = (
    $00000001, $00000002);
  CControlData: TControlData2 = (
    ClassID:      '{4D654021-F9D9-49F7-B2F9-6529A19746F7}';
    EventIID:     '{16602768-2C96-4D93-984B-E36E7E35BFBE}';
    EventCount:   2;
    EventDispIDs: @CEventDispIDs;
    LicenseKey:   nil (*HR:$80004002*);
    Flags:        $00000000;
    Version:      500);
begin
  ControlData := @CControlData;
  TControlData2(CControlData).FirstEventOfs := UIntPtr(@@FOnReceiveRealData) - UIntPtr(Self);
end;

procedure TXAReal.CreateControl;

  procedure DoCreate;
  begin
    FIntf := IUnknown(OleObject) as IXAReal;
  end;

begin
  if FIntf = nil then DoCreate;
end;

function TXAReal.GetControlInterface: IXAReal;
begin
  CreateControl;
  Result := FIntf;
end;

function TXAReal.GetTrCode: WideString;
begin
  Result := DefaultInterface.GetTrCode;
end;

function TXAReal.LoadFromResFile(const szFileName: WideString): WordBool;
begin
  Result := DefaultInterface.LoadFromResFile(szFileName);
end;

procedure TXAReal.SetFieldData(const szBlockName: WideString; const szFieldName: WideString; 
                               const szData: WideString);
begin
  DefaultInterface.SetFieldData(szBlockName, szFieldName, szData);
end;

function TXAReal.GetFieldData(const szBlockName: WideString; const szFieldName: WideString): WideString;
begin
  Result := DefaultInterface.GetFieldData(szBlockName, szFieldName);
end;

procedure TXAReal.AdviseRealData;
begin
  DefaultInterface.AdviseRealData;
end;

procedure TXAReal.UnadviseRealData;
begin
  DefaultInterface.UnadviseRealData;
end;

procedure TXAReal.UnadviseRealDataWithKey(const szCode: WideString);
begin
  DefaultInterface.UnadviseRealDataWithKey(szCode);
end;

procedure TXAReal.AdviseLinkFromHTS;
begin
  DefaultInterface.AdviseLinkFromHTS;
end;

procedure TXAReal.UnAdviseLinkFromHTS;
begin
  DefaultInterface.UnAdviseLinkFromHTS;
end;

function TXAReal.GetBlockData(const szBlockName: WideString): WideString;
begin
  Result := DefaultInterface.GetBlockData(szBlockName);
end;

procedure Register;
begin
  RegisterComponents(dtlOcxPage, [TXAQuery, TXAReal]);
end;

end.
