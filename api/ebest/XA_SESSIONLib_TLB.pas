unit XA_SESSIONLib_TLB;

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
// File generated on 2022-03-03 오후 9:57:16 from Type Library described below.

// ************************************************************************  //
// Type Lib: C:\Dalin\Bin\XA_Session.dll (1)
// LIBID: {F5CC65C5-2965-451B-A6C4-CBC7B8B4AE08}
// LCID: 0
// Helpfile: 
// HelpString: eBest Xing Session Lib
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
  XA_SESSIONLibMajorVersion = 1;
  XA_SESSIONLibMinorVersion = 0;

  LIBID_XA_SESSIONLib: TGUID = '{F5CC65C5-2965-451B-A6C4-CBC7B8B4AE08}';

  DIID__IXASessionEvents: TGUID = '{6D45238D-A5EB-4413-907A-9EA14D046FE5}';
  IID_IXASession: TGUID = '{8C0F4618-3BAB-4F19-A59B-A32E08EA711F}';
  CLASS_XASession: TGUID = '{7FEF321C-6BFD-413C-AA80-541A275434A1}';

// *********************************************************************//
// Declaration of Enumerations defined in Type Library                    
// *********************************************************************//
// Constants for enum XA_MESSAGE_ID
type
  XA_MESSAGE_ID = TOleEnum;
const
  XA_FAILED = $FFFFFFFF;
  XA_SUCCESS = $00000000;

// Constants for enum XA_SERVER_TYPE
type
  XA_SERVER_TYPE = TOleEnum;
const
  XA_NOSELECTED_SERVER = $FFFFFFFF;
  XA_REAL_SERVER = $00000000;
  XA_SIMUL_SERVER = $00000001;

type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  _IXASessionEvents = dispinterface;
  IXASession = interface;
  IXASessionDisp = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  XASession = IXASession;


// *********************************************************************//
// DispIntf:  _IXASessionEvents
// Flags:     (4096) Dispatchable
// GUID:      {6D45238D-A5EB-4413-907A-9EA14D046FE5}
// *********************************************************************//
  _IXASessionEvents = dispinterface
    ['{6D45238D-A5EB-4413-907A-9EA14D046FE5}']
    procedure Login(const szCode: WideString; const szMsg: WideString); dispid 1;
    procedure Logout; dispid 2;
    procedure Disconnect; dispid 3;
  end;

// *********************************************************************//
// Interface: IXASession
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {8C0F4618-3BAB-4F19-A59B-A32E08EA711F}
// *********************************************************************//
  IXASession = interface(IDispatch)
    ['{8C0F4618-3BAB-4F19-A59B-A32E08EA711F}']
    function IsLoadAPI: WordBool; safecall;
    function ConnectServer(const szServerIP: WideString; nServerPort: Integer): WordBool; safecall;
    procedure DisconnectServer; safecall;
    function IsConnected: WordBool; safecall;
    function Login(const szID: WideString; const szPwd: WideString; const szCertPwd: WideString; 
                   nServerType: Integer; bShowCertErrDlg: WordBool): WordBool; safecall;
    function Logout: WordBool; safecall;
    function Get_ConnectTimeOut: Integer; safecall;
    procedure Set_ConnectTimeOut(pVal: Integer); safecall;
    function Get_SendPacketSize: Integer; safecall;
    procedure Set_SendPacketSize(pVal: Integer); safecall;
    function GetLastError: Integer; safecall;
    function GetErrorMessage(nErrorCode: Integer): WideString; safecall;
    function GetCommMedia: WideString; safecall;
    function GetETKMedia: WideString; safecall;
    function GetClientIP: WideString; safecall;
    function GetServerName: WideString; safecall;
    function GetAccountList(nIndex: Integer): WideString; safecall;
    function GetAccountListCount: Integer; safecall;
    function GetAccountName(const szAcc: WideString): WideString; safecall;
    function GetAcctDetailName(const szAcc: WideString): WideString; safecall;
    function GetAcctNickname(const szAcc: WideString): WideString; safecall;
    function GetPath: WideString; safecall;
    procedure SetPath(const szPath: WideString); safecall;
    function SetMode(const szMode: WideString; const szValue: WideString): WordBool; safecall;
    property ConnectTimeOut: Integer read Get_ConnectTimeOut write Set_ConnectTimeOut;
    property SendPacketSize: Integer read Get_SendPacketSize write Set_SendPacketSize;
  end;

// *********************************************************************//
// DispIntf:  IXASessionDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {8C0F4618-3BAB-4F19-A59B-A32E08EA711F}
// *********************************************************************//
  IXASessionDisp = dispinterface
    ['{8C0F4618-3BAB-4F19-A59B-A32E08EA711F}']
    function IsLoadAPI: WordBool; dispid 1;
    function ConnectServer(const szServerIP: WideString; nServerPort: Integer): WordBool; dispid 2;
    procedure DisconnectServer; dispid 3;
    function IsConnected: WordBool; dispid 4;
    function Login(const szID: WideString; const szPwd: WideString; const szCertPwd: WideString; 
                   nServerType: Integer; bShowCertErrDlg: WordBool): WordBool; dispid 5;
    function Logout: WordBool; dispid 7;
    property ConnectTimeOut: Integer dispid 8;
    property SendPacketSize: Integer dispid 9;
    function GetLastError: Integer; dispid 10;
    function GetErrorMessage(nErrorCode: Integer): WideString; dispid 11;
    function GetCommMedia: WideString; dispid 12;
    function GetETKMedia: WideString; dispid 13;
    function GetClientIP: WideString; dispid 14;
    function GetServerName: WideString; dispid 15;
    function GetAccountList(nIndex: Integer): WideString; dispid 16;
    function GetAccountListCount: Integer; dispid 17;
    function GetAccountName(const szAcc: WideString): WideString; dispid 18;
    function GetAcctDetailName(const szAcc: WideString): WideString; dispid 19;
    function GetAcctNickname(const szAcc: WideString): WideString; dispid 20;
    function GetPath: WideString; dispid 21;
    procedure SetPath(const szPath: WideString); dispid 22;
    function SetMode(const szMode: WideString; const szValue: WideString): WordBool; dispid 23;
  end;


// *********************************************************************//
// OLE Control Proxy class declaration
// Control Name     : TXASession
// Help String      : XASession Class
// Default Interface: IXASession
// Def. Intf. DISP? : No
// Event   Interface: _IXASessionEvents
// TypeFlags        : (2) CanCreate
// *********************************************************************//
  TXASessionLogin = procedure(ASender: TObject; const szCode: WideString; const szMsg: WideString) of object;

  TXASession = class(TOleControl)
  private
    FOnLogin: TXASessionLogin;
    FOnLogout: TNotifyEvent;
    FOnDisconnect: TNotifyEvent;
    FIntf: IXASession;
    function  GetControlInterface: IXASession;
  protected
    procedure CreateControl;
    procedure InitControlData; override;
  public
    function IsLoadAPI: WordBool;
    function ConnectServer(const szServerIP: WideString; nServerPort: Integer): WordBool;
    procedure DisconnectServer;
    function IsConnected: WordBool;
    function Login(const szID: WideString; const szPwd: WideString; const szCertPwd: WideString; 
                   nServerType: Integer; bShowCertErrDlg: WordBool): WordBool;
    function Logout: WordBool;
    function GetLastError: Integer;
    function GetErrorMessage(nErrorCode: Integer): WideString;
    function GetCommMedia: WideString;
    function GetETKMedia: WideString;
    function GetClientIP: WideString;
    function GetServerName: WideString;
    function GetAccountList(nIndex: Integer): WideString;
    function GetAccountListCount: Integer;
    function GetAccountName(const szAcc: WideString): WideString;
    function GetAcctDetailName(const szAcc: WideString): WideString;
    function GetAcctNickname(const szAcc: WideString): WideString;
    function GetPath: WideString;
    procedure SetPath(const szPath: WideString);
    function SetMode(const szMode: WideString; const szValue: WideString): WordBool;
    property  ControlInterface: IXASession read GetControlInterface;
    property  DefaultInterface: IXASession read GetControlInterface;
  published
    property Anchors;
    property ConnectTimeOut: Integer index 8 read GetIntegerProp write SetIntegerProp stored False;
    property SendPacketSize: Integer index 9 read GetIntegerProp write SetIntegerProp stored False;
    property OnLogin: TXASessionLogin read FOnLogin write FOnLogin;
    property OnLogout: TNotifyEvent read FOnLogout write FOnLogout;
    property OnDisconnect: TNotifyEvent read FOnDisconnect write FOnDisconnect;
  end;

procedure Register;

resourcestring
  dtlServerPage = 'ActiveX';

  dtlOcxPage = 'ActiveX';

implementation

uses System.Win.ComObj;

procedure TXASession.InitControlData;
const
  CEventDispIDs: array [0..2] of DWORD = (
    $00000001, $00000002, $00000003);
  CControlData: TControlData2 = (
    ClassID:      '{7FEF321C-6BFD-413C-AA80-541A275434A1}';
    EventIID:     '{6D45238D-A5EB-4413-907A-9EA14D046FE5}';
    EventCount:   3;
    EventDispIDs: @CEventDispIDs;
    LicenseKey:   nil (*HR:$80004002*);
    Flags:        $00000000;
    Version:      500);
begin
  ControlData := @CControlData;
  TControlData2(CControlData).FirstEventOfs := UIntPtr(@@FOnLogin) - UIntPtr(Self);
end;

procedure TXASession.CreateControl;

  procedure DoCreate;
  begin
    FIntf := IUnknown(OleObject) as IXASession;
  end;

begin
  if FIntf = nil then DoCreate;
end;

function TXASession.GetControlInterface: IXASession;
begin
  CreateControl;
  Result := FIntf;
end;

function TXASession.IsLoadAPI: WordBool;
begin
  Result := DefaultInterface.IsLoadAPI;
end;

function TXASession.ConnectServer(const szServerIP: WideString; nServerPort: Integer): WordBool;
begin
  Result := DefaultInterface.ConnectServer(szServerIP, nServerPort);
end;

procedure TXASession.DisconnectServer;
begin
  DefaultInterface.DisconnectServer;
end;

function TXASession.IsConnected: WordBool;
begin
  Result := DefaultInterface.IsConnected;
end;

function TXASession.Login(const szID: WideString; const szPwd: WideString; 
                          const szCertPwd: WideString; nServerType: Integer; 
                          bShowCertErrDlg: WordBool): WordBool;
begin
  Result := DefaultInterface.Login(szID, szPwd, szCertPwd, nServerType, bShowCertErrDlg);
end;

function TXASession.Logout: WordBool;
begin
  Result := DefaultInterface.Logout;
end;

function TXASession.GetLastError: Integer;
begin
  Result := DefaultInterface.GetLastError;
end;

function TXASession.GetErrorMessage(nErrorCode: Integer): WideString;
begin
  Result := DefaultInterface.GetErrorMessage(nErrorCode);
end;

function TXASession.GetCommMedia: WideString;
begin
  Result := DefaultInterface.GetCommMedia;
end;

function TXASession.GetETKMedia: WideString;
begin
  Result := DefaultInterface.GetETKMedia;
end;

function TXASession.GetClientIP: WideString;
begin
  Result := DefaultInterface.GetClientIP;
end;

function TXASession.GetServerName: WideString;
begin
  Result := DefaultInterface.GetServerName;
end;

function TXASession.GetAccountList(nIndex: Integer): WideString;
begin
  Result := DefaultInterface.GetAccountList(nIndex);
end;

function TXASession.GetAccountListCount: Integer;
begin
  Result := DefaultInterface.GetAccountListCount;
end;

function TXASession.GetAccountName(const szAcc: WideString): WideString;
begin
  Result := DefaultInterface.GetAccountName(szAcc);
end;

function TXASession.GetAcctDetailName(const szAcc: WideString): WideString;
begin
  Result := DefaultInterface.GetAcctDetailName(szAcc);
end;

function TXASession.GetAcctNickname(const szAcc: WideString): WideString;
begin
  Result := DefaultInterface.GetAcctNickname(szAcc);
end;

function TXASession.GetPath: WideString;
begin
  Result := DefaultInterface.GetPath;
end;

procedure TXASession.SetPath(const szPath: WideString);
begin
  DefaultInterface.SetPath(szPath);
end;

function TXASession.SetMode(const szMode: WideString; const szValue: WideString): WordBool;
begin
  Result := DefaultInterface.SetMode(szMode, szValue);
end;

procedure Register;
begin
  RegisterComponents(dtlOcxPage, [TXASession]);
end;

end.
