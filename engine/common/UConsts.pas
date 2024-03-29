unit UConsts;

interface

uses
  Utypes
  ;

const

  PRICE_EPSILON = 0.001;
  EPSILON    = 0.00000001;
  DOUBLE_EPSILON : Double = 0.00000001;

  GRID_MOD_COLOR = $F8F8F8;
  SELECTED_COLOR  = $00F2BEB9;

  SELECTED_FONT_COLOR = $000000;

  GRID_SELECT_COLOR = $F0F0F0;
  GRID_REVER_COLOR  = $00EEEEEE;
  FUND_FORM_COLOR   = $00D8E5EE;

  DISABLED_COLOR  = $BBBBBB;
  ERROR_COLOR     = $008080FF;
  ODD_COLOR       = $FFFFFF;
  EVEN_COLOR      = $EEEEEE;

  LONG_COLOR = $E4E2FC;
  SHORT_COLOR = $F5E2DA;


  LONG_FLAG = 100;
  SHORT_FLAG = 200;
  NONE_FLAG = 10;


    // data ID
  DNW_EVENT = 100;
  TRD_DATA  = 200;

    // event ID
  DNW_BOTH_TRUE = 101;
  DNW_BOTH_FALE = 102;
  DWN_DEPOSIT_TRUE  = 103;
  DWN_DEPOSIT_FALSE = 104;
  DWN_WITHDRAW_TRUE = 105;
  DWN_WITHDRAW_FALSE= 106;

    // order events
  ORDER_NEW           = 201;
  ORDER_ACCEPTED      = 202;
  ORDER_REJECTED      = 203;
  ORDER_CANCELED      = 204;
  ORDER_FILLED        = 205;
    // fill events
  FILL_NEW            = 206;

    // position Event;
  POSITION_NEW        = 210;
  POSITION_UPDATE     = 211;
  POSITION_ABLEQTY    = 212;
  POSITION_TRAD_AMT   = 213;

    // wallet event
  ACCOUNT_INFO = 220;


  DIV_ORD_NO = 1;
  DIV_LOC_NO = 0;

  // asNone, asInit, asSetValue, asRecovery, asLoad
  TAppStatusDesc : array [ TAppStatus ] of string = ('None', 'Init', 'SetValue', 'Recovery', 'Load', 'Show');

implementation

end.
