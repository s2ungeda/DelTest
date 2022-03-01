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

  // asNone, asInit, asSetValue, asRecovery, asLoad
  TAppStatusDesc : array [ TAppStatus ] of string = ('None', 'Init', 'SetValue', 'Recovery', 'Load', 'Show');

implementation

end.
