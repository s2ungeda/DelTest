unit UTypes;
interface

uses
  messages
  ;

type
  TAppStatus = ( asNone, asInit, asSetValue, asRecovery, asLoad, asShow );

  TAppStatusEvent  = procedure( asType : TAppStatus ) of object;
  TQuoteType = (qtNone, qtMarketDepth, qtTimeNSale, qtCustom, qtUnknown);
  TPositionType = (ptLong, ptShort);
  TPositionTypes = set of TPositionType;
  TSideType = ( stNone, stLong, stShort );


  TWinParam = record
    FontName : string;
    FontSize : integer;
  end;

const
  WM_LOGARRIVED     = WM_USER + $0002;


implementation


end.
