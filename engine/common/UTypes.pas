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



  TLogLevel = ( llFatal, llError, llWarning, llInfo, llDebug, llTrace );
  TLogDataType  = ( ldExRate );

  TWinParam = record
    FontName : string;
    FontSize : integer;
    FTerm    : integer;
  end;

const
	WM_EXRATE_MESSAGE = WM_USER + $0001;
  WM_LOGARRIVED     = WM_USER + $0002;  


implementation


end.
