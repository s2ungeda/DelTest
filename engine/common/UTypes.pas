unit UTypes;

interface

type

  TAppStatus = ( asNone, asInit, asLoad );


  TAppStatusEvent  = procedure( asType : TAppStatus ) of object;

  TQuoteType = (qtNone, qtMarketDepth, qtTimeNSale, qtCustom, qtUnknown);

  TPositionType = (ptLong, ptShort);
  TPositionTypes = set of TPositionType;

  TSideType = ( stNone, stLong, stShort );



implementation

end.
