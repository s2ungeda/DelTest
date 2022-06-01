unit UApiConsts;

interface

uses
  UApiTypes
  ;

const

  // spot 이랑 future 랑 종목코드가 같아서  구분을 위한 접미사
  Fut_Suf = '_PF';

{
  Overseas = 0; // 해외거래소
  Domestic = 1; // 국내거래소

  Bin = 0;
  Upb = 1;
  Bit = 2;

  ExCnt = 3;

 }
  QOUTE_SOCK = 0;
  TRADE_SOCK = 1;

  QOUTE_REST = 0;
  TRADE_REST = 1;

  PUB_REQ = 0;
  PRI_REQ = 1;
  ORD_REQ = 2; 
  


  TMajorSymbolCode  : array [TMajorSymbolKind] of string = ('BTC', 'ETH', 'XRP');

  TExchangeKindDesc : array [ TExchangeKind ] of string = ('Binace', 'Upbit', 'Bithumb');
  TExchangeKindShortDesc : array [ TExchangeKind ] of string = ('BN', 'UP', 'BT');
  // short version 위에꺼 사용하기 힘듬...
  TExShortDesc : array [ TExchangeKind ] of string = ('BN', 'UP', 'BT');
  TMarketTypeDesc : array [ TMarketType ] of string = ('Spot', 'Future');
  TAccountMarketTypeDesc : array [ TAccountMarketType ] of string = ('All', 'Spot', 'Margin', 'Future');

implementation

end.
