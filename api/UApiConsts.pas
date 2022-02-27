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


  TMajorSymbol  : array [0..2] of string = ('BTC', 'ETH', 'XRP');


  TExchangeKindDesc : array [ TExchangeKind ] of string = ('Binace', 'Upbit', 'Bithumb');
  TExchangeKindShortDesc : array [ TExchangeKind ] of string = ('BN', 'UP', 'BT');
  TMarketTypeDesc : array [ TMarketType ] of string = ('Spot', 'Future');
  TAccountMarketTypeDesc : array [ TAccountMarketType ] of string = ('Spot', 'Margin', 'Future');

implementation

end.
