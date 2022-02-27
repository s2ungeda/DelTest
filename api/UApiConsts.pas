unit UApiConsts;

interface

uses
  UApiTypes
  ;

const

  // spot �̶� future �� �����ڵ尡 ���Ƽ�  ������ ���� ���̻�
  Fut_Suf = '_PF';

{
  Overseas = 0; // �ؿܰŷ���
  Domestic = 1; // �����ŷ���

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
