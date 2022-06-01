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

  QOUTE_REST = 0;
  TRADE_REST = 1;

  PUB_REQ = 0;
  PRI_REQ = 1;
  ORD_REQ = 2; 
  


  TMajorSymbolCode  : array [TMajorSymbolKind] of string = ('BTC', 'ETH', 'XRP');

  TExchangeKindDesc : array [ TExchangeKind ] of string = ('Binace', 'Upbit', 'Bithumb');
  TExchangeKindShortDesc : array [ TExchangeKind ] of string = ('BN', 'UP', 'BT');
  // short version ������ ����ϱ� ����...
  TExShortDesc : array [ TExchangeKind ] of string = ('BN', 'UP', 'BT');
  TMarketTypeDesc : array [ TMarketType ] of string = ('Spot', 'Future');
  TAccountMarketTypeDesc : array [ TAccountMarketType ] of string = ('All', 'Spot', 'Margin', 'Future');

implementation

end.
