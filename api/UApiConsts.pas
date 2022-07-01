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
  


  TMajorSymbolCode        : array [TMajorSymbolKind] of string = ('BTC', 'ETH', 'XRP');

  TExchangeKindDesc       : array [ TExchangeKind ] of string = ('Binace', 'Upbit', 'Bithumb');
  TExchangeKindShortDesc  : array [ TExchangeKind ] of string = ('BN', 'UP', 'BT');
  // short version ������ ����ϱ� ����...
  TExShortDesc            : array [ TExchangeKind ] of string = ('BN', 'UP', 'BT');
  TMarketTypeDesc         : array [ TMarketType ] of string = ('Spot', 'Future');
  TAccountMarketTypeDesc  : array [ TAccountMarketType ] of string = ('All', 'Spot', 'Margin', 'Future');
  TSettleCurTypeDesc      : array [ TSettleCurType ] of string = ('NONE', 'KRW', 'USDT', 'BTC' );

  // ���̳� �ֹ� �迭 �ε���
  BO_CNT = 8;
  BO_CODE  = 0;
  BO_LS    = 1;
  BO_PRC   = 2;
  BO_QTY   = 3;
  BO_TYPE  = 4;
  BO_CID   = 5;
  BO_TIF   = 6;
  BO_RDO   = 7;    // reduce only

  // ���̳� ��� �迭 �ε���
  BC_CNT = 2;
  BC_OID  = 0;
  BC_CODE = 1;

  // ����Ʈ
  UO_CNT = 6;
  UO_CODE  = 0;
  UO_LS    = 1;
  UO_PRC   = 2;
  UO_QTY   = 3;
  UO_TYPE  = 4;
  UO_UID   = 5;

  UC_CNT = 1;
  UC_UID   = 0;

  // ����
  TO_CNT = 5;
  TO_CODE  = 0;
  TO_LS    = 1;
  TO_PRC   = 2;
  TO_QTY   = 3;
  TO_STT   = 4;

  TC_CNT = 4;
  TC_OID   = 0;
  TC_CODE  = 1;
  TC_LS    = 2;
  TC_STT   = 3;

implementation

end.
