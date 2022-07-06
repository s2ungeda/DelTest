unit USharedConsts;

interface

const
  DATA_SIZE = 1024 * 100;
  Q_SIZE = 5;

  // REST ���忡��..
  REST_PUSH = 1;
  REST_POP  = 0;
  // Dalin ���忡��..
  SHRD_PUSH = 0;
  SHRE_POP  = 1;
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

  TR_NEW_ORD = 'N';     // �ű� �ֹ�
  TR_CNL_ORD = 'C';     // ��� �ֹ�
  TR_REQ_ORD = 'O';     // �ֹ� ��ȸ..
  TR_REQ_POS = 'P';     // ������ ��ȸ..
  TR_REQ_BAL = 'B';     // �ܰ� ��ȸ...

  EX_BN = 'B';
  EX_UP = 'U';
  EX_BI = 'T';



implementation

end.
