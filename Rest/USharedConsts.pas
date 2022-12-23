unit USharedConsts;

interface

const
  DATA_SIZE = 1024 * 200;
  Q_SIZE = 200;

  // REST ���忡��..
  REST_PUSH = 1;
  REST_POP  = 0;
  // Dalin ���忡��..
  SHRD_PUSH = 0;
  SHRE_POP  = 1;
  // ���̳� �ֹ� �迭 �ε���
  BO_CNT = 7;
  BO_CODE  = 0;
  BO_LS    = 1;
  BO_PRC   = 2;
  BO_QTY   = 3;
  BO_TYPE  = 4;
  BO_CID   = 5;
  BO_RDO   = 6;    // reduce only

  // ���̳� ��� �迭 �ε���
  BC_CNT = 2;
  BC_OID  = 0;
  BC_CODE = 1;

  // ����Ʈ
  UO_CNT = 5;
  UO_CODE  = 0;
  UO_LS    = 1;
  UO_PRC   = 2;
  UO_QTY   = 3;
  UO_TYPE  = 4;

  // ���
  UC_CNT = 1;
  UC_UID   = 0;

  // �ֹ�����
  UA_CNT = 1;
  UA_CODE = 0;
  // �ֹ� ����Ʈ
  UL_CNT = 2;
  UL_STATE = 0;
  UL_ASC = 1;
  // order detail
  UD_CNT = 1;
  UD_UID	= 0;


  // ����
  // new order
  TO_CNT= 5;
  TO_CODE  = 0;
  TO_LS    = 1;
  TO_PRC   = 2;
  TO_QTY   = 3;
  TO_STT   = 4;

  // cancel order
  TC_CNT = 4;
  TC_OID   = 0;
  TC_CODE  = 1;
  TC_LS    = 2;
  TC_STT   = 3;

  // order list  & trade Amt
  TL_CNT = 1;
  TL_CODE = 0;

  // balance
  TB_CNT = 1;
  TB_CODE = 0; 

  // order detail
  TD_CNT = 2;
  TD_CODE = 0;
  TD_OID	= 1;

  TR_NEW_ORD = 'N';     // �ű� �ֹ�
  TR_CNL_ORD = 'C';     // ��� �ֹ�
  TR_REQ_ORD = 'O';     // �ֹ� ��ȸ..
  TR_REQ_POS = 'P';     // ������ ��ȸ..
  TR_REQ_BAL = 'B';     // �ܰ� ��ȸ...
  TR_ABLE_ORD= 'A';			// �ֹ����ɱݾ�..
  TR_ORD_DETAIL = 'D';	// �ֹ�����ȸ.
  TR_TRD_AMT  = 'T';    // �ŷ���
  TR_ORD_BOOK = 'H';    // ���ϵ���..

  EX_BN = 'B';
  EX_UP = 'U';
  EX_BI = 'T';



implementation

end.
