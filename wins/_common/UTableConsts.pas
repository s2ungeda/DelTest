unit UTableConsts;

interface

const
  prcTbl1_TitleCnt = 13 ;
  prcTbll1_Title : array [0..prcTbl1_TitleCnt-1]of string = ('����', '�ŷ���'
    ,'�ŵ�KIP', '�ż�KIP', '�ܷ�(õ)', '����/�ŵ���','����/�ż���'
    ,'�ܷ�(õ)', '���簡', '��/���/��','�Ա�','���'
    ,'�ϰŷ���(��)');

  prcTbl2_TitleCnt = 13 ;
  prcTbll2_Title : array [0..prcTbl2_TitleCnt-1]of string = ('����', '�����'
    ,'����', '�ŵ���', '�ż���', '���簡','���(%)'
    ,'��', '��', '�ϰŷ���(��)','�ð��Ѿ�','����'
    ,'����');


  prcTbll1_Width : array [0..prcTbl1_TitleCnt-1] of integer = ( 40, 40,
    80, 80, 80, 100, 100,  //  ���� �ż�
    80, 100, 80, 35, 35,
    100 );


  CoinCol = 0;
  ExCol   = 1;
  AskKipCol = 2;
  BidKipCol = 3;

  CurCol = 8;

function GetMajorRow( i : integer ) : integer;

implementation

function GetMajorRow( i : integer ) : integer;
begin
  Result := i * 3  + 1;
end;

end.
