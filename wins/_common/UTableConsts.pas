unit UTableConsts;

interface

const
  prcTbl1_TitleCnt = 13 ;
  prcTbll1_Title : array [0..prcTbl1_TitleCnt-1]of string = ('����', '�ŷ���'
    ,'KIP', 'WCD', 'KRW�ܷ�(õ��)', '�ŵ���','�ż���'
    ,'KRW�ܷ�(õ��)', '���簡', '��/���/��','�Ա�','���'
    ,'�ϰŷ���(��)');

  prcTbll1_Width : array [0..prcTbl1_TitleCnt-1] of integer = ( 50, 40,
    80, 80, 100, 100, 100,  //  ���� �ż�
    100, 100, 80, 35, 35,
    80 );


  prcTbl2_TitleCnt = 14 ;
  prcTbll2_Title : array [0..prcTbl2_TitleCnt-1]of string = ('����', ''
    ,'BN', '�ð�', 'UP', '�ð�','BT','�ð�'
    ,'�ŷ���','KIP', 'WCD', '���簡','���'
    ,'�ϰŷ���(��)');

  prcTbll2_Width : array [0..prcTbl2_TitleCnt-1] of integer = ( 70, 40,
    30,  65,  30, 65, 30, 65,
    40,  60,  65, 90, 60,
    70 );


  quoteMon_TitleCnt = 10;
  quoteMon_Title : array [0..quoteMon_TitleCnt-1] of string = ('����', '�ŷ���'
    ,'KIP', 'WCD','SP','���簡','���'
    ,'��','��', '�ϰŷ���');
  quoteMon_Width : array [0..quoteMon_TitleCnt-1] of integer = (70, 40
    , 60, 60, 60, 80, 60
    , 60, 60, 60 );

  CoinCol = 0;
  ExCol   = 1;
  AskKipCol = 2;
  BidKipCol = 3;

  CurCol = 8;
  DayAmtCol = 12;

  // only dnw state win
  BN_CoinCol = 2;
  UP_CoinCol = 4;
  BT_CoinCol = 6;


  RPSNT_COL_WID = 47;

  orderList_TitleCnt = 11 ;
  orderList_Title : array [0..orderList_TitleCnt-1] of string = ('�ŷ���'
  	,'����',	'�����ڵ�',	'����','�ֹ�����','����'
    ,'��հ�','ü�ᷮ',		'����','�����ð�','�ֹ���ȣ');
  orderList_Width : array [0..orderList_TitleCnt-1] of integer = (30
  	,50, 60, 30, 80, 60
    ,80, 60, 50, 60, 60);                                 
  

function GetMajorRow( i : integer ) : integer;
function FindBinRow( iRow : integer ) : integer;

implementation

function GetMajorRow( i : integer ) : integer;
begin
  Result := i * 3  + 1;
end;

function FindBinRow( iRow : integer ) : integer;
var
  iMod : integer;
begin
  // 1-> 1,  2->1, 3-> 1
  // 4-> 4   5->4  6->4
  iMod := iRow mod 3;

  if iMod = 1 then
    Result := iRow
  else if iMod = 2 then
    Result := iRow-1
  else
    Result := iRow -2;
end;

end.
