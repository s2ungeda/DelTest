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
  DayAmtCol = 12;

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
