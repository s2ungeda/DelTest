unit UTableConsts;

interface

const
  prcTbl1_TitleCnt = 13 ;
  prcTbll1_Title : array [0..prcTbl1_TitleCnt-1]of string = ('코인', '거래소'
    ,'매도KIP', '매수KIP', '잔량(천)', '선물/매도가','마진/매수가'
    ,'잔량(천)', '현재가', '고/등락/저','입금','출금'
    ,'일거래액(억)');

  prcTbl2_TitleCnt = 13 ;
  prcTbll2_Title : array [0..prcTbl2_TitleCnt-1]of string = ('코인', '시장명'
    ,'김프', '매도가', '매수가', '현재가','등락(%)'
    ,'고', '저', '일거래액(억)','시가총액','선물'
    ,'마진');


  prcTbll1_Width : array [0..prcTbl1_TitleCnt-1] of integer = ( 40, 40,
    80, 80, 80, 100, 100,  //  마진 매수
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
