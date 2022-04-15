unit UTableConsts;

interface

const
  prcTbl1_TitleCnt = 13 ;
  prcTbll1_Title : array [0..prcTbl1_TitleCnt-1]of string = ('코인', '거래소'
    ,'KIP', 'WDC', '잔량(천)', '선물/매도가','마진/매수가'
    ,'잔량(천)', '현재가', '고/등락/저','입금','출금'
    ,'일거래액');

  prcTbll1_Width : array [0..prcTbl1_TitleCnt-1] of integer = ( 50, 40,
    80, 80, 100, 100, 100,  //  마진 매수
    100, 100, 80, 35, 35,
    80 );


  prcTbl2_TitleCnt = 14 ;
  prcTbll2_Title : array [0..prcTbl2_TitleCnt-1]of string = ('코인', ''
    ,'BN', '시각', 'UP', '시각','BT','시각'
    ,'거래소','KIP', 'WCD', '현재가','등락'
    ,'일거래액');

  prcTbll2_Width : array [0..prcTbl2_TitleCnt-1] of integer = ( 50, 40,
    30,  65,  30, 65, 30, 65,
    40,  80,  80, 100, 60,
    80 );


  quoteMon_TitleCnt = 10;
  quoteMon_Title : array [0..quoteMon_TitleCnt-1] of string = ('코인', '거래소'
    ,'KIP', 'WCD','현재가','등락'
    ,'고','저', '일거래액', '마진');
  quoteMon_Width : array [0..quoteMon_TitleCnt-1] of integer = (70, 40
    , 80, 80, 120, 80
    , 80, 80, 80, 35  );

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
