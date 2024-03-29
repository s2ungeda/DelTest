unit UTableConsts;

interface

const
  prcTbl1_TitleCnt = 13 ;
  prcTbll1_Title : array [0..prcTbl1_TitleCnt-1]of string = ('코인', '거래소'
    ,'KIP', 'WCD', 'KRW잔량(천원)', '매도가','매수가'
    ,'KRW잔량(천원)', '현재가', '고/등락/저','입금','출금'
    ,'일거래액(억)');

  prcTbll1_Width : array [0..prcTbl1_TitleCnt-1] of integer = ( 50, 40,
    80, 80, 100, 100, 100,  //  마진 매수
    100, 100, 80, 35, 35,
    80 );


  prcTbl2_TitleCnt = 14 ;
  prcTbll2_Title : array [0..prcTbl2_TitleCnt-1]of string = ('코인', ''
    ,'BN', '시각', 'UP', '시각','BT','시각'
    ,'거래소','KIP', 'WCD', '현재가','등락'
    ,'일거래액(억)');

  prcTbll2_Width : array [0..prcTbl2_TitleCnt-1] of integer = ( 70, 40,
    30,  65,  30, 65, 30, 65,
    40,  60,  65, 90, 60,
    70 );


  quoteMon_TitleCnt = 15;
  quoteMon_Title : array [0..quoteMon_TitleCnt-1] of string = ('코인', '거래소'
    ,'KIP', 'WCD','SP','현재가'
    ,'매도잔량','매도가','매수가','매수잔량'
    ,'등락' ,'고','저', '일거래액'
    ,'선물');
  quoteMon_Width : array [0..quoteMon_TitleCnt-1] of integer = (70, 40
    , 60, 60, 60, 80
    , 70, 70, 70, 70
    , 60, 60, 60, 60
    , 30 );

  Mon_CurCol = 5;
  Mon_AskCol = 7;
  Mon_BidCol = 8;


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
  orderList_Title : array [0..orderList_TitleCnt-1] of string = ('거래소'
  	,'전략',	'종목코드',	'방향','주문가겨','수량'
    ,'평균가','체결량',		'상태','접수시각','주문번호');
  orderList_Width : array [0..orderList_TitleCnt-1] of integer = (50
  	,50, 80, 30, 80, 60
    ,80, 60, 50, 60, 100);
  

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
