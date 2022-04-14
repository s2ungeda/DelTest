unit USymbolUtils;

interface

uses
  USymbols
  ;

  // asending
function CompareDailyAmount(Data1, Data2: Pointer): Integer;
function CompareDayUpDown(Data1, Data2: Pointer): Integer;
function CompareLast(Data1, Data2: Pointer): Integer;
function CompareBidPrice(Data1, Data2: Pointer): Integer;
function CompareAskPrice(Data1, Data2: Pointer): Integer;
function CompareKimpPrice(Data1, Data2: Pointer): Integer;
function CompareWDCPrice(Data1, Data2: Pointer): Integer;

// desending

function CompareDailyAmount2(Data1, Data2: Pointer): Integer;
function CompareDayUpDown2(Data1, Data2: Pointer): Integer;
function CompareLast2(Data1, Data2: Pointer): Integer;
function CompareBidPrice2(Data1, Data2: Pointer): Integer;
function CompareAskPrice2(Data1, Data2: Pointer): Integer;
function CompareKimpPrice2(Data1, Data2: Pointer): Integer;
function CompareWDCPrice2(Data1, Data2: Pointer): Integer;

implementation


{ Symbol Sort }

function CompareDailyAmount(Data1, Data2: Pointer): Integer;
var
  Symbol1: TSymbol absolute Data1;
  Symbol2: TSymbol absolute Data2;
begin
  if Symbol1.DayAmount < Symbol2.DayAmount then
    Result := 1
  else if Symbol1.DayAmount > Symbol2.DayAmount  then
    Result := -1
  else
    Result := 0;
end;

function CompareDayUpDown(Data1, Data2: Pointer): Integer;
var
  Symbol1: TSymbol absolute Data1;
  Symbol2: TSymbol absolute Data2;
begin
  if Symbol1.DayUpDown < Symbol2.DayUpDown then
    Result := 1
  else if Symbol1.DayUpDown > Symbol2.DayUpDown  then
    Result := -1
  else
    Result := 0;
end;

function CompareLast(Data1, Data2: Pointer): Integer;
var
  Symbol1: TSymbol absolute Data1;
  Symbol2: TSymbol absolute Data2;
begin
  if Symbol1.Last < Symbol2.Last then
    Result := 1
  else if Symbol1.Last > Symbol2.Last  then
    Result := -1
  else
    Result := 0;
end;

function CompareBidPrice(Data1, Data2: Pointer): Integer;
var
  Symbol1: TSymbol absolute Data1;
  Symbol2: TSymbol absolute Data2;
begin
  if Symbol1.Bids[0].Price < Symbol2.Bids[0].Price then
    Result := 1
  else if Symbol1.Bids[0].Price > Symbol2.Bids[0].Price  then
    Result := -1
  else
    Result := 0;
end;


function CompareAskPrice(Data1, Data2: Pointer): Integer;
var
  Symbol1: TSymbol absolute Data1;
  Symbol2: TSymbol absolute Data2;
begin
  if Symbol1.Asks[0].Price < Symbol2.Asks[0].Price then
    Result := 1
  else if Symbol1.Asks[0].Price > Symbol2.Asks[0].Price  then
    Result := -1
  else
    Result := 0;
end;


function CompareKimpPrice(Data1, Data2: Pointer): Integer;
var
  Symbol1: TSymbol absolute Data1;
  Symbol2: TSymbol absolute Data2;
begin
  if Symbol1.KimpPrice < Symbol2.KimpPrice then
    Result := 1
  else if Symbol1.KimpPrice > Symbol2.KimpPrice  then
    Result := -1
  else
    Result := 0;
end;

function CompareWDCPrice(Data1, Data2: Pointer): Integer;
var
  Symbol1: TSymbol absolute Data1;
  Symbol2: TSymbol absolute Data2;
begin
  if Symbol1.WDCPrice < Symbol2.WDCPrice then
    Result := 1
  else if Symbol1.WDCPrice > Symbol2.WDCPrice  then
    Result := -1
  else
    Result := 0;
end;

//------------------------------------------------------------------------------

function CompareDailyAmount2(Data1, Data2: Pointer): Integer;
var
  Symbol1: TSymbol absolute Data1;
  Symbol2: TSymbol absolute Data2;
begin
  if Symbol1.DayAmount > Symbol2.DayAmount then
    Result := 1
  else if Symbol1.DayAmount < Symbol2.DayAmount  then
    Result := -1
  else
    Result := 0;
end;

function CompareDayUpDown2(Data1, Data2: Pointer): Integer;
var
  Symbol1: TSymbol absolute Data1;
  Symbol2: TSymbol absolute Data2;
begin
  if Symbol1.DayUpDown > Symbol2.DayUpDown then
    Result := 1
  else if Symbol1.DayUpDown < Symbol2.DayUpDown  then
    Result := -1
  else
    Result := 0;
end;

function CompareLast2(Data1, Data2: Pointer): Integer;
var
  Symbol1: TSymbol absolute Data1;
  Symbol2: TSymbol absolute Data2;
begin
  if Symbol1.Last > Symbol2.Last then
    Result := 1
  else if Symbol1.Last < Symbol2.Last  then
    Result := -1
  else
    Result := 0;
end;

function CompareBidPrice2(Data1, Data2: Pointer): Integer;
var
  Symbol1: TSymbol absolute Data1;
  Symbol2: TSymbol absolute Data2;
begin
  if Symbol1.Bids[0].Price > Symbol2.Bids[0].Price then
    Result := 1
  else if Symbol1.Bids[0].Price < Symbol2.Bids[0].Price  then
    Result := -1
  else
    Result := 0;
end;


function CompareAskPrice2(Data1, Data2: Pointer): Integer;
var
  Symbol1: TSymbol absolute Data1;
  Symbol2: TSymbol absolute Data2;
begin
  if Symbol1.Asks[0].Price > Symbol2.Asks[0].Price then
    Result := 1
  else if Symbol1.Asks[0].Price < Symbol2.Asks[0].Price  then
    Result := -1
  else
    Result := 0;
end;

function CompareWDCPrice2(Data1, Data2: Pointer): Integer;
var
  Symbol1: TSymbol absolute Data1;
  Symbol2: TSymbol absolute Data2;
begin
  if Symbol1.WDCPrice > Symbol2.WDCPrice then
    Result := 1
  else if Symbol1.WDCPrice < Symbol2.WDCPrice  then
    Result := -1
  else
    Result := 0;
end;


function CompareKimpPrice2(Data1, Data2: Pointer): Integer;
var
  Symbol1: TSymbol absolute Data1;
  Symbol2: TSymbol absolute Data2;
begin
  if Symbol1.KimpPrice > Symbol2.KimpPrice then
    Result := 1
  else if Symbol1.KimpPrice < Symbol2.KimpPrice  then
    Result := -1
  else
    Result := 0;
end;

//-----------------------------------------------------------------------------

end.
