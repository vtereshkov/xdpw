// Sorting demo


program Sort;



type
  TNumber = Integer;



procedure Swap(var x, y: TNumber);
var
  buf: TNumber;
begin
buf := x;
x := y;
y := buf;
end;



procedure QuickSort(var data: array of TNumber; len: Integer);


  function Partition(var data: array of TNumber; low, high: Integer): Integer;
  var
    pivot: TNumber;
    pivotIndex, i: Integer;
  begin
  pivot := data[high];
  pivotIndex := low;

  for i := low to high - 1 do
    if data[i] < pivot then
      begin
      Swap(data[pivotIndex], data[i]);
      Inc(pivotIndex);
      end; // if

  Swap(data[high], data[pivotIndex]);

  Result := pivotIndex;
  end; 
  
  
  procedure Sort(var data: array of TNumber; low, high: Integer);
  var
    pivotIndex: Integer;
  begin
  if high > low then
    begin
    pivotIndex := Partition(data, low, high);
    
    Sort(data, low, pivotIndex - 1);
    Sort(data, pivotIndex + 1, high);
    end; // if
  end;  
    
  
begin
Sort(data, 0, len - 1);
end;



procedure BubbleSort(var data: array of TNumber; len: Integer);
var
  changed: Boolean;
  i: Integer;
begin
repeat
  changed := FALSE;

  for i := 0 to len - 2 do
    if data[i + 1] < data[i] then
      begin
      Swap(data[i + 1], data[i]);
      changed := TRUE;
      end;

until not changed; 
end;



procedure SelectionSort(var data: array of TNumber; len: Integer);
var
  i, j, extrIndex: Integer;
  extr: TNumber;
begin
for i := 0 to len - 1 do
  begin
  extr := data[i];
  extrIndex := i;

  for j := i + 1 to len - 1 do
    if data[j] < extr then
      begin
      extr := data[j];
      extrIndex := j;
      end;

  Swap(data[i], data[extrIndex]);
  end; // for
end;



function IsSorted(var data: array of TNumber; len: Integer): Boolean;
var
  i: Integer;
begin
Result := TRUE;
for i := 0 to len - 2 do
  if data[i + 1] < data[i] then
    Result := FALSE;
end;    



const
  DataLength = 600;



var
  RandomData: array [1..DataLength] of TNumber;
  i: Integer;
  Method: Char;



begin
WriteLn;
WriteLn('Sorting demo');
WriteLn;
WriteLn('Initial array: ');
WriteLn;

Randomize;

for i := 1 to DataLength do
  begin
  RandomData[i] := Round((Random - 0.5) * 1000000);
  Write(RandomData[i]);
  if i mod 6 <> 0 then Write(#9) else WriteLn;
  end;

WriteLn;
WriteLn;
Write('Select method (Q - quick, B - bubble, S - selection): '); Read(Method);
WriteLn;

case Method of
  'Q', 'q': 
    begin
    WriteLn('Quick sorting');
    QuickSort(RandomData, DataLength);
    end;
  'B', 'b':
    begin
    WriteLn('Bubble sorting');
    BubbleSort(RandomData, DataLength);
    end;
  'S', 's': 
    begin
    WriteLn('Selection sorting');
    SelectionSort(RandomData, DataLength);
    end
else
  WriteLn('Sorting method is not selected.');
  ReadLn;
  Halt;     
end;

WriteLn;
WriteLn('Sorted array: ');
WriteLn;

for i := 1 to DataLength do
  begin
  Write(RandomData[i]); 
  if i mod 6 <> 0 then Write(#9) else WriteLn;
  end;
WriteLn;

WriteLn('Sorted: ', IsSorted(RandomData, DataLength));
WriteLn('Done.');

ReadLn;
end.








