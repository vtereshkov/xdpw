// Sorting demo


program Sort;



const
  DataLength = 60;



type
  TNumber = Integer;
  
  TData = array [1..DataLength] of TNumber;
  PData = ^TData;



procedure Swap(var x, y: TNumber);
var
  buf: TNumber;
begin
buf := x;
x := y;
y := buf;
end;




function Partition(var data: TData; len: Integer): Integer;
var
  pivot: TNumber;
  pivotIndex, i: Integer;
begin
pivot := data[len];
pivotIndex := 1;

for i := 1 to len do
  if data[i] < pivot then
    begin
    Swap(data[pivotIndex], data[i]);
    Inc(pivotIndex);
    end; // if

Swap(data[len], data[pivotIndex]);

Result := pivotIndex;
end;




procedure QuickSort(var data: TData; len: Integer);
var
  pivotIndex: Integer;
  dataShiftedPtr: PData;
begin
if len > 1 then
  begin
  pivotIndex := Partition(data, len);
  dataShiftedPtr := PData(@data[pivotIndex + 1]);
  
  QuickSort(data,            pivotIndex - 1  );
  QuickSort(dataShiftedPtr^, len - pivotIndex);
  end; // if
end;




procedure BubbleSort(var data: TData; len: Integer);
var
  changed: Boolean;
  i: Integer;
begin
repeat
  changed := FALSE;

  for i := 1 to len - 1 do
    if data[i + 1] < data[i] then
      begin
      Swap(data[i + 1], data[i]);
      changed := TRUE;
      end;

until not changed; 
end;



procedure SelectionSort(var data: TData; len: Integer);
var
  i, j, extrIndex: Integer;
  extr: TNumber;
begin
for i := 1 to len do
  begin
  extr := data[i];
  extrIndex := i;

  for j := i + 1 to len do
    if data[j] < extr then
      begin
      extr := data[j];
      extrIndex := j;
      end;

  Swap(data[i], data[extrIndex]);
  end; // for
end;



var
  RandomData: TData;
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
  if i mod 4 <> 0 then Write(#9) else WriteLn;
  end;

WriteLn;
WriteLn;
Write('Select method (Q - quick, B - bubble, S - selection): '); Read(Method);
WriteLn(Method);
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
  if i mod 4 <> 0 then Write(#9) else WriteLn;
  end;
WriteLn;

WriteLn;
WriteLn('Done.');

ReadLn;
end.








