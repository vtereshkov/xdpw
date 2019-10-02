// The Game of Life

{$APPTYPE CONSOLE}

program Life;


const
  FieldSize = 20;


type
  TField = array [1..FieldSize * FieldSize] of Boolean;


var
  Fld: TField;



function ind(i, j: Integer): Integer;              // Linear index of a cell modulo field size
begin
while i > FIELDSIZE do i := i - FIELDSIZE;
while i < 1         do i := i + FIELDSIZE;
while j > FIELDSIZE do j := j - FIELDSIZE;
while j < 1         do j := j + FIELDSIZE;

Result := FIELDSIZE * (i - 1) + j;
end;




procedure Redraw;
var
  i, j: Integer;
  ch: Char;
begin
for i := 1 to FieldSize do
  begin
  for j := 1 to FieldSize do
    begin
    if Fld[ind(i, j)] then ch := 'O' else ch := '.';
    Write(ch);
    end;
  WriteLn;
  end;
WriteLn;  
end; // Redraw




procedure Init;
var
  i, j: Integer;
begin
Randomize;

for i := 1 to FieldSize do
  for j := 1 to FieldSize do
    Fld[ind(i, j)] := Random > 0.5;
end; // Init




procedure Regenerate;
var
  NextFld: TField;
  i, j, ni, nj, n: Integer;
begin

for i := 1 to FieldSize do
  for j := 1 to FieldSize do
    begin
    // Count cell neighbors
    n := 0;
    for ni := i - 1 to i + 1 do
      for nj := j - 1 to j + 1 do
        if Fld[ind(ni, nj)] and not ((ni = i) and (nj = j)) then Inc(n);

    // Revive or kill the current cell in the next generation
    if Fld[ind(i, j)] then
      NextFld[ind(i, j)] := (n > 1) and (n < 4)  // Kill the cell or keep it alive
    else
      NextFld[ind(i, j)] := n = 3;               // Revive the cell or keep it dead
    end; // for j...

// Make new generation
for i := 1 to FieldSize do
  for j := 1 to FieldSize do
    Fld[ind(i, j)] := NextFld[ind(i, j)];

end; // Regenerate




var
  ch: Char;

begin
// Create initial population
Init;

// Run simulation
repeat   
  Redraw;
  Regenerate;
  ReadLn(ch);
until ch = 'q';

end.

  

