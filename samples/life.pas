// The Game of Life

{$APPTYPE CONSOLE}

program Life;


const
  Width = 8;
  Height = 25;


type
  TField = array [0..Height - 1, 0..Width - 1] of Boolean;


var
  Fld: TField;



procedure Redraw;
var
  i, j: Integer;
  ch: Char;
  s: string;
begin
s := '';
for i := 0 to Height - 1 do
  begin
  for j := 0 to Width - 1 do
    begin
    if Fld[i, j] then ch := 'O' else ch := '.';
    s := s + ch;
    end;  
  if i < Height - 1 then s := s + #13 + #10;
  end;
Write(s);
end; // Redraw




procedure Init;
var
  i, j: Integer;
begin
Randomize;

for i := 0 to Height - 1 do
  for j := 0 to Width - 1 do
    Fld[i, j] := Random > 0.5;
end; // Init




procedure Regenerate;
var
  NextFld: TField;
  i, j, ni, nj, n: Integer;
begin

for i := 0 to Height - 1 do
  for j := 0 to Width - 1 do
    begin
    // Count cell neighbors
    n := 0;
    for ni := i - 1 to i + 1 do
      for nj := j - 1 to j + 1 do
        if Fld[(ni + Height) mod Height, (nj + Width) mod Width] and not ((ni = i) and (nj = j)) then Inc(n);

    // Revive or kill the current cell in the next generation
    if Fld[i, j] then
      NextFld[i, j] := (n > 1) and (n < 4)  // Kill the cell or keep it alive
    else
      NextFld[i, j] := n = 3;               // Revive the cell or keep it dead
    end; // for j...

// Make new generation
Fld := NextFld;
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
until (ch = 'Q') or (ch = 'q');

end.

  

