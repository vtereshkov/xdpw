// Implementation of Gauss' method for linear systems

unit Gauss;


interface


const
  MAXSIZE = 10;


type
  TVector = array [1..MAXSIZE] of Real;
  TMatrix = array [1..MAXSIZE] of TVector;

  TError = procedure (const E: string);


procedure SolveLinearSystem(var T: TMatrix; var x: TVector; m: Integer; Error: TError);



implementation



procedure TriangularizeMatrix(var T: TMatrix; m: Integer; Error: TError);
var
  i, j, k: Integer;
  r: Real;
begin
for k := 1 to m - 1 do
  for i := k + 1 to m do
    begin
    if T[k, k] = 0 then Error('Diagonal element is zero');

    r := -T[i, k] / T[k, k];

    for j := k to m + 1 do
      T[i, j] := T[i, j] + r * T[k, j];
    end;
end;



procedure SolveLinearSystem{(var T: TMatrix; var x: TVector; m: Integer; Error: TError)};
var
  i, j: Integer;
  s: Real;

begin
TriangularizeMatrix(T, m, Error);

for i := m downto 1 do
  begin
  s := T[i, m + 1];
  for j := m downto i + 1 do
    s := s - T[i, j] * x[j];

  if T[i, i] = 0 then Error('Singular matrix');

  x[i] := s / T[i, i];
  end; // for

end;


end.


