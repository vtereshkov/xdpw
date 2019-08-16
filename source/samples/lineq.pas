// Linear equations solver


program LinEq;



{$I samples\gauss.inc}



procedure Error;
begin
WriteLn;
WriteLn('Error: ', E, '.');
ReadLn;
Halt(1);
end;



var
  A: TMatrix;
  x: TVector;
  m, i, j: Integer;

  DatName, Comment: string;
  DatFile: Text;
  Err: Integer;



begin
WriteLn;
WriteLn('Linear equations solver');
WriteLn;
Write('File name   : '); ReadLn(DatName);
WriteLn;

Reset(DatFile, DatName);
Err := IOResult;
if Err <> 0 then
  begin
  WriteLn('Unable to open file: ', DatName, ' (error code ', Err, ')');
  ReadLn;
  Halt(1);
  end;

ReadLn(DatFile, Comment);
WriteLn('Comment     : ', Comment);
WriteLn;

ReadLn(DatFile, m);
WriteLn('System order: ', m);
WriteLn;

WriteLn('Augmented ', m, ' x ', m + 1, ' matrix: ');
WriteLn;

for i := 1 to m do
  begin
  for j := 1 to m + 1 do
    begin
    Read(DatFile, A[i, j]);
    Write(A[i, j], ' ');
    end;
  ReadLn(DatFile);  
  WriteLn;
  end;

Close(DatFile);

SolveLinearSystem(A, x, m);

WriteLn;
WriteLn('Triangularized matrix:');
WriteLn;

for i := 1 to m do
  begin
  for j := 1 to m + 1 do
    Write(A[i, j], ' ');
  WriteLn;
  end;

WriteLn;
WriteLn('Solution: ');
WriteLn;

for i := 1 to m do
  WriteLn('x', i, ' = ', x[i]);

WriteLn;
WriteLn('Done.');
ReadLn;
end.
