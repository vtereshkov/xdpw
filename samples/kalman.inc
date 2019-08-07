// Kalman filter implementation



const
  MAXORDER   = 3;



type
  TMatrix = array [1..MAXORDER, 1..MAXORDER] of Real;
  
  TKalmanFilter = record
    n, m, s: SmallInt;
    x, xapri, z: TMatrix;
    Phi, G, H, Q, R, P, Papri, K: TMatrix;
  end;  





procedure Transpose(m, n: SmallInt; var C, CT: TMatrix);
var
  i, j: SmallInt;
begin
for i := 1 to m do
  for j := 1 to n do
    CT[j, i] := C[i, j];
end;




// C = C1 + C2
procedure Add(m, n: SmallInt; var C1, C2, C: TMatrix);
var
  i, j: SmallInt;
begin
for i := 1 to m do
  for j := 1 to n do
    C[i, j] := C1[i, j] + C2[i, j];
end;




// C = C1 - C2
procedure Sub(m, n: SmallInt; var C1, C2, C: TMatrix);
var
  i, j: SmallInt;
begin
for i := 1 to m do
  for j := 1 to n do
    C[i, j] := C1[i, j] - C2[i, j];
end;




// C = C1 * C2
procedure Mult(m1, n1, n2: SmallInt; var C1, C2, C: TMatrix);
var
  i, j, k: SmallInt;
begin
for i := 1 to m1 do
  for j := 1 to n2 do
    begin 
    C[i, j] := 0;
    for k := 1 to n1 do
      C[i, j] := C[i, j] + C1[i, k] * C2[k, j];
    end;
end;




// Cs = B  * C  * BT
// mm   mn   nn   nm
procedure Similarity(m, n: SmallInt; var B, C, Cs: TMatrix);
var
  BT, BC: TMatrix;
begin
Mult(m, n, n, B, C, BC);
Transpose(m, n, B, BT);
Mult(m, n, m, BC, BT, Cs);
end;




procedure Identity(n: SmallInt; var E: TMatrix);
var
  i, j: SmallInt;
begin
for i := 1 to n do
  for j := 1 to n do
    if i = j then E[i, j] := 1 else E[i, j] := 0;
end;
    



procedure Inverse(m: SmallInt; var C, Cinv: TMatrix);
var
  big, fabval, pivinv, temp: Real;
  i, j, k, l, ll, irow, icol: SmallInt;
  indxc, indxr, ipiv: array [1..MAXORDER] of SmallInt;

begin
  for i := 1 to m do
    for j := 1 to m do
      Cinv[i, j] := C[i, j];

  for j := 1 to m do
    ipiv[j] := 0;

  icol := 1; irow := 1;  

  for i := 1 to m do
    begin
    big := 0;

    for j := 1 to m do
      if ipiv[j] <> 1 then
        for k := 1 to m do
          begin
          if ipiv[k] = 0 then
            begin
            if Cinv[j, k] < 0 then fabval := -Cinv[j, k] else fabval := Cinv[j, k];

            if fabval >= big then
              begin
              big := fabval;
              irow := j;
              icol := k;
              end;
            end // if
          else
            begin
            // Singular matrix
            end; // else
          end; // for

    Inc(ipiv[icol]);

    if irow <> icol then
      for l := 1 to m do
        begin
        temp := Cinv[irow, l];
        Cinv[irow, l] := Cinv[icol, l];
        Cinv[icol, l] := temp;
        end;

    indxr[i] := irow;
    indxc[i] := icol;

    pivinv := 1 / Cinv[icol, icol];
    Cinv[icol, icol] := 1;

    for l := 1 to m do
      Cinv[icol, l] := Cinv[icol, l] * pivinv;

    for ll := 1 to m do
      if ll <> icol then
        begin
        temp := Cinv[ll, icol];
        Cinv[ll, icol] := 0;
        for l := 1 to m do Cinv[ll, l] := Cinv[ll, l] - Cinv[icol, l] * temp;
        end; // for
  end; // for

  for l := m downto 1 do
    begin
    if indxr[l] <> indxc[l] then
      for k := 1 to m do
        begin
        temp := Cinv[k, indxr[l]];
        Cinv[k, indxr[l]] := Cinv[k, indxc[l]];
        Cinv[k, indxc[l]] := temp;
        end; // for
    end; //for
end;




procedure ExecuteFilter(var KF: TKalmanFilter);
var
  PhiPPhiT, GQGT, HT, HPapriHT, HPapriHTplusR, HPapriHTplusRinv, PapriHT, Hxapri, nu, Knu, KH, EminusKH, E: TMatrix;
begin
{All variable names correspond to the notation in the book:
 Salychev O. S. Applied Inertial Navigation
 'apri' means 'a priori' and stands for 'k/k-1'}

// A priori state vector estimate
Mult(KF.n, KF.n, 1, KF.Phi, KF.x, KF.xapri);

// A priori variance matrix
Similarity(KF.n, KF.n, KF.Phi, KF.P, PhiPPhiT);
Similarity(KF.n, KF.s, KF.G, KF.Q, GQGT);

Add(KF.n, KF.n, PhiPPhiT, GQGT, KF.Papri);

// Gain matrix
Similarity(KF.m, KF.n, KF.H, KF.Papri, HPapriHT);
Add(KF.m, KF.m, HPapriHT, KF.R, HPapriHTplusR);
Inverse(KF.m, HPapriHTplusR, HPapriHTplusRinv);

Transpose(KF.m, KF.n, KF.H, HT);
Mult(KF.n, KF.n, KF.m, KF.Papri, HT, PapriHT);

Mult(KF.n, KF.m, KF.m, PapriHT, HPapriHTplusRinv, KF.K);

// A posteriori state vector estimate
Mult(KF.m, KF.n, 1, KF.H, KF.xapri, Hxapri);
Sub(KF.m, 1, KF.z, Hxapri, nu);
Mult(KF.n, KF.m, 1, KF.K, nu, Knu);

Add(KF.n, 1, KF.xapri, Knu, KF.x);

// A posteriori variance matrix
Mult(KF.n, KF.m, KF.n, KF.K, KF.H, KH);
Identity(KF.n, E);
Sub(KF.n, KF.n, E, KH, EminusKH);

Mult(KF.n, KF.n, KF.n, EminusKH, KF.Papri, KF.P);
end;

