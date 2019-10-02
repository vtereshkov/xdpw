// Inertial navigation system error estimator demo

{$APPTYPE CONSOLE}

program INSErr;


{$I samples\kalman.inc}



const
  g     = 9.81;
  Re    = 6378e3;
  dt    = 0.1;
  A     = 100.0;
  beta  = 1e-4;
  deg   = pi / 180;
  hr    = 3600;
  tstop = 2 * hr;



type
  TModel = record
    dV, Phi, omega, z: Real;
  end;  
  



function GaussRnd(m, sigma: Real): Real;
var
  s: Real;
  i: SmallInt;
begin
s := 0;

for i := 1 to 12 do
  s := s + Random;

Result := m + sigma * (s - 6);
end;




procedure InitModel(var M: TModel);
begin
M.dV    := 0;
M.Phi   := GaussRnd(0, 0.1 * deg);
M.omega := GaussRnd(0, 0.5 * deg / hr);
end;



procedure ExecuteModel(var M: TModel);
var
  dVdot, Phidot, omegadot: Real;
begin
dVdot    := -g * M.Phi;
Phidot   :=  M.dV / Re + M.omega;
omegadot := -beta * M.omega + A * sqrt(2 * beta) * GaussRnd(0, 0.0000001);

M.dV     := M.dV + dVdot * dt;
M.Phi    := M.Phi + Phidot * dt;
M.omega  := M.omega + omegadot * dt;

M.z      := M.dV + GaussRnd(0, 3.0);
end;



procedure InitSchulerKF(var KF: TKalmanFilter; Q, R: Real);
begin
{
The following INS error model is used:
Velocity error: dV'    = -g * F
Attitude error: Phi'   = dV / R + omega
Gyro bias     : omega' = -beta * omega + A * sqrt(2 * beta) * w
Measurements  : z      = dV + v
}

KF.n := 3;  KF.m := 1;  KF.s := 1;

KF.Phi[1, 1] := 1;       KF.Phi[1, 2] := -g * dt;  KF.Phi[1, 3] := 0;
KF.Phi[2, 1] := dt / Re; KF.Phi[2, 2] := 1;        KF.Phi[2, 3] := dt;
KF.Phi[3, 1] := 0;       KF.Phi[3, 2] := 0;        KF.Phi[3, 3] := 1 - beta * dt;

KF.H[1, 1] := 1;         KF.H[1, 2] := 0;          KF.H[1, 3] := 0;

KF.G[1, 1] := 0;
KF.G[2, 1] := 0;
KF.G[3, 1] := A * sqrt(2 * beta) * dt;

KF.Q[1, 1] := Q;

KF.R[1, 1] := R;

KF.x[1, 1] := 0;         KF.x[2, 1] := 0;          KF.x[3, 1] := 0;
 
KF.P[1, 1] := 1;         KF.P[1, 2] := 0;          KF.P[1, 3] := 0;
KF.P[2, 1] := 0;         KF.P[2, 2] := 1;          KF.P[2, 3] := 0;
KF.P[3, 1] := 0;         KF.P[3, 2] := 0;          KF.P[3, 3] := 1;
end;




var
  Model: TModel;
  Filter: TKalmanFilter;
  i: Integer;



begin
Randomize;

InitModel(Model);
InitSchulerKF(Filter, 1e-10, 1e6);

for i := 0 to Round(tstop / dt) do
  begin
  ExecuteModel(Model);
  Filter.z[1, 1] := Model.z;
  ExecuteFilter(Filter);

  if i mod 10 = 0 then
    WriteLn(  't ', i * dt: 6: 1,
            '  dVel ', Model.dV: 6: 1,                 Filter.x[1, 1]: 6: 1, 
            '  dAtt ', Model.Phi / deg: 6: 2,          Filter.x[2, 1] / deg: 6: 2,
            '  Bias ', Model.omega / (deg / hr): 6: 2, Filter.x[3, 1] / (deg / hr): 6: 2);
  end; // for

WriteLn('Done.');
ReadLn;
end.
