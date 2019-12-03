// Raytracer demo - demonstrates XD Pascal methods and interfaces

{$APPTYPE CONSOLE}

program Raytracer;


type 
  TVec = record 
    x, y, z: Real;
  end;
  

function add for u: TVec (var v: TVec): TVec; 
begin 
with Result do begin x := u.x + v.x;  y := u.y + v.y;  z := u.z + v.z; end; 
end;


function sub for u: TVec (var v: TVec): TVec; 
begin 
with Result do begin x := u.x - v.x;  y := u.y - v.y;  z := u.z - v.z; end; 
end;


function mul for v: TVec (a: Real): TVec; 
begin 
with Result do begin x := v.x * a;  y := v.y * a;  z := v.z * a; end; 
end;


function dot for u: TVec (var v: TVec): Real; 
begin 
Result := u.x * v.x + u.y * v.y + u.z * v.z; 
end;


function elementwise for u: TVec (var v: TVec): TVec; 
begin 
with Result do begin x := u.x * v.x;  y := u.y * v.y;  z := u.z * v.z; end; 
end;


function norm(var v: TVec): Real;
begin 
Result := sqrt(v.dot(v));
end;


function normalize(var v: TVec): TVec;
begin 
Result := v.mul(1.0 / norm(v));
end;


function rand: TVec;
begin
with Result do begin x := Random;  y := Random;  z := Random; end; 
end;


type
  TColor = TVec;

  TRay = record
    Origin, Dir: TVec;
  end;
  
  TGenericBody = record
    Center: TVec;
    Color: TColor;
    Diffuseness: Real;
    IsLamp: Boolean;
  end; 

  PGenericBody = ^TGenericBody; 


function LambertFactor for b: TGenericBody (Lambert: Real): Real;
begin
Result := 1.0 - (1.0 - Lambert) * b.Diffuseness;
end;


type 
  TBox = record
    GenericBody: TGenericBody;
    HalfSize: TVec;
  end;


function GetGenericBody for b: TBox: PGenericBody; 
begin 
Result := @b.GenericBody; 
end;


function Intersect for b: TBox (var Ray: TRay; var Point, Normal: TVec): Boolean; 

  function Within(x, y, xmin, ymin, xmax, ymax: Real): Boolean;
  begin
  Result := (x > xmin) and (x < xmax) and (y > ymin) and (y < ymax);
  end;
  
var
  Side, Factor: Real;  
  
begin
if abs(Ray.Dir.z) > 1e-9 then // xy
  begin 
  Side := 1.0;
  if Ray.Dir.z > 0.0 then Side := -1.0;
  
  Factor := (b.GenericBody.Center.z + Side * b.HalfSize.z - Ray.Origin.z) / Ray.Dir.z;  
  if Factor > 0.1 then
    begin
    Point := Ray.Origin.add(Ray.Dir.mul(Factor));
    
    if Within(Point.x, Point.y,
              b.GenericBody.Center.x - b.HalfSize.x, b.GenericBody.Center.y - b.HalfSize.y,
              b.GenericBody.Center.x + b.HalfSize.x, b.GenericBody.Center.y + b.HalfSize.y)
    then
      begin
      with Normal do begin x := 0; y := 0; z := Side; end;
      Result := TRUE;
      Exit;
      end;
    end;
  end;

if abs(Ray.Dir.x) > 1e-9 then // yz
  begin 
  Side := 1.0;
  if Ray.Dir.x > 0.0 then Side := -1.0;
  
  Factor := (b.GenericBody.Center.x + Side * b.HalfSize.x - Ray.Origin.x) / Ray.Dir.x;  
  if Factor > 0.1 then
    begin
    Point := Ray.Origin.add(Ray.Dir.mul(Factor));
    
    if Within(Point.y, Point.z,
              b.GenericBody.Center.y - b.HalfSize.y, b.GenericBody.Center.z - b.HalfSize.z,
              b.GenericBody.Center.y + b.HalfSize.y, b.GenericBody.Center.z + b.HalfSize.z) 
    then
      begin
      with Normal do begin x := Side; y := 0; z := 0; end;
      Result := TRUE;
      Exit;
      end;
    end;
  end;

if abs(Ray.Dir.y) > 1e-9 then // zx
  begin 
  Side := 1.0;
  if Ray.Dir.y > 0.0 then Side := -1.0;
  
  Factor := (b.GenericBody.Center.y + Side * b.HalfSize.y - Ray.Origin.y) / Ray.Dir.y;  
  if Factor > 0.1 then
    begin
    Point := Ray.Origin.add(Ray.Dir.mul(Factor));
    
    if Within(Point.z, Point.x,
              b.GenericBody.Center.z - b.HalfSize.z, b.GenericBody.Center.x - b.HalfSize.x,
              b.GenericBody.Center.z + b.HalfSize.z, b.GenericBody.Center.x + b.HalfSize.x) 
    then
      begin
      with Normal do begin x := 0; y := Side; z := 0; end;
      Result := TRUE;
      Exit;
      end;
    end;
  end;

Result := FALSE;
end;


type 
  TSphere = record
    GenericBody: TGenericBody;
    Radius: Real;
  end;
  
  
function GetGenericBody for s: TSphere: PGenericBody; 
begin 
Result := @s.GenericBody; 
end;  
  

function Intersect for s: TSphere (var Ray: TRay; var Point, Normal: TVec): Boolean;
var
  Displacement: TVec;
  Proj, Discr, Factor: Real;
begin
Displacement := s.GenericBody.Center.sub(Ray.Origin);
Proj := Displacement.dot(Ray.Dir);
Discr := sqr(s.Radius) + sqr(Proj) - Displacement.dot(Displacement);

if Discr > 0 then
  begin
  Factor := Proj - sqrt(Discr);
  if Factor > 0.1 then
    begin
    Point := Ray.Origin.add(Ray.Dir.mul(Factor));
    Normal := Point.sub(s.GenericBody.Center).mul(1.0 / s.Radius);
    Result := TRUE;
    Exit;
    end;
  end;

Result := FALSE;
end;


type
  IBody = interface
    GetGenericBody: function: PGenericBody;
    Intersect: function (var Ray: TRay; var Point, Normal: TVec): Boolean;
  end;


const
  MAXBODIES = 10;
  

type 
  TScene = record
    AmbientColor: TColor;
    Body: array [1..MAXBODIES] of IBody;
    NumBodies: Integer;
  end;
  

function Trace for sc: TScene (var Ray: TRay; Depth: Integer): TColor;
var
  BestBody: PGenericBody;
  Point, Normal, BestPoint, BestNormal, SpecularDir, DiffuseDir, RandomVec: TVec;
  DiffuseRay: TRay;
  Dist, BestDist, Lambert: Real;
  BestIndex, i: Integer;
  
begin
if Depth > 3 then
  begin
  Result := sc.AmbientColor;
  Exit;
  end;

// Find nearest intersection
BestDist := 1e9;
BestIndex := 0;

for i := 1 to sc.NumBodies do
  begin  
  if sc.Body[i].Intersect(Ray, Point, Normal) then
    begin
    Dist := norm(Point.sub(Ray.Origin));
    if Dist < BestDist then
      begin
      BestDist := Dist;
      BestIndex := i;
      BestPoint := Point;
      BestNormal := Normal;
      end;
    end;
  end;

// Reflect rays
if BestIndex > 0 then
  begin
  BestBody := sc.Body[BestIndex].GetGenericBody();

  if BestBody^.IsLamp then
    begin
    Result := BestBody^.Color;
    Exit;
    end;

  RandomVec := rand;
  SpecularDir := Ray.Dir.sub(BestNormal.mul(2.0 * (Ray.Dir.dot(BestNormal))));
  DiffuseDir := normalize(SpecularDir.add(RandomVec.mul(2.0 * BestBody^.Diffuseness)));

  Lambert := DiffuseDir.dot(BestNormal);
  if Lambert < 0 then
    begin
    DiffuseDir := DiffuseDir.sub(BestNormal.mul(2.0 * Lambert));
    Lambert := -Lambert;
    end;

  DiffuseRay.Origin := BestPoint;
  DiffuseRay.Dir := DiffuseDir;

  Result := sc.Trace(DiffuseRay, Depth + 1).mul(BestBody^.LambertFactor(Lambert)).elementwise(BestBody^.Color);
  Exit;
  end;

Result := sc.AmbientColor;
end;


// Main program

const  
  // Define scene
  Box1: TBox = 
    (
    GenericBody: 
      (
      Center: (x: 500; y: -100; z: 1200);
      Color: (x: 0.4; y: 0.7; z: 1.0);
      Diffuseness: 0.1;
      IsLamp: FALSE
      );      
    HalfSize: (x: 400 / 2; y: 600 / 2; z: 300 / 2)
    );

  Box2: TBox = 
    (
    GenericBody:
      (
      Center: (x: 550; y: 210; z: 1100);
      Color: (x: 0.9; y: 1.0; z: 0.6);
      Diffuseness: 0.3;
      IsLamp: FALSE
      );      
    HalfSize: (x: 1000 / 2; y: 20 / 2; z: 1000 / 2)
    );

  Sphere1: TSphere = 
    (
    GenericBody:
      (
      Center: (x: 600; y: 0; z: 700);
      Color: (x: 1.0; y: 0.4; z: 0.6);
      Diffuseness: 0.2;
      IsLamp: FALSE
      );      
    Radius: 200
    );

  Sphere2: TSphere = 
    (
    GenericBody:
      (
      Center: (x: 330; y: 150; z: 700);
      Color: (x: 1.0; y: 1.0; z: 0.3);
      Diffuseness: 0.15;
      IsLamp: FALSE
      );      
    Radius: 50
    );

  // Define light
  Lamp1: TSphere = 
    (
    GenericBody:
      (
      Center: (x: 500; y: -1000; z: -700);
      Color: (x: 1.0; y: 1.0; z: 1.0);
      Diffuseness: 1.0;
      IsLamp: TRUE
      );
    Radius: 800
    );
    
  AmbientLightColor: TColor = (x: 0.2; y: 0.2; z: 0.2);  


  // Define eye
  Pos: TVec = (x: 0; y: 0; z: 0);
  Azimuth = 30.0 * pi / 180.0;
  Width = 640;
  Height = 480;
  Focal = 500;
  Antialiasing = 1.0;
  
  
var
  Scene: TScene;
  Dir, RotDir, RandomDir, RandomVec: TVec;
  Color: TColor;
  Ray: TRay;
  sinAz, cosAz: Real;
  F: Text;
  Rays, i, j, r: Integer; 
  

begin
WriteLn('Raytracer demo');
WriteLn;
Write('Rays per pixel (recommended 1 to 100): '); ReadLn(Rays);
WriteLn;

Randomize;

with Scene do
  begin 
  AmbientColor := AmbientLightColor;
  Body[1] := IBody(Box1);
  Body[2] := IBody(Box2);
  Body[3] := IBody(Sphere1);
  Body[4] := IBody(Sphere2);
  Body[5] := IBody(Lamp1);
  NumBodies := 5;
  end;
  
sinAz := sin(Azimuth);  cosAz := cos(Azimuth);
  
Assign(F, 'scene.ppm');
Rewrite(F);
WriteLn(F, 'P3');
WriteLn(F, Width, ' ', Height);
WriteLn(F, 255);

for i := 0 to Height - 1 do
  begin
  for j := 0 to Width - 1 do
    begin
    with Color do 
      begin 
      x := 0; 
      y := 0; 
      z := 0; 
      end;
    
    with Dir do 
      begin 
      x := j - Width / 2;  
      y := i - Height / 2;  
      z := Focal; 
      end;

    with RotDir do
      begin
      x :=  Dir.x * cosAz + Dir.z * sinAz;
      y :=  Dir.y;
      z := -Dir.x * sinAz + Dir.z * cosAz;
      end;
  
    for r := 1 to Rays do
      begin
      RandomVec := rand();
      RandomDir := RotDir.add(RandomVec.mul(Antialiasing));
      Ray.Origin := Pos;
      Ray.Dir := normalize(RandomDir);
      Color := Color.add(Scene.Trace(Ray, 0));
      end;
      
    Color := Color.mul(255.0 / Rays);
    Write(F, Round(Color.x), ' ', Round(Color.y), ' ', Round(Color.z), ' ');
    end;
    
  WriteLn(F);
  WriteLn(i + 1, '/', Height);
  end;

Close(F);

WriteLn;
WriteLn('Done. See scene.ppm');
ReadLn;  
end.
