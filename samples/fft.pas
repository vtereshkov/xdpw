// Fast Fourier Transform demo program

{$APPTYPE CONSOLE}

program FFT;



const
  DataLength = 256; 



type
  Complex = record
    Re, Im: Real;
  end;
  
  TData = array [0..DataLength - 1] of Real;
  TComplexData = array [0..DataLength - 1] of Complex;
  
  PData = ^TData;



var
  x, S: TData;
  Twiddle: TComplexData;
  



function CAdd(var a, b: Complex): Complex;
begin
Result.Re := a.Re + b.Re;
Result.Im := a.Im + b.Im;
end;




function CSub(var a, b: Complex): Complex;
begin
Result.Re := a.Re - b.Re;
Result.Im := a.Im - b.Im;
end;




function CMul(var a, b: Complex): Complex;
begin
Result.Re := a.Re * b.Re - a.Im * b.Im;
Result.Im := a.Re * b.Im + a.Im * b.Re;
end;




function CAbs(var a: Complex): Real;
begin
CAbs := sqrt(a.Re * a.Re + a.Im * a.Im);
end;




function GetFFT(var x: TData; Depth: Integer): TComplexData;
var
  k, HalfLen, Step: Integer;
  FFTEven, FFTOdd: TComplexData;
  FFTOddTwiddled: Complex;

begin
HalfLen := DataLength shr (Depth + 1);
Step := 1 shl Depth;

if HalfLen = 0 then
  begin
  Result[0].Re := x[0];
  Result[0].Im := 0;
  end
else
  begin  
  FFTEven := GetFFT(x,                Depth + 1);
  FFTOdd  := GetFFT(PData(@x[Step])^, Depth + 1);

  for k := 0 to HalfLen - 1 do
    begin
    FFTOddTwiddled := CMul(FFTOdd[k], Twiddle[k * Step]);

    Result[k]           := CAdd(FFTEven[k], FFTOddTwiddled);
    Result[k + HalfLen] := CSub(FFTEven[k], FFTOddTwiddled);
    end; // for
  end; // else

end;




function Spectrum(var x: TData): TData;
var
  FFT: TComplexData;
  i: Integer;
begin
for i := 0 to DataLength - 1 do
  begin
  Twiddle[i].Re :=  cos(2 * Pi * i / DataLength);
  Twiddle[i].Im := -sin(2 * Pi * i / DataLength);
  end;

FFT := GetFFT(x, 0);

for i := 0 to DataLength - 1 do
  Result[i] := CAbs(FFT[i]);

end;




var
  Mag, Period: array [0..4] of Real;
  Phase: Real;
  i, j: Integer;


begin
Randomize;

// Four random sinusoids and a constant bias
for j := 0 to 4 do
  begin
  Mag[j]    := (Random - 0.5) * 100;
  Period[j] := 2 + abs(Random - 0.5) * 40;
  end;

for i := 0 to DataLength - 1 do
  begin
  Phase := 2 * Pi * i;

  x[i] := Mag[0] / 2;

  for j := 1 to 4 do
    x[i] := x[i] + Mag[j] * sin(Phase / Period[j]);
  end; // for

// FFT
S := Spectrum(x);

for i := 0 to DataLength shr 1 - 1 do  
  WriteLn('Freq ', i: 5, '   Mag ', S[i] * 2 / DataLength: 10: 4);

ReadLn;
end.


  