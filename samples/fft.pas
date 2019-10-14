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
  



procedure CAdd(var a, b, c: Complex);
begin
c.Re := a.Re + b.Re;
c.Im := a.Im + b.Im;
end;




procedure CSub(var a, b, c: Complex);
begin
c.Re := a.Re - b.Re;
c.Im := a.Im - b.Im;
end;




procedure CMul(var a, b, c: Complex);
begin
c.Re := a.Re * b.Re - a.Im * b.Im;
c.Im := a.Re * b.Im + a.Im * b.Re;
end;




function CAbs(var a: Complex): Real;
begin
CAbs := sqrt(a.Re * a.Re + a.Im * a.Im);
end;




procedure GetFFT(var x: TData; var FFT: TComplexData; Depth: Integer);
var
  k, HalfLen, Step: Integer;
  FFTEven, FFTOdd: TComplexData;
  FFTOddTwiddled: Complex;
  xShiftedPtr: PData;

begin
HalfLen := DataLength shr (Depth + 1);
Step := 1 shl Depth;

if HalfLen = 0 then
  begin
  FFT[0].Re := x[0];
  FFT[0].Im := 0;
  end
else
  begin
  xShiftedPtr := @x[Step];
  
  GetFFT(x,            FFTEven, Depth + 1);
  GetFFT(xShiftedPtr^, FFTOdd,  Depth + 1);

  for k := 0 to HalfLen - 1 do
    begin
    CMul(FFTOdd[k], Twiddle[k * Step], FFTOddTwiddled);

    CAdd(FFTEven[k], FFTOddTwiddled, FFT[k]);
    CSub(FFTEven[k], FFTOddTwiddled, FFT[k + HalfLen]);
    end; // for
  end; // else

end;




procedure Spectrum(var x, S: TData);
var
  FFT: TComplexData;
  i: Integer;
begin
for i := 0 to DataLength - 1 do
  begin
  Twiddle[i].Re :=  cos(2 * Pi * i / DataLength);
  Twiddle[i].Im := -sin(2 * Pi * i / DataLength);
  end;

GetFFT(x, FFT, 0);

for i := 0 to DataLength - 1 do
  S[i] := CAbs(FFT[i]);

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
Spectrum(x, S);

for i := 0 to DataLength shr 1 - 1 do  
  WriteLn('Freq ', i: 5, '   Mag ', S[i] * 2 / DataLength: 10: 4);

ReadLn;
end.


  