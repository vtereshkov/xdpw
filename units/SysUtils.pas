// XD Pascal - a 32-bit compiler for Windows
// Copyright (c) 2009-2010, 2019-2020, Vasiliy Tereshkov

unit SysUtils;


interface


type
  AnsiChar = Char;
  PAnsiChar = PChar;
  
  WideChar = Word;
  PWideChar = ^WideChar;
  WideString = array [1..MaxStrLength + 1] of WideChar;

 

function StrToPChar(const s: string): PChar;
function PCharToStr(p: PChar): string;
function StrToPWideChar(const s: string): PWideChar;
function PWideCharToStr(p: PWideChar): string; 



implementation


var
  WideStringBuf: WideString;
  

  
  
function StrToPChar{(const s: string): PChar};
begin
Result := @s[1];
end;




function PCharToStr{(p: PChar): string};
var
  i: Integer;
begin
i := 0;
repeat
  Inc(i);
  Result[i] := p^;
  p := PChar(Integer(p) + SizeOf(Char));
until Result[i] = #0;
end;




function StrToPWideChar{(const s: string): PWideChar};
var
  i: Integer;  
begin
i := 0;
repeat
  Inc(i);
  WideStringBuf[i] := Ord(s[i]);
until s[i] = #0;
Result := @WideStringBuf;  
end;




function PWideCharToStr{(p: PWideChar): string};
var
  i: Integer;
begin
i := 0;
repeat
  Inc(i);
  Result[i] := Char(p^);
  p := PWideChar(Integer(p) + SizeOf(WideChar));
until Result[i] = #0;
end;


end.
  