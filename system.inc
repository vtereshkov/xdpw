// XD Pascal - a 32-bit compiler for Windows
// Copyright (c) 2009-2010, 2019, Vasiliy Tereshkov

// System library



const
  pi = 3.1415927;
 
  
  // Windows API constants
  
  STD_INPUT_HANDLE      = -10;
  STD_OUTPUT_HANDLE     = -11;
  
  FILE_ATTRIBUTE_NORMAL = 128;
  
  CREATE_ALWAYS         = 2;
  OPEN_EXISTING         = 3;
  
  GENERIC_READ          = $80000000;
  GENERIC_WRITE         = $40000000;
  
  INVALID_HANDLE_VALUE  = -1;
  
  FILE_BEGIN            = 0;
  FILE_CURRENT          = 1;
  FILE_END              = 2;



type
  LongInt = Integer;  
  Single = Real;
  Text = file;  
  PChar = ^Char;
  
  TStream = record
    Data: PChar;
    Index: Integer;
  end;

  PStream = ^TStream;

  TSetStorage = array [0..255] of Boolean;  
  


var
  RandSeed: Integer;
  
  Heap: LongInt;
  
  Buffer: string;
  IOError: Integer;
  LastReadChar: Char;
  StdInputHandle, StdOutputHandle: file;
  
  
  
// Windows API functions

function GetCommandLineA: Pointer; external 'KERNEL32.DLL' name 'GetCommandLineA';

function GetProcessHeap: LongInt; external 'KERNEL32.DLL' name 'GetProcessHeap';

function HeapAlloc(hHeap,
                   dwFlags,
                   dwBytes: LongInt): Pointer; external 'KERNEL32.DLL' name 'HeapAlloc';

procedure HeapFree(hHeap,
                   dwFlags: LongInt; 
                   lpMem: Pointer); external 'KERNEL32.DLL' name 'HeapFree';

function GetStdHandle(nStdHandle: Integer): file; external 'KERNEL32.DLL' name 'GetStdHandle';

procedure SetConsoleMode(hConsoleHandle: file; 
                         dwMode: LongInt); external 'KERNEL32.DLL' name 'SetConsoleMode';

function CreateFileA(const lpFileName: string; 
                     dwDesiredAccess: LongInt;
                     dwShareMode: LongInt;
                     lpSecurityAttributes: Pointer; 
                     dwCreationDisposition, 
                     dwFlagsAndAttributes, 
                     hTemplateFile: LongInt): file; external 'KERNEL32.DLL' name 'CreateFileA';
                     
function SetFilePointer(hFile: file; 
                        lDistanceToMove: LongInt; 
                        pDistanceToMoveHigh: Pointer; 
                        dwMoveMethod: LongInt): LongInt; external 'KERNEL32.DLL' name 'SetFilePointer';

function GetFileSize(hFile: file; 
                     lpFileSizeHigh: Pointer): LongInt; external 'KERNEL32.DLL' name 'GetFileSize';        
                     
procedure WriteFile(hFile: file;
                    lpBuffer: Pointer;
                    nNumberOfBytesToWrite: LongInt;
                    var lpNumberOfBytesWritten: LongInt;
                    lpOverlapped: LongInt); external 'KERNEL32.DLL' name 'WriteFile';
                    
procedure ReadFile(hFile: file;
                   lpBuffer: Pointer;
                   nNumberOfBytesToRead: LongInt;
                   var lpNumberOfBytesRead: LongInt;
                   lpOverlapped: LongInt); external 'KERNEL32.DLL' name 'ReadFile';

procedure CloseHandle(hObject: Text); external 'KERNEL32.DLL' name 'CloseHandle';

function GetLastError: LongInt; external 'KERNEL32.DLL' name 'GetLastError';

function GetTickCount: LongInt; external 'KERNEL32.DLL' name 'GetTickCount';

procedure ExitProcess(uExitCode: Integer); external 'KERNEL32.DLL' name 'ExitProcess';



// Initialization


procedure InitSystem;
begin
Heap := GetProcessHeap;

StdInputHandle := Text(GetStdHandle(STD_INPUT_HANDLE));
StdOutputHandle := Text(GetStdHandle(STD_OUTPUT_HANDLE));
SetConsoleMode(StdInputHandle, $02F5);                      // set all flags except ENABLE_LINE_INPUT and ENABLE_WINDOW_INPUT 
IOError := 0;
end;



// Timer


function Timer: LongInt;
begin
Result := GetTickCount;
end;




// Heap operations


procedure GetMem(var P: Pointer; Size: Integer);
begin
P := HeapAlloc(Heap, 0, Size);
end;




procedure FreeMem(var P: Pointer; Size: Integer);
begin
HeapFree(Heap, 0, P);
end;




// Mathematical routines


procedure Randomize;
begin
RandSeed := Timer;
end;




function Random: Real;
begin
RandSeed := 1975433173 * RandSeed;
Result := 0.5 * (RandSeed / $7FFFFFFF + 1.0);
end;




function Min(x, y: Real): Real;
begin
if x < y then Result := x else Result := y;
end;




function IMin(x, y: Integer): Integer;
begin
if x < y then Result := x else Result := y;
end;





function Max(x, y: Real): Real;
begin
if x > y then Result := x else Result := y;
end;




function IMax(x, y: Integer): Integer;
begin
if x > y then Result := x else Result := y;
end;




// File and console I/O routines


procedure WriteConsole(Ch: Char);
var
  LenWritten: Integer;
begin
WriteFile(StdOutputHandle, @Ch, 1, LenWritten, 0);
end;




procedure ReadConsole(var Ch: Char);
var
  LenRead: Integer;
begin
ReadFile(StdInputHandle, @Ch, 1, LenRead, 0);
WriteConsole(Ch);
end;




procedure Rewrite(var F: file; const Name: string);
begin
F := CreateFileA(Name, GENERIC_WRITE, 0, nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
if Integer(F) = INVALID_HANDLE_VALUE then IOError := -2;
end;




procedure Reset(var F: file; const Name: string);
begin
F := CreateFileA(Name, GENERIC_READ or GENERIC_WRITE, 0, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
if Integer(F) = INVALID_HANDLE_VALUE then IOError := -2;
end;




procedure Close(var F: Text);
begin
CloseHandle(F);
end;



  
procedure BlockWrite(var F: file; var Buf; Len: Integer);
var
  LenWritten: Integer;
begin
WriteFile(F, @Buf, Len, LenWritten, 0);
end;




procedure BlockRead(var F: file; var Buf; Len: Integer; var LenRead: Integer);
begin
ReadFile(F, @Buf, Len, LenRead, 0);
if LenRead < Len then IOError := -1;
end;




procedure Seek(var F: file; Pos: Integer);
begin
Pos := SetFilePointer(F, Pos, nil, FILE_BEGIN);
end;




function FileSize(var F: file): Integer;
begin
Result := GetFileSize(F, nil);
end;




function FilePos(var F: file): Integer;
begin
Result := SetFilePointer(F, 0, nil, FILE_CURRENT);
end;




function EOF(var F: file): Boolean;
begin
if Integer(F) = 0 then
  Result := FALSE
else
  Result := FilePos(F) >= FileSize(F);
end;




function IOResult: Integer;
begin
Result := IOError;
IOError := 0;
end;




procedure WriteCh(F: Text; P: PStream; ch: Char);
var
  Dest: PChar;
begin
if P <> nil then             // String stream output
  begin                      
  Dest := PChar(Integer(P^.Data) + P^.Index);
  Dest^ := ch;
  Inc(P^.Index);
  end
else  
  if Integer(F) = 0 then     // Console output
    WriteConsole(ch)                
  else                       // File output
    BlockWrite(F, ch, 1);   
end;




procedure WriteInt(F: Text; P: PStream; Number: Integer);
var
  Digit, Weight: Integer;
  Skip: Boolean;

begin
if Number = 0 then
  WriteCh(F, P,  '0')
else
  begin
  if Number < 0 then
    begin
    WriteCh(F, P,  '-');
    Number := -Number;
    end;

  Weight := 1000000000;
  Skip := TRUE;

  while Weight >= 1 do
    begin
    if Number >= Weight then Skip := FALSE;

    if not Skip then
      begin
      Digit := Number div Weight;
      WriteCh(F, P,  Char(ShortInt('0') + Digit));
      Number := Number - Weight * Digit;
      end;

    Weight := Weight div 10;
    end; // while
  end; // else

end;




procedure WriteHex(F: Text; P: PStream; Number: Integer; Digits: ShortInt);
var
  i, Digit: ShortInt;
begin
for i := Digits - 1 downto 0 do
  begin
  Digit := (Number shr (i shl 2)) and $0F;
  if Digit <= 9 then Digit := ShortInt('0') + Digit else Digit := ShortInt('A') + Digit - 10;
  WriteCh(F, P,  Char(Digit));
  end; 
end;





procedure WritePointer(F: Text; P: PStream; Number: Integer);
begin
WriteHex(F, P, Number, 8);
end;





procedure WriteReal(F: Text; P: PStream; Number: Real);
const
  FracBits = 16;
var
  Integ, Frac, InvWeight, Digit, IntegExpon: Integer;
  Expon: Real;

begin
// Write sign
if Number < 0 then
  begin
  WriteCh(F, P,  '-');
  Number := -Number;
  end;

// Normalize number
if Number = 0 then Expon := 0 else Expon := ln(Number) / ln(10);
if (Expon > 8) or (Expon < -3) then
  begin
  IntegExpon := Trunc(Expon);
  if IntegExpon < 0 then Dec(IntegExpon);
  Number := Number / exp(IntegExpon * ln(10));
  end
else
  IntegExpon := 0;  

// Write integer part
Integ := Trunc(Number);
Frac  := Round((Number - Integ) * (1 shl FracBits));

WriteInt(F, P, Integ);  WriteCh(F, P, '.');

// Write fractional part
InvWeight := 10;

while InvWeight <= 10000 do
  begin
  Digit := (Frac * InvWeight) shr FracBits;
  if Digit > 9 then Digit := 9;
  WriteCh(F, P,  Char(ShortInt('0') + Digit));
  Frac := Frac - (Digit shl FracBits) div InvWeight;
  InvWeight := InvWeight * 10;
  end; // while

// Write exponent
if IntegExpon <> 0 then 
  begin
  WriteCh(F, P, 'e');  WriteInt(F, P, IntegExpon);
  end;
 
end;




procedure WriteString(F: Text; P: PStream; const s: string);
var
  i: Integer;
begin
i := 1;
while s[i] <> #0 do
  begin
  WriteCh(F, P, s[i]);
  Inc(i);
  end; 
end;




procedure WriteBoolean(F: Text; P: PStream; Flag: Boolean);
begin
if Flag then WriteString(F, P, 'TRUE') else WriteString(F, P, 'FALSE');
end;




procedure WriteNewLine(F: Text; P: PStream);
begin
WriteCh(F, P, #13);  WriteCh(F, P, #10);
end;




procedure ReadCh(F: Text; P: PStream; var ch: Char);
var
  Len: Integer;
  Dest: PChar;
begin
if P <> nil then                // String stream input
  begin                      
  Dest := PChar(Integer(P^.Data) + P^.Index);
  ch := Dest^;
  Inc(P^.Index);
  end
else  
  if Integer(F) = 0 then        // Console input
    begin
    ReadConsole(ch);
    if ch = #13 then WriteConsole(#10);
    end 
  else                          // File input
    begin
    BlockRead(F, ch, 1, Len);
    if ch = #10 then BlockRead(F, ch, 1, Len);
    if Len <> 1 then ch := #0;
    end;

LastReadChar := ch;             // Required by ReadNewLine
end;




procedure ReadInt(F: Text; P: PStream; var Number: Integer);
var
  Ch: Char;
  Negative: Boolean;

begin
Number := 0;

// Read sign
Negative := FALSE;
ReadCh(F, P, Ch);
if Ch = '+' then
  ReadCh(F, P, Ch)
else if Ch = '-' then   
  begin
  Negative := TRUE;
  ReadCh(F, P, Ch);
  end;

// Read number
while (Ch >= '0') and (Ch <= '9') do
  begin
  Number := Number * 10 + ShortInt(Ch) - ShortInt('0');
  ReadCh(F, P, Ch);
  end; 

if Negative then Number := -Number;
end;




procedure ReadSmallInt(F: Text; P: PStream; var Number: SmallInt);
var
  IntNumber: Integer;
begin
ReadInt(F, P, IntNumber);
Number := IntNumber;
end;
  



procedure ReadShortInt(F: Text; P: PStream; var Number: ShortInt);
var
  IntNumber: Integer;
begin
ReadInt(F, P, IntNumber);
Number := IntNumber;
end;




procedure ReadWord(F: Text; P: PStream; var Number: Word);
var
  IntNumber: Integer;
begin
ReadInt(F, P, IntNumber);
Number := IntNumber;
end;




procedure ReadByte(F: Text; P: PStream; var Number: Byte);
var
  IntNumber: Integer;
begin
ReadInt(F, P, IntNumber);
Number := IntNumber;
end;




procedure ReadBoolean(F: Text; P: PStream; var Value: Boolean);
var
  IntNumber: Integer;
begin
ReadInt(F, P, IntNumber);
Value := IntNumber <> 0;
end;




procedure ReadReal(F: Text; P: PStream; var Number: Real);
var
  Ch: Char;
  Negative, ExponNegative: Boolean;
  Weight: Real;
  Expon: Integer;
 
begin
Number := 0;
Expon := 0;

// Read sign
Negative := FALSE;
ReadCh(F, P, Ch);
if Ch = '+' then
  ReadCh(F, P, Ch)
else if Ch = '-' then   
  begin
  Negative := TRUE;
  ReadCh(F, P, Ch);
  end;

// Read integer part
while (Ch >= '0') and (Ch <= '9') do
  begin
  Number := Number * 10 + ShortInt(Ch) - ShortInt('0');
  ReadCh(F, P, Ch);
  end;

if Ch = '.' then                     // Fractional part found
  begin
  ReadCh(F, P, Ch);

  // Read fractional part
  Weight := 0.1;
  while (Ch >= '0') and (Ch <= '9') do
    begin
    Number := Number + Weight * (ShortInt(Ch) - ShortInt('0'));
    Weight := Weight / 10;
    ReadCh(F, P, Ch);
    end;
  end;

if (Ch = 'E') or (Ch = 'e') then     // Exponent found
  begin
  // Read exponent sign
  ExponNegative := FALSE;
  ReadCh(F, P, Ch);
  if Ch = '+' then
    ReadCh(F, P, Ch)
  else if Ch = '-' then   
    begin
    ExponNegative := TRUE;
    ReadCh(F, P, Ch);
    end;

  // Read exponent
  while (Ch >= '0') and (Ch <= '9') do
    begin
    Expon := Expon * 10 + ShortInt(Ch) - ShortInt('0');
    ReadCh(F, P, Ch);
    end;

  if ExponNegative then Expon := -Expon;
  end;
     
if Expon <> 0 then Number := Number * exp(Expon * ln(10));
if Negative then Number := -Number;
end;




procedure ReadString(F: Text; P: PStream; const s: string);
var
  i: Integer;
  Ch: Char;
begin
i := 1;
ReadCh(F, P, Ch);

while Ch <> #13 do
  begin
  s[i] := Ch;
  Inc(i);
  ReadCh(F, P, Ch);
  end;

s[i] := #0;
end;




procedure ReadNewLine(F: Text; P: PStream);
var
  Ch: Char;
begin
Ch := LastReadChar;
while not EOF(F) and (Ch <> #13) do ReadCh(F, P, Ch);
LastReadChar := #0;
end;




// String manipulation routines


function Length(const s: string): Integer;
begin
Result := 0;
while s[Result + 1] <> #0 do Inc(Result);
end;




procedure SetLength(var s: string; NewLength: Integer);
begin
if NewLength >= 0 then s[NewLength + 1] := #0;
end;




procedure AppendStr(var Dest: string; const Source: string);
var
  DestLen, i: Integer;
begin
DestLen := Length(Dest);
i := 0;
repeat 
  Inc(i);
  Dest[DestLen + i] := Source[i];
until Source[i] = #0;
end;




procedure ConcatStr(const s1, s2: string; var s: string);
begin
s := s1;
AppendStr(s, s2);
end;




procedure ConcatStrChar(const s1: string; ch: Char; var s: string);
var
  s2: string;
begin
s2[1] := ch;  
s2[2] := #0;
ConcatStr(s1, s2, s);
end;




procedure ConcatCharStr(ch: Char; const s2: string; var s: string);
var
  s1: string;
begin
s1[1] := ch;  
s1[2] := #0;
ConcatStr(s1, s2, s);
end;




procedure ConcatChar(ch1, ch2: Char; var s: string);
var
  s1, s2: string;
begin
s1[1] := ch1;  
s1[2] := #0;
s2[1] := ch2;  
s2[2] := #0;
ConcatStr(s1, s2, s);
end;




function CompareStr(const s1, s2: string): Integer;
var
  i: Integer;
begin
Result := 0;
i := 0;
repeat 
  Inc(i);
  Result := Integer(s1[i]) - Integer(s2[i]);
until (s1[i] = #0) or (s2[i] = #0) or (Result <> 0);
end;




function CompareStrChar(const s: string; ch: Char): Integer;
var
  s2: string;
begin
s2[1] := ch;  
s2[2] := #0;
Result := CompareStr(s, s2);
end;




function CompareCharStr(ch: Char; const s: string): Integer;
var
  s1: string;
begin
s1[1] := ch;  
s1[2] := #0;
Result := CompareStr(s1, s);
end;




procedure Move(const Source; var Dest; Count: Integer);
var
  S, D: ^string;
  i: Integer;
begin
S := @Source;
D := @Dest;

if S = D then Exit;

for i := 1 to Count do
  D^[i] := S^[i];
end;




procedure FillChar(var Data; Count: Integer; Value: Char);
var
  D: ^string;
  i: Integer;
begin
D := @Data;
for i := 1 to Count do
  D^[i] := Value;
end;




function ParseCmdLine(Index: Integer; var Str: string): Integer;
var
  CmdLine: string;
  CmdLinePtr: ^string;
  Param: string;
  ParamPtr: array [0..7] of ^string;
  i, NumParam, CmdLineLen: Integer;

begin
CmdLinePtr := GetCommandLineA;
CmdLine := CmdLinePtr^;
CmdLineLen := Length(CmdLine);

NumParam := 1;
ParamPtr[NumParam - 1] := @CmdLine;

for i := 1 to CmdLineLen do
  begin
  if CmdLine[i] <= ' ' then
    CmdLine[i] := #0;
    
  if (i > 1) and (CmdLine[i] > ' ') and (CmdLine[i - 1] = #0) then
    begin
    Inc(NumParam);
    ParamPtr[NumParam - 1] := @CmdLine[i];
    end;
  end;
  
if Index < NumParam then
  Str := ParamPtr[Index]^
else
  Str := '';

Result := NumParam;  
end;




function ParamCount: Integer;
var
  Str: string;
begin  
Result := ParseCmdLine(0, Str) - 1;
end;   




// Conversion routines


procedure Val(const s: string; var Number: Real; var Code: Integer);
var
  Stream: TStream;
begin
Stream.Data := @s;
Stream.Index := 0;

ReadReal(Text(0), @Stream, Number);

if Stream.Index - 1 <> Length(s) then Code := Stream.Index else Code := 0;
end;




procedure Str(Number: Real; var s: string);
var
  Stream: TStream;
begin
Stream.Data := @s;
Stream.Index := 0;

WriteReal(Text(0), @Stream, Number);
s[Stream.Index + 1] := #0;
end;




procedure IVal(const s: string; var Number: Integer; var Code: Integer);
var
  Stream: TStream;
begin
Stream.Data := @s;
Stream.Index := 0;

ReadInt(Text(0), @Stream, Number);

if Stream.Index - 1 <> Length(s) then Code := Stream.Index else Code := 0;
end;




procedure IStr(Number: Integer; var s: string);
var
  Stream: TStream;
begin
Stream.Data := @s;
Stream.Index := 0;

WriteInt(Text(0), @Stream, Number);
s[Stream.Index + 1] := #0;
end;




function UpCase(ch: Char): Char;
begin
if (ch >= 'a') and (ch <= 'z') then
  Result := Chr(Ord(ch) - Ord('a') + Ord('A'))
else
  Result := ch;
end; 




// Set manipulation routines


procedure InitSet(var SetStorage: TSetStorage);
begin
FillChar(SetStorage, SizeOf(SetStorage), #0);
end;




procedure AddToSet(var SetStorage: TSetStorage; FromVal, ToVal: Integer);
var
  i: Integer;
begin
SetStorage[FromVal] := TRUE;
if ToVal > FromVal then
  for i := FromVal + 1 to ToVal do
    SetStorage[i] := TRUE;
end;




function InSet(Element: Integer; var SetStorage: TSetStorage): Boolean;
var
  i: Integer;
begin
Result := SetStorage[Element];  
end;




procedure SetUnion(const SetStorage1, SetStorage2: TSetStorage; var SetStorage: TSetStorage);
var
  i: Integer;
begin
for i := 0 to SizeOf(SetStorage) - 1 do
  SetStorage[i] := SetStorage1[i] or SetStorage2[i];
end;




procedure SetDifference(const SetStorage1, SetStorage2: TSetStorage; var SetStorage: TSetStorage);
var
  i: Integer;
begin
for i := 0 to SizeOf(SetStorage) - 1 do
  SetStorage[i] := SetStorage1[i] and not SetStorage2[i];
end; 




procedure SetIntersection(const SetStorage1, SetStorage2: TSetStorage; var SetStorage: TSetStorage);
var
  i: Integer;
begin
for i := 0 to SizeOf(SetStorage) - 1 do
  SetStorage[i] := SetStorage1[i] and SetStorage2[i];
end; 




function CompareSets(const SetStorage1, SetStorage2: TSetStorage): Integer;
var
  i: Integer;
begin
Result := 0;
for i := 0 to SizeOf(SetStorage1) - 1 do
  if SetStorage1[i] <> SetStorage2[i] then
    begin
    Result := 1;
    Exit;
    end;
end;  
 
