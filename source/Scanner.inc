// XD Pascal - a 32-bit compiler for Windows
// Copyright (c) 2009-2010, 2019, Vasiliy Tereshkov

// Scanner



var
  Letters, Digits, HexDigits, Spaces, AlphaNums: set of Char;
  EndOfProgram: Boolean;
  ch, ch2: Char;
  
  

  
procedure ReadCharSafe(var ch: Char);
var
  Ptr: PChar;
begin
ch := #0;
if UnitStackTop > 0 then
  if UnitStack[UnitStackTop].Pos < UnitStack[UnitStackTop].Size then
    begin
    Ptr := PChar(Integer(UnitStack[UnitStackTop].Buffer) + UnitStack[UnitStackTop].Pos);
    ch := Ptr^;
    Inc(UnitStack[UnitStackTop].Pos);
    end; 
end;
  



procedure EnterIncludedFile(const Name: TString);
var
  F: TInFile;
  ActualSize: Integer;
  
begin
CustomReset(F, Name);
if IOResult <> 0 then
  Error('Unable to open source file ', Name, -1);

Inc(UnitStackTop);
UnitStack[UnitStackTop].FileName := Name;
UnitStack[UnitStackTop].Size := FileSize(F);
UnitStack[UnitStackTop].Pos := 0;
UnitStack[UnitStackTop].Line := 1;

GetMem(UnitStack[UnitStackTop].Buffer, UnitStack[UnitStackTop].Size);

ActualSize := 0;
BlockRead(F, UnitStack[UnitStackTop].Buffer^, UnitStack[UnitStackTop].Size, ActualSize);
Close(F);

if ActualSize <> UnitStack[UnitStackTop].Size then
  Error('Unable to read source file ', Name, -1);
end;




procedure LeaveIncludedFile(var ch: Char);
begin
FreeMem(UnitStack[UnitStackTop].Buffer, UnitStack[UnitStackTop].Size);
Dec(UnitStackTop);
  
ReadCharSafe(ch);  

if UnitStackTop = 0 then EndOfProgram := TRUE;
end;




procedure InitializeScanner(const Name: TString);
begin
Letters   := ['A'..'Z'];
Digits    := ['0'..'9'];
HexDigits := Digits + ['A'..'F'];
Spaces    := [#1..#31, ' '];
AlphaNums := Letters + Digits + ['_'];

ch  := ' ';
ch2 := ' ';

EndOfProgram := FALSE;

UnitStackTop := 0;
EnterIncludedFile(Name);
end;




procedure FinalizeScanner;
begin
LeaveIncludedFile(ch);
end;




procedure AppendStrSafe(var s: TString; ch: Char);
begin
if Length(s) >= MAXSTRLENGTH - 1 then
  Error('String is too long', '', -1);
s := s + ch;  
end;




procedure ReadChar(var ch: Char);
begin
if EndOfProgram then
  ch := #0
else
  if UnitStack[UnitStackTop].Pos >= UnitStack[UnitStackTop].Size then
    LeaveIncludedFile(ch)
  else
    ReadCharSafe(ch);

if ch = #10 then Inc(UnitStack[UnitStackTop].Line);  // End of line found
end;




procedure ReadValidChar(var ch: Char);
begin
ReadChar(ch);
ch := UpCase(ch);
end;




procedure ReadLiteralChar(var ch: Char);
begin
ReadChar(ch);
if (ch = #0) or (ch = #10) then
  Error('Unterminated string', '', -1);
end;




procedure ReadSingleLineComment;
begin
while (ch <> #10) and not EndOfProgram do
  ReadChar(ch);
end;




procedure ReadMultiLineComment;
begin
while (ch <> '}') and not EndOfProgram do
  ReadChar(ch);
end;




procedure ReadDirective;
var
  Text: TString;
begin
ReadChar(ch);
case UpCase(ch) of
  'I':                               // Include directive found
    begin
    Text := '';
    ReadChar(ch);
    while (ch <> '}') and not EndOfProgram do
      begin
      if (ch = #0) or (ch > ' ') then 
        AppendStrSafe(Text, ch);
      ReadChar(ch);
      end;
      
    EnterIncludedFile(Text);
    end;
    
  'A':                              // Console/GUI application type directive found
    begin
    Text := '';
    ReadChar(ch);
    while (ch <> '}') and not EndOfProgram do
      begin
      if (ch = #0) or (ch > ' ') then 
        AppendStrSafe(Text, UpCase(ch));
      ReadChar(ch);
      end;
      
    if Text = 'CONSOLE' then
      IsConsoleProgram := 1
    else if Text = 'GUI' then
      IsConsoleProgram := 0
    else
      Error('Unknown application type', '', -1);
    end      
      
  else
    Error('Unknown compiler directive', '', -1);
  end; // case  
end;




procedure ReadHexadecimalNumber;
var
  Num: Integer;
  NumFound: Boolean;
begin
Num := 0;

NumFound := FALSE;
while ch in HexDigits do
  begin
  if ch in Digits then
    Num := 16 * Num + Ord(ch) - Ord('0')
  else
    Num := 16 * Num + Ord(ch) - Ord('A') + 10;
  NumFound := TRUE;
  ReadValidChar(ch);
  end;

if not NumFound then
  Error('Hexadecimal constant is not found', '', -1);

Tok.Kind := INTNUMBERTOK;
Tok.Value := Num;
end;




procedure ReadDecimalNumber;
var
  Num, Expon: Integer;
  Frac, FracWeight: Single;
  NegExpon, RangeFound, ExponFound: Boolean;
begin
Num := 0;
Frac := 0;
Expon := 0;
NegExpon := FALSE;

while ch in Digits do
  begin
  Num := 10 * Num + Ord(ch) - Ord('0');
  ReadValidChar(ch);
  end;

if (ch <> '.') and (ch <> 'E') then                                   // Integer number
  begin
  Tok.Kind := INTNUMBERTOK;
  Tok.Value := Num;
  end
else
  begin

  // Check for '..' token
  RangeFound := FALSE;
  if ch = '.' then
    begin
    ReadValidChar(ch2);
    if ch2 = '.' then                                                 // Integer number followed by '..' token
      begin
      Tok.Kind := INTNUMBERTOK;
      Tok.Value := Num;
      RangeFound := TRUE;
      end;
    if not EndOfProgram then Dec(UnitStack[UnitStackTop].Pos);
    end; // if ch = '.'
    
  if not RangeFound then                                              // Fractional number
    begin

    // Check for fractional part
    if ch = '.' then
      begin
      FracWeight := 0.1;
      ReadValidChar(ch);

      while ch in Digits do
        begin
        Frac := Frac + FracWeight * (Ord(ch) - Ord('0'));
        FracWeight := FracWeight / 10;
        ReadValidChar(ch);
        end;
      end; // if ch = '.'

    // Check for exponent
    if ch = 'E' then
      begin
      ReadValidChar(ch);

      // Check for exponent sign
      if ch = '+' then
        ReadValidChar(ch)
      else if ch = '-' then
        begin
        NegExpon := TRUE;
        ReadValidChar(ch);
        end;

      ExponFound := FALSE;
      while ch in Digits do
        begin
        Expon := 10 * Expon + Ord(ch) - Ord('0');
        ReadValidChar(ch);
        ExponFound := TRUE;
        end;

      if not ExponFound then
        Error('Exponent is not found', '', -1);

      if NegExpon then Expon := -Expon;
      end; // if ch = 'E'

    Tok.Kind := FRACNUMBERTOK;
    Tok.FracValue := (Num + Frac) * exp(Expon * ln(10));
    end; // if not RangeFound
  end; // else
end;




procedure ReadNumber;
begin
if ch = '$' then
  begin
  ReadValidChar(ch);
  ReadHexadecimalNumber;
  end
else
  ReadDecimalNumber;
end;    




procedure ReadCharCode;
begin
ReadValidChar(ch);

if not (ch in Digits + ['$']) then
  Error('Character code is not found', '', -1);

ReadNumber;

if Tok.Kind = FRACNUMBERTOK then
  Error('Integer character code expected', '', -1);

Tok.Kind := CHARLITERALTOK;
end;




procedure ReadKeywordOrIdentifier;
var
  Text: TString;
  CurToken: Integer;
begin
Text := '';
repeat
  AppendStrSafe(Text, ch);
  ReadValidChar(ch);
until not (ch in AlphaNums);

CurToken := GetKeyword(Text);
if CurToken <> 0 then               // Keyword found
  Tok.Kind := CurToken
else
  begin                             // Identifier found
  Tok.Kind := IDENTTOK;
  Tok.Name := Text;
  end;
end;




procedure ReadCharOrStringLiteral;
var
  Text: TString;
  EndOfLiteral: Boolean;
begin
Text := '';
EndOfLiteral := FALSE;

repeat
  ReadLiteralChar(ch);
  if ch <> '''' then
    AppendStrSafe(Text, ch)
  else
    begin
    ReadCharSafe(ch2);
    if ch2 = '''' then                                                      // Apostrophe character found
      AppendStrSafe(Text, ch)
    else
      begin
      if not EndOfProgram then Dec(UnitStack[UnitStackTop].Pos);            // Discard ch2
      EndOfLiteral := TRUE;
      end;
    end;
until EndOfLiteral;

if Length(Text) = 1 then
  begin
  Tok.Kind := CHARLITERALTOK;
  Tok.Value := Ord(Text[STRINGFIRSTINDEX]);
  end
else
  begin
  Tok.Kind := STRINGLITERALTOK;
  Tok.Name := Text;
  end;

DefineStaticString(Tok, Text);    // A character literal is also copied to a single-character static string so this string can be passed to subroutines

ReadValidChar(ch);
end;




procedure NextTok;
var
  DivFound: Boolean;
begin
Tok.Kind := 0;

// Skip spaces, comments, directives
DivFound := FALSE;
while (ch in Spaces) or (ch = '{') or ((ch = '/') and not DivFound) do
  begin
  if ch = '{' then                                                      // Multi-line comment or directive
    begin
    ReadValidChar(ch);
    if ch = '$' then ReadDirective else ReadMultiLineComment;
    end
  else if ch = '/' then
    begin
    ReadValidChar(ch2);
    if ch2 = '/' then
      ReadSingleLineComment                                             // Single-line comment
    else
      begin
      if not EndOfProgram then                                          // Discard ch and ch2
        begin
        Dec(UnitStack[UnitStackTop].Pos);
        Dec(UnitStack[UnitStackTop].Pos);
        end;       
      DivFound := TRUE;
      end;
    end;
  ReadValidChar(ch);
  end;

// Read token
case ch of
  '0'..'9', '$':
    ReadNumber;
  '#':
    ReadCharCode;
  'A'..'Z', '_':
    ReadKeywordOrIdentifier;
  '''':
    ReadCharOrStringLiteral;
  ':':                              // Single- or double-character tokens
    begin
    Tok.Kind := COLONTOK;
    ReadValidChar(ch);
    if ch = '=' then
      begin
      Tok.Kind := ASSIGNTOK;
      ReadValidChar(ch);
      end;
    end;
  '>':
    begin
    Tok.Kind := GTTOK;
    ReadValidChar(ch);
    if ch = '=' then
      begin
      Tok.Kind := GETOK;
      ReadValidChar(ch);
      end;
    end;
  '<':
    begin
    Tok.Kind := LTTOK;
    ReadValidChar(ch);
    if ch = '=' then
      begin
      Tok.Kind := LETOK;
      ReadValidChar(ch);
      end
    else if ch = '>' then
      begin
      Tok.Kind := NETOK;
      ReadValidChar(ch);
      end;
    end;
  '.':
    begin
    Tok.Kind := PERIODTOK;
    ReadValidChar(ch);
    if ch = '.' then
      begin
      Tok.Kind := RANGETOK;
      ReadValidChar(ch);
      end;
    end
else                                // Single-character tokens
  case ch of
    '=': Tok.Kind := EQTOK;
    ',': Tok.Kind := COMMATOK;
    ';': Tok.Kind := SEMICOLONTOK;
    '(': Tok.Kind := OPARTOK;
    ')': Tok.Kind := CPARTOK;
    '*': Tok.Kind := MULTOK;
    '/': Tok.Kind := DIVTOK;
    '+': Tok.Kind := PLUSTOK;
    '-': Tok.Kind := MINUSTOK;
    '^': Tok.Kind := DEREFERENCETOK;
    '@': Tok.Kind := ADDRESSTOK;
    '[': Tok.Kind := OBRACKETTOK;
    ']': Tok.Kind := CBRACKETTOK
  else
    Error('Unexpected end of program', '', -1);
  end; // case

  ReadValidChar(ch);
end; // case

end; // NextTok




procedure CheckTok(ExpectedTokKind: Byte);
begin
if Tok.Kind <> ExpectedTokKind then
  Error('Expected ', '', ExpectedTokKind);
end;




procedure EatTok(ExpectedTokKind: Byte);
begin
CheckTok(ExpectedTokKind);
NextTok;
end;


