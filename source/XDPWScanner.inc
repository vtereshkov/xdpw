// XD Pascal - a 32-bit compiler for Windows
// Developed by Vasiliy Tereshkov, 2009-2010, 2019

// Scanner



var
  ProgramName: TString;
  EndOfProgram: Boolean;
  ch, ch2: Char;



procedure InitializeScanner;
begin
EndOfProgram := FALSE;
UnitStackTop := 1;
UnitStack[UnitStackTop].FileName := ProgramName;
CustomReset(InFile, ProgramName);

if IOResult <> 0 then
  Error('Unable to open source file ', ProgramName, -1);

Line := 1;

ch  := ' ';
ch2 := ' ';
end;




procedure FinalizeScanner;
var
  DummyIOResult: Integer;
begin
Close(InFile);
DummyIOResult := IOResult;
end;




procedure EnterIncludedFile(const Name: TString);
begin
UnitStack[UnitStackTop].Pos := FilePos(InFile);
UnitStack[UnitStackTop].Line := Line;

Close(InFile);
CustomReset(InFile, Name);

if IOResult <> 0 then
  Error('Unable to open source file ', Name, -1);

Inc(UnitStackTop);
UnitStack[UnitStackTop].FileName := Name;

Line := 1;
end;




procedure LeaveIncludedFile(var ch: Char);
begin
if UnitStackTop > 1 then 
  begin
  Dec(UnitStackTop);
  CustomReset(InFile, UnitStack[UnitStackTop].FileName);
  Seek(InFile, UnitStack[UnitStackTop].Pos);
  Line := UnitStack[UnitStackTop].Line;
  CustomReadChar(InFile, ch);
  end
else
  begin
  EndOfProgram := TRUE;
  ch := #0;
  end;
end;




procedure SafeStrAppend(var s: TString; ch: Char);
begin
if Length(s) >= MAXSTRLENGTH - 1 then
  Error('String is too long', '', -1);
CustomStrAppend(s, ch);  
end;




procedure ReadChar(var ch: Char);
begin
if EndOfProgram then
  ch := #0
else
  if EOF(InFile) then
    begin
    Close(InFile);
    LeaveIncludedFile(ch);
    end
  else
    CustomReadChar(InFile, ch);

if ch = #10 then Inc(Line);                            // End of line found
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
        SafeStrAppend(Text, ch);
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
        SafeStrAppend(Text, UpCase(ch));
      ReadChar(ch);
      end;
      
    if CustomStrComp(Text, 'CONSOLE') = 0 then
      IsConsoleProgram := 1
    else if CustomStrComp(Text, 'GUI') = 0 then
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
while ((ch >= '0') and (ch <= '9')) or ((ch >= 'A') and (ch <= 'F')) do
  begin
  if (ch  >= '0') and (ch <= '9') then
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

while (ch >= '0') and (ch <= '9') do
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
    if not EndOfProgram then Seek(InFile, FilePos(InFile) - 1);
    end; // if ch = '.'
    
  if not RangeFound then                                              // Fractional number
    begin

    // Check for fractional part
    if ch = '.' then
      begin
      FracWeight := 0.1;
      ReadValidChar(ch);

      while (ch >= '0') and (ch <= '9') do
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
      while (ch >= '0') and (ch <= '9') do
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

if not (((ch >= '0') and (ch <= '9')) or (ch = '$')) then
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
  SafeStrAppend(Text, ch);
  ReadValidChar(ch);
until not (((ch >= 'A') and (ch <= 'Z')) or (ch = '_') or ((ch >= '0') and (ch <= '9')));

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
    SafeStrAppend(Text, ch)
  else
    begin
    CustomReadChar(InFile, ch2);
    if ch2 = '''' then                                                      // Apostrophe character found
      SafeStrAppend(Text, ch)
    else
      begin
      if not EndOfProgram then Seek(InFile, FilePos(InFile) - 1);           // Discard ch2
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
while ((ch >= #1) and (ch <= #31)) or (ch = ' ') or (ch = '{') or ((ch = '/') and not DivFound) do
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
      if not EndOfProgram then Seek(InFile, FilePos(InFile) - 2);       // Discard ch and ch2
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




