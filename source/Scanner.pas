// XD Pascal - a 32-bit compiler for Windows
// Copyright (c) 2009-2010, 2019-2020, Vasiliy Tereshkov

{$I-}
{$H-}
{$J+}

unit Scanner;


interface


uses Common;


var
  Tok: TToken;


procedure InitializeScanner(const Name: TString);
function SaveScanner: Boolean;
function RestoreScanner: Boolean;
procedure FinalizeScanner;
procedure NextTok;
procedure CheckTok(ExpectedTokKind: TTokenKind);
procedure EatTok(ExpectedTokKind: TTokenKind);
procedure AssertIdent;
function ScannerFileName: TString;
function ScannerLine: Integer;



implementation



type
  TBuffer = record    
    Ptr: PCharacter;
    Size, Pos: Integer;
  end;  
  

  TScannerState = record
    Token: TToken;
    FileName: TString;
    Line: Integer;
    Buffer: TBuffer;
    ch, ch2: TCharacter;
    EndOfUnit: Boolean;    
  end;


const
  SCANNERSTACKSIZE = 10;


var
  ScannerState: TScannerState;
  ScannerStack: array [1..SCANNERSTACKSIZE] of TScannerState;
  
    
const
  ScannerStackTop: Integer = 0;
 
  Digits:    set of TCharacter = ['0'..'9'];
  HexDigits: set of TCharacter = ['0'..'9', 'A'..'F'];
  Spaces:    set of TCharacter = [#1..#31, ' '];
  AlphaNums: set of TCharacter = ['A'..'Z', '0'..'9', '_'];
  
  

  
procedure InitializeScanner(const Name: TString);
var
  F: TInFile;
  ActualSize: Integer;
  
begin
ScannerState.Buffer.Ptr := nil;

// First search the source folder, then the units folder
Assign(F, TGenericString(SourceFolder + Name));
Reset(F, 1);
if IOResult <> 0 then
  begin
  Assign(F, TGenericString(UnitsFolder + Name));
  Reset(F, 1);  
  if IOResult <> 0 then
    Error('Unable to open source file ' + Name);
  end;  

with ScannerState do
  begin
  FileName := Name;
  Line := 1;
  
  with Buffer do
    begin
    Size := FileSize(F);
    Pos := 0;
    
    GetMem(Ptr, Size);
    
    ActualSize := 0;
    BlockRead(F, Ptr^, Size, ActualSize);
    Close(F);

    if ActualSize <> Size then
      Error('Unable to read source file ' + Name);
    end;  

  ch  := ' ';
  ch2 := ' ';
  EndOfUnit := FALSE;
  end;  
end;




function SaveScanner: Boolean;
begin
Result := FALSE;
if ScannerStackTop < SCANNERSTACKSIZE then
  begin
  Inc(ScannerStackTop);
  ScannerStack[ScannerStackTop] := ScannerState;
  Result := TRUE;
  end; 
end;




function RestoreScanner: Boolean;
begin
Result := FALSE;
if ScannerStackTop > 0 then
  begin  
  ScannerState := ScannerStack[ScannerStackTop];
  Dec(ScannerStackTop);
  Tok := ScannerState.Token;
  Result := TRUE;
  end;
end;




procedure FinalizeScanner;
begin
ScannerState.EndOfUnit := TRUE;
with ScannerState.Buffer do
  if Ptr <> nil then
    begin
    FreeMem(Ptr, Size);
    Ptr := nil;
    end;
end;




procedure AppendStrSafe(var s: TString; ch: TCharacter);
begin
if Length(s) >= MAXSTRLENGTH - 1 then
  Error('String is too long');
s := s + ch;  
end;




procedure ReadChar(var ch: TCharacter);
begin
ch := #0;
with ScannerState.Buffer do
  if Pos < Size then
    begin
    ch := PCharacter(Integer(Ptr) + Pos)^;
    Inc(Pos);
    end
  else
    ScannerState.EndOfUnit := TRUE; 

if ch = #10 then Inc(ScannerState.Line);  // End of line found
end;




procedure ReadValidChar(var ch: TCharacter);
begin
ReadChar(ch);
ch := UpCase(ch);
end;




procedure ReadLiteralChar(var ch: TCharacter);
begin
ReadChar(ch);
if (ch = #0) or (ch = #10) then
  Error('Unterminated string');
end;




procedure ReadSingleLineComment;
begin
with ScannerState do
  while (ch <> #10) and not EndOfUnit do
    ReadChar(ch);
end;




procedure ReadMultiLineComment;
begin
with ScannerState do
  while (ch <> '}') and not EndOfUnit do
    ReadChar(ch);
end;




procedure ReadDirective;
var
  Text: TString;
begin
with ScannerState do
  begin
  Text := '';
  repeat
    AppendStrSafe(Text, ch);
    ReadValidChar(ch);
  until not (ch in AlphaNums);

  if Text = '$I' then
    begin
    if (ch = '+') or (ch = '-') then   // I/O checking directive - ignored
      ReadMultiLineComment
    else
      Error('Unknown compiler directive');
    end
    
  else if Text = '$APPTYPE' then       // Console/GUI application type directive
    begin
    Text := '';
    ReadChar(ch);
    while (ch <> '}') and not EndOfUnit do
      begin
      if (ch = #0) or (ch > ' ') then 
        AppendStrSafe(Text, UpCase(ch));
      ReadChar(ch);
      end;
      
    if Text = 'CONSOLE' then
      IsConsoleProgram := TRUE
    else if Text = 'GUI' then
      IsConsoleProgram := FALSE
    else
      Error('Unknown application type ' + Text);
    end
        
  else
    ReadMultiLineComment;
  end;  
end;




procedure ReadHexadecimalNumber;
var
  Num: Integer;
  NumFound: Boolean;
begin
with ScannerState do
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
    Error('Hexadecimal constant is not found');

  Token.Kind := INTNUMBERTOK;
  Token.Value := Num;
  end;
end;




procedure ReadDecimalNumber;
var
  Num, Expon: Integer;
  Frac, FracWeight: Single;
  NegExpon, RangeFound, ExponFound: Boolean;
begin
with ScannerState do
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
    Token.Kind := INTNUMBERTOK;
    Token.Value := Num;
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
        Token.Kind := INTNUMBERTOK;
        Token.Value := Num;
        RangeFound := TRUE;
        end;
      if not EndOfUnit then Dec(Buffer.Pos);
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
          Error('Exponent is not found');

        if NegExpon then Expon := -Expon;
        end; // if ch = 'E'

      Token.Kind := FRACNUMBERTOK;
      Token.FracValue := (Num + Frac) * exp(Expon * ln(10));
      end; // if not RangeFound
    end; // else
  end;  
end;




procedure ReadNumber;
begin
with ScannerState do
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
with ScannerState do
  begin
  ReadValidChar(ch);

  if not (ch in Digits + ['$']) then
    Error('Character code is not found');

  ReadNumber;

  if Token.Kind = FRACNUMBERTOK then
    Error('Integer character code expected');

  Token.Kind := CHARLITERALTOK;
  end;
end;




procedure ReadKeywordOrIdentifier;
var
  Text: TString;
  CurToken: TTokenKind;
begin
with ScannerState do
  begin
  Text := '';
  repeat
    AppendStrSafe(Text, ch);
    ReadValidChar(ch);
  until not (ch in AlphaNums);

  CurToken := GetKeyword(Text);
  if CurToken <> EMPTYTOK then        // Keyword found
    Token.Kind := CurToken
  else
    begin                             // Identifier found
    Token.Kind := IDENTTOK;
    Token.Name := Text;
    end;
  end;  
end;




procedure ReadCharOrStringLiteral;
var
  Text: TString;
  EndOfLiteral: Boolean;
begin
with ScannerState do
  begin
  Text := '';
  EndOfLiteral := FALSE;

  repeat
    ReadLiteralChar(ch);
    if ch <> '''' then
      AppendStrSafe(Text, ch)
    else
      begin
      ReadChar(ch2);
      if ch2 = '''' then                                                   // Apostrophe character found
        AppendStrSafe(Text, ch)
      else
        begin
        if not EndOfUnit then Dec(Buffer.Pos);                             // Discard ch2
        EndOfLiteral := TRUE;
        end;
      end;
  until EndOfLiteral;

  if Length(Text) = 1 then
    begin
    Token.Kind := CHARLITERALTOK;
    Token.Value := Ord(Text[1]);
    end
  else
    begin
    Token.Kind := STRINGLITERALTOK;
    Token.Name := Text;
    DefineStaticString(Token, Text);
    end;

  ReadValidChar(ch);
  end;
end;




procedure NextTok;
begin
with ScannerState do
  begin
  Token.Kind := EMPTYTOK;

  // Skip spaces, comments, directives
  while (ch in Spaces) or (ch = '{') or (ch = '/') do
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
        if not EndOfUnit then Dec(Buffer.Pos);                            // Discard ch2     
        Break;
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
      Token.Kind := COLONTOK;
      ReadValidChar(ch);
      if ch = '=' then
        begin
        Token.Kind := ASSIGNTOK;
        ReadValidChar(ch);
        end;
      end;
    '>':
      begin
      Token.Kind := GTTOK;
      ReadValidChar(ch);
      if ch = '=' then
        begin
        Token.Kind := GETOK;
        ReadValidChar(ch);
        end;
      end;
    '<':
      begin
      Token.Kind := LTTOK;
      ReadValidChar(ch);
      if ch = '=' then
        begin
        Token.Kind := LETOK;
        ReadValidChar(ch);
        end
      else if ch = '>' then
        begin
        Token.Kind := NETOK;
        ReadValidChar(ch);
        end;
      end;
    '.':
      begin
      Token.Kind := PERIODTOK;
      ReadValidChar(ch);
      if ch = '.' then
        begin
        Token.Kind := RANGETOK;
        ReadValidChar(ch);
        end;
      end
  else                                // Single-character tokens
    case ch of
      '=': Token.Kind := EQTOK;
      ',': Token.Kind := COMMATOK;
      ';': Token.Kind := SEMICOLONTOK;
      '(': Token.Kind := OPARTOK;
      ')': Token.Kind := CPARTOK;
      '*': Token.Kind := MULTOK;
      '/': Token.Kind := DIVTOK;
      '+': Token.Kind := PLUSTOK;
      '-': Token.Kind := MINUSTOK;
      '^': Token.Kind := DEREFERENCETOK;
      '@': Token.Kind := ADDRESSTOK;
      '[': Token.Kind := OBRACKETTOK;
      ']': Token.Kind := CBRACKETTOK
    else
      Error('Unexpected end of program');
    end; // case

    ReadValidChar(ch);
  end; // case
  end;
  
Tok := ScannerState.Token;  
end; // NextTok




procedure CheckTok(ExpectedTokKind: TTokenKind);
begin
with ScannerState do
  if Token.Kind <> ExpectedTokKind then
    Error(GetTokSpelling(ExpectedTokKind) + ' expected but ' + GetTokSpelling(Token.Kind) + ' found');
end;




procedure EatTok(ExpectedTokKind: TTokenKind);
begin
CheckTok(ExpectedTokKind);
NextTok;
end;




procedure AssertIdent;
begin
with ScannerState do
  if Token.Kind <> IDENTTOK then
    Error('Identifier expected but ' + GetTokSpelling(Token.Kind) + ' found');
end;




function ScannerFileName: TString;
begin
Result := ScannerState.FileName;
end;




function ScannerLine: Integer;
begin
Result := ScannerState.Line;
end;

end.