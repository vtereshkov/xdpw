// XD Pascal - a 32-bit compiler for Windows
// Developed by Vasiliy Tereshkov, 2009-2010, 2019

// Common structures and routines



const
  VERSIONMAJOR          = 0;
  VERSIONMINOR          = 9;
  
  NUMDELIMITERS         = 22;
  NUMKEYWORDS           = 31;

  // Standard token codes

  OPARTOK               = 1;
  CPARTOK               = 2;
  MULTOK                = 3;
  PLUSTOK               = 4;
  COMMATOK              = 5;
  MINUSTOK              = 6;
  PERIODTOK             = 7;
  RANGETOK              = 8;
  DIVTOK                = 9;
  COLONTOK              = 10;
  ASSIGNTOK             = 11;
  SEMICOLONTOK          = 12;
  LTTOK                 = 13;
  LETOK                 = 14;
  NETOK                 = 15;
  EQTOK                 = 16;
  GTTOK                 = 17;
  GETOK                 = 18;
  ADDRESSTOK            = 19;
  OBRACKETTOK           = 20;
  CBRACKETTOK           = 21;
  DEREFERENCETOK        = 22;

  ANDTOK                = 23;
  ARRAYTOK              = 24;
  BEGINTOK              = 25;
  CASETOK               = 26;
  CONSTTOK              = 27;
  IDIVTOK               = 28;
  DOTOK                 = 29;
  DOWNTOTOK             = 30;
  ELSETOK               = 31;
  ENDTOK                = 32;
  FORTOK                = 33;
  FUNCTIONTOK           = 34;
  IFTOK                 = 35;
  MODTOK                = 36;
  NILTOK                = 37;
  NOTTOK                = 38;
  OFTOK                 = 39;
  ORTOK                 = 40;
  PROCEDURETOK          = 41;
  PROGRAMTOK            = 42;
  RECORDTOK             = 43;
  REPEATTOK             = 44;
  SHLTOK                = 45;
  SHRTOK                = 46;
  THENTOK               = 47;
  TOTOK                 = 48;
  TYPETOK               = 49;
  UNTILTOK              = 50;
  VARTOK                = 51;
  WHILETOK              = 52;
  XORTOK                = 53;

  // Non-standard token codes

  IDENTTOK              = 101;
  INTNUMBERTOK          = 102;
  FRACNUMBERTOK         = 103;
  CHARLITERALTOK        = 104;
  STRINGLITERALTOK      = 105;

  // Identifier kind codes

  CONSTANT              = 1;
  USERTYPE              = 2;
  VARIABLE              = 3;
  PROC                  = 4;
  FUNC                  = 5;

  // Type kinds

  ANYTYPE               = 1;
  INTEGERTYPE           = 2;
  SMALLINTTYPE          = 3;
  SHORTINTTYPE          = 4;
  CHARTYPE              = 5;
  BOOLEANTYPE           = 6;
  REALTYPE              = 7;
  POINTERTYPE           = 8;
  TEXTTYPE              = 9;
  ARRAYTYPE             = 10;
  RECORDTYPE            = 11;
  SUBRANGETYPE          = 12;
  FORWARDTYPE           = 101;

  // Type indices

  ANYTYPEINDEX          = 1;      // Untyped parameter, or base type for untyped pointers
  INTEGERTYPEINDEX      = 2;
  SMALLINTTYPEINDEX     = 3;
  SHORTINTTYPEINDEX     = 4;
  CHARTYPEINDEX         = 5;
  BOOLEANTYPEINDEX      = 6;
  REALTYPEINDEX         = 7;
  POINTERTYPEINDEX      = 8;      // Untyped pointer, compatible with any other
  TEXTTYPEINDEX         = 9;      // Universal file type
  STRINGTYPEINDEX       = 10;

  // Predefined routine codes

  INCPROC               = 1;
  DECPROC               = 2;
  READPROC              = 3;
  WRITEPROC             = 4;
  READLNPROC            = 5;
  WRITELNPROC           = 6;
  NEWPROC               = 7;
  DISPOSEPROC           = 8;
  BREAKPROC             = 9;
  CONTINUEPROC          = 10;
  EXITPROC              = 11;
  HALTPROC              = 12;

  SIZEOFFUNC            = 15;
  ORDFUNC               = 16;
  CHRFUNC               = 17;
  PREDFUNC              = 18;
  SUCCFUNC              = 19;
  ROUNDFUNC             = 20;
  TRUNCFUNC             = 21;
  ABSFUNC               = 22;
  SQRFUNC               = 23;
  SINFUNC               = 24;
  COSFUNC               = 25;
  ARCTANFUNC            = 26;
  EXPFUNC               = 27;
  LNFUNC                = 28;
  SQRTFUNC              = 29;

  // Compiler parameters
             
  MAXSTRLENGTH          = 80;
  MAXIDENTS             = 1000;
  MAXTYPES              = 1000;
  MAXBLOCKS             = 1000;
  MAXNESTING            = 10;
  MAXPARAMS             = 20;
  MAXUNITNESTING        = 5;
  MAXFIELDS             = 100;
  MAXRELOCS             = 20000;
  MAXLOOPNESTING        = 20;
  MAXBREAKCALLS         = 100;
  MAXEXITCALLS          = 100;

  MAXCODESIZE           = 1024 * 1024;
  MAXDATASIZE           = 32768 * 1024; 
  MAXSTATICSTRDATASIZE  = 128 * 1024;

  // Scope levels

  GLOBAL                = 1;
  LOCAL                 = 2;

  // Parameter passing

  VALPASSING            = 1;
  CONSTPASSING          = 2;
  VARPASSING            = 3;
  
  // Relocation types
  
  RELOCNONE             = 0;
  RELOCCODE             = 1;
  RELOCDATA             = 2;



type
  TUnit = record
    FileName: TString;
    Pos, Line: Integer;
  end;  
  
  TParam = record
    Name: TString;
    DataType: Integer;
    PassMethod: Byte;
  end;

  PParam = ^TParam;

  TField = record
    Name: TString;
    DataType: Integer;
    Offset: Integer;
  end;

  TType = record
    Block: Integer;
    TypeKind: Byte;
    
    // For subranges
    HostType: Integer;
    Low, High: Integer;    
    
    // For pointers and arrays 
    BaseType, IndexType: Integer;
    IsOpenArray: Boolean;
      
    // For records  
    NumFields: Integer;
    Field: array [1..MAXFIELDS] of ^TField;
    
    // For forward types
    TypeIdentName: TString;   
  end;

  TConst = record
    Kind: Byte;
      
    // For integer numbers
    Value: LongInt;
    
    // For floating-point numbers 
    FracValue: Single;
  end;

  TToken = record
    Kind: Byte;
    Name: TString;
    Value: LongInt;
    FracValue: Single;
    StrAddress: Integer;
    StrLength: Integer;
  end;

  TIdentifier = record
    Kind: Byte;
    Name: TString;
    Value: LongInt;                    // Value for a constant, address for a variable, procedure or function
    FracValue: Single;
    Block: Integer;                    // Index of a block in which the identifier is defined
    NestingLevel: Byte;
    DataType: Integer;
    RecType: Integer;                  // Parent record type code for a field
    Scope: Byte;
    PassMethod: Byte;                  // Value, CONST or VAR parameter status
    NumParams: Integer;
    Param: array [1..MAXPARAMS] of PParam;
    ProcAsBlock: Integer;
    PredefIndex: Byte;
    IsUnresolvedForward: Boolean;
    IsExternal: Boolean;
  end;

  TRelocatable = record
    RelocType: Byte;
    Pos: LongInt;
    Value: LongInt;
  end;
  
  TBreakContinueExitCallList = record
    NumCalls: Integer;
    Pos: array [1..MAXBREAKCALLS] of LongInt;
  end;  
  


var
  Keyword: array [1..NUMKEYWORDS] of TKeyName;
  Ident: array [1..MAXIDENTS] of TIdentifier;
  Types: array [1..MAXTYPES] of TType;
  UnitStack: array [1..MAXUNITNESTING] of TUnit;
  Code: array [0..MAXCODESIZE - 1] of Byte;
  StaticStringData: array [0..MAXSTATICSTRDATASIZE - 1] of Char;
  CodePosStack: array [0..1023] of Integer;
  BlockStack: array [1..MAXNESTING] of Integer;
  Reloc: array [1..MAXRELOCS] of TRelocatable;
  BreakCall, ContinueCall: array [1..MAXLOOPNESTING] of TBreakContinueExitCallList;
  ExitCall: TBreakContinueExitCallList;

  Tok: TToken;

  NumIdent, NumTypes, NumStaticStrChars, NumBlocks, BlockStackTop, NumRelocs, ForLoopNesting,
  CodeSize, CodePosStackTop, GlobalDataSize,
  DataSectionOrigin, ProgramEntryPoint,
  UnitStackTop, Line: Integer;
  
  IsConsoleProgram: Integer;                                              // More convenient than Boolean
  
  InFile: TInFile;
  

  
procedure FillKeywords;
begin
Keyword[1]  := 'AND';
Keyword[2]  := 'ARRAY';
Keyword[3]  := 'BEGIN';
Keyword[4]  := 'CASE';
Keyword[5]  := 'CONST';
Keyword[6]  := 'DIV';
Keyword[7]  := 'DO';
Keyword[8]  := 'DOWNTO';
Keyword[9]  := 'ELSE';
Keyword[10] := 'END';
Keyword[11] := 'FOR';
Keyword[12] := 'FUNCTION';
Keyword[13] := 'IF';
Keyword[14] := 'MOD';
Keyword[15] := 'NIL';
Keyword[16] := 'NOT';
Keyword[17] := 'OF';
Keyword[18] := 'OR';
Keyword[19] := 'PROCEDURE';
Keyword[20] := 'PROGRAM';
Keyword[21] := 'RECORD';
Keyword[22] := 'REPEAT';
Keyword[23] := 'SHL';
Keyword[24] := 'SHR';
Keyword[25] := 'THEN';
Keyword[26] := 'TO';
Keyword[27] := 'TYPE';
Keyword[28] := 'UNTIL';
Keyword[29] := 'VAR';
Keyword[30] := 'WHILE';
Keyword[31] := 'XOR';
end;  




procedure DisposeAll;
var
  i, j: Integer;
begin
// Dispose dynamically allocated parameter data
for i := 1 to NumIdent do
  if (Ident[i].Kind = PROC) or (Ident[i].Kind = FUNC) then
    for j := 1 to Ident[i].NumParams do
      Dispose(Ident[i].Param[j]);

// Dispose dynamically allocated field data
for i := 1 to NumTypes do
  if Types[i].TypeKind = RECORDTYPE then
    for j := 1 to Types[i].NumFields do
      Dispose(Types[i].Field[j]);
end;





procedure WriteToken(TokKind: Integer);
begin
if TokKind = 0 then
  Write('no token')
else if TokKind <= NUMDELIMITERS then
  case TokKind of
    OPARTOK:        Write('(');
    CPARTOK:        Write(')');
    MULTOK:         Write('*');
    PLUSTOK:        Write('+');
    COMMATOK:       Write(',');
    MINUSTOK:       Write('-');
    PERIODTOK:      Write('.');
    RANGETOK:       Write('..');
    DIVTOK:         Write('/');
    COLONTOK:       Write(':');
    ASSIGNTOK:      Write(':=');
    SEMICOLONTOK:   Write(';');
    LTTOK:          Write('<');
    LETOK:          Write('<=');
    NETOK:          Write('<>');
    EQTOK:          Write('=');
    GTTOK:          Write('>');
    GETOK:          Write('>=');
    ADDRESSTOK:     Write('@');
    OBRACKETTOK:    Write('[');
    CBRACKETTOK:    Write(']');
    DEREFERENCETOK: Write('^');
  end // case
else if TokKind <= NUMDELIMITERS + NUMKEYWORDS then
  Write(Keyword[TokKind - NUMDELIMITERS])
else if TokKind = IDENTTOK then
  Write('identifier')
else if (TokKind = INTNUMBERTOK) or (TokKind = FRACNUMBERTOK) then
  Write('number')
else if (TokKind = CHARLITERALTOK) or (TokKind = STRINGLITERALTOK) then
  Write('literal')
else
  Write('unknown token');
end;




  
procedure Error(const Msg, Name: TString; TokKind: Integer);
begin
Write('Error ', UnitStack[UnitStackTop].FileName, ' ', Line, ': ', Msg, Name);
if TokKind > -1 then WriteToken(TokKind);
WriteLn;

DisposeAll;
Close(InFile);
Halt(1);
end;





function GetKeyword(const S: TKeyName): Integer;
var
  Max, Mid, Min: Integer;
  Found: Boolean;
begin
Result := 0;

// Binary search
Min := 1;
Max := NUMKEYWORDS;

repeat
  Mid := (Min + Max) div 2;
  if CompareStr(S, Keyword[Mid]) > 0 then
    Min := Mid + 1
  else
    Max := Mid - 1;
  Found := CompareStr(S, Keyword[Mid]) = 0;
until Found or (Min > Max);

if Found then Result := NUMDELIMITERS + Mid;
end;





function GetIdentUnsafe(const S: TString): Integer;
var
  IdentIndex, BlockStackIndex: Integer;
begin
for BlockStackIndex := BlockStackTop downto 1 do
  for IdentIndex := NumIdent downto 1 do
    if (CompareStr(Ident[IdentIndex].Name, S) = 0) and (Ident[IdentIndex].Block = BlockStack[BlockStackIndex]) then 
      begin
      Result := IdentIndex;
      Exit;
      end;
      
Result := 0;
end;




function GetIdent(const S: TString): Integer;
begin
Result := GetIdentUnsafe(S);
if Result = 0 then
  Error('Unknown identifier: ', S, -1);
end;




function GetField(RecType: Integer; const S: TString): Integer;
var
  FieldIndex: Integer;
begin
for FieldIndex := 1 to Types[RecType].NumFields do
  if CompareStr(Types[RecType].Field[FieldIndex]^.Name, S) = 0 then
    begin
    Result := FieldIndex;
    Exit;
    end;

Error('Unknown field: ', S, -1);
end;      




function IsMultiplicativeOperator(const Tok: TToken): Boolean;
begin
Result := (Tok.Kind = MULTOK) or 
          (Tok.Kind = DIVTOK) or (Tok.Kind = IDIVTOK) or (Tok.Kind = MODTOK) or 
          (Tok.Kind = SHLTOK) or (Tok.Kind = SHRTOK) or
          (Tok.Kind = ANDTOK);
end;



          

function IsAdditiveOperator(const Tok: TToken): Boolean;
begin
Result := (Tok.Kind = PLUSTOK) or (Tok.Kind = MINUSTOK) or
          (Tok.Kind = ORTOK) or (Tok.Kind = XORTOK);
end;





function IsUnaryOperator(const Tok: TToken): Boolean;
begin
Result := (Tok.Kind = PLUSTOK) or (Tok.Kind = MINUSTOK);
end;  





function IsRelationOperator(const Tok: TToken): Boolean;
begin
Result := (Tok.Kind = EQTOK) or (Tok.Kind = NETOK) or 
          (Tok.Kind = LTTOK) or (Tok.Kind = LETOK) or 
          (Tok.Kind = GTTOK) or (Tok.Kind = GETOK);
end;





function IsOperatorForIntegers(const Tok: TToken): Boolean;
begin
Result := (IsMultiplicativeOperator(Tok) and (Tok.Kind <> DIVTOK)) or
           IsAdditiveOperator(Tok) or
           IsRelationOperator(Tok) or
           (Tok.Kind = NOTTOK);
end;




           
function IsOperatorForReals(const Tok: TToken): Boolean;
begin
Result := (Tok.Kind = MULTOK) or (Tok.Kind = DIVTOK) or
          (Tok.Kind = PLUSTOK) or (Tok.Kind = MINUSTOK) or
          IsRelationOperator(Tok);
end;





function IsOperatorForBooleans(const Tok: TToken): Boolean;
begin
Result := (Tok.Kind = ANDTOK) or (Tok.Kind = ORTOK) or (Tok.Kind = XORTOK) or (Tok.Kind = NOTTOK) or
          IsRelationOperator(Tok);
end;




function IsIntegerType(const T: TType): Boolean;
begin
Result := (T.TypeKind = INTEGERTYPE) or (T.TypeKind = SMALLINTTYPE) or (T.TypeKind = SHORTINTTYPE);
end;





function IsOrdinalType(const T: TType): Boolean;
begin
Result := IsIntegerType(T) or (T.TypeKind = CHARTYPE) or (T.TypeKind = BOOLEANTYPE) or (T.TypeKind = SUBRANGETYPE);
end;





function IsNumericType(const T: TType): Boolean;
begin
Result := IsIntegerType(T) or (T.TypeKind = REALTYPE);
end;





function IsStructuredType(const T: TType): Boolean;
begin
Result := (T.TypeKind = ARRAYTYPE) or (T.TypeKind = RECORDTYPE);
end;





function IsCastableType(const T: TType): Boolean;
begin
Result := IsOrdinalType(T) or (T.TypeKind = TEXTTYPE) or (T.TypeKind = POINTERTYPE);
end;





procedure DefineStaticString(var Tok: TToken; const StrValue: TString);
var
  i: Integer;
begin
Tok.StrAddress := DataSectionOrigin + NumStaticStrChars;
Tok.StrLength := Length(StrValue);

for i := 0 to Length(StrValue) - 1 do
  begin
  StaticStringData[NumStaticStrChars] := StrValue[i + STRINGFIRSTINDEX];
  Inc(NumStaticStrChars);
  if NumStaticStrChars > MAXSTATICSTRDATASIZE - 1 then
    Error('Maximum string data size exceeded', '', -1);
  end;

// Add string termination character
StaticStringData[NumStaticStrChars] := #0;
Inc(NumStaticStrChars);
end;





function LowBound(DataType: Integer): Integer;
begin
Result := 0;
case Types[DataType].TypeKind of
  INTEGERTYPE:  Result := -2147483648;
  SMALLINTTYPE: Result := -32768;
  SHORTINTTYPE: Result := -128;
  CHARTYPE:     Result :=  0;
  BOOLEANTYPE:  Result := -1;
  SUBRANGETYPE: Result :=  Types[DataType].Low
else
  Error('Ordinal type expected', '', -1)
end;// case
end;

                        



function HighBound(DataType: Integer): Integer;
begin
Result := 0;
case Types[DataType].TypeKind of
  INTEGERTYPE:  Result := 2147483647;
  SMALLINTTYPE: Result := 32767;
  SHORTINTTYPE: Result := 127;
  CHARTYPE:     Result := 255;
  BOOLEANTYPE:  Result := 0;
  SUBRANGETYPE: Result := Types[DataType].High
else
  Error('Ordinal type expected', '', -1)
end;// case
end;





function TypeSize(DataType: Integer): Integer;
var
  i: Integer;
begin
Result := 0;
case Types[DataType].TypeKind of
  INTEGERTYPE:  Result := SizeOf(Integer);
  SMALLINTTYPE: Result := SizeOf(SmallInt);
  SHORTINTTYPE: Result := SizeOf(ShortInt);
  CHARTYPE:     Result := SizeOf(Char);
  BOOLEANTYPE:  Result := SizeOf(Boolean);
  REALTYPE:     Result := SizeOf(Single);
  POINTERTYPE:  Result := SizeOf(Pointer);
  TEXTTYPE:     Result := SizeOf(Integer);
  SUBRANGETYPE: Result := SizeOf(Integer);
  ARRAYTYPE:    if Types[DataType].IsOpenArray then
                  Error('Illegal type', '', -1)
                else  
                  Result := (HighBound(Types[DataType].IndexType) - LowBound(Types[DataType].IndexType) + 1) * TypeSize(Types[DataType].BaseType);
  RECORDTYPE:   begin
                Result := 0;
                for i := 1 to Types[DataType].NumFields do
                  Result := Result + TypeSize(Types[DataType].Field[i]^.DataType);
                end               
else
  Error('Illegal type', '', -1)
end;// case
end;    




function GetCompatibleType(LeftType, RightType: Integer): Integer;
begin
Result := 0;

if LeftType = RightType then                 // General rule
  Result := LeftType
else                                         // Special cases
  begin
  // Untyped pointers are compatible with any other pointers
  if (Types[LeftType].TypeKind = POINTERTYPE) and (Types[RightType].TypeKind = POINTERTYPE) and
     ((Types[LeftType].BaseType = ANYTYPE) or (Types[RightType].BaseType = ANYTYPE)) then
    Result := LeftType;

  // Subranges are compatible with their host types
  if Types[LeftType].TypeKind = SUBRANGETYPE then
    Result := GetCompatibleType(Types[LeftType].HostType, RightType);
  if Types[RightType].TypeKind = SUBRANGETYPE then
    Result := GetCompatibleType(LeftType, Types[RightType].HostType);

  // Integers
  if IsIntegerType(Types[LeftType]) and IsIntegerType(Types[RightType]) then
    Result := LeftType;

  // Booleans
  if (Types[LeftType].TypeKind = BOOLEANTYPE) and
     (Types[RightType].TypeKind = BOOLEANTYPE) then
    Result := LeftType;

  // Characters
  if (Types[LeftType].TypeKind = CHARTYPE) and
     (Types[RightType].TypeKind = CHARTYPE) then
    Result := LeftType;
  end;// if

if Result = 0 then
  Error('Incompatible types', '', -1);  
end;




function ConversionIsPossible(SrcType, DestType: Integer): Boolean;
begin
// Implicit type conversion is possible if DestType is real and SrcType is integer or a subrange of integer
Result := (Types[DestType].TypeKind = REALTYPE) and
          (IsIntegerType(Types[SrcType]) or
           ((Types[SrcType].TypeKind = SUBRANGETYPE) and IsIntegerType(Types[Types[SrcType].HostType])));
end;





procedure AssertIdent;
begin
if Tok.Kind <> IDENTTOK then
  Error('Expected identifier but found ', '', Tok.Kind);
end;




procedure CheckOperator(const Tok: TToken; DataType: Integer); 
begin
if Types[DataType].TypeKind = SUBRANGETYPE then
  CheckOperator(Tok, Types[DataType].HostType)
else 
  begin
  if not IsOrdinalType(Types[DataType]) and (Types[DataType].TypeKind <> REALTYPE) and (Types[DataType].TypeKind <> POINTERTYPE) then
    Error('Operator is not applicable', '', -1);
   
  if (IsIntegerType(Types[DataType]) and not IsOperatorForIntegers(Tok)) or
    ((Types[DataType].TypeKind = REALTYPE) and not IsOperatorForReals(Tok)) or   
    ((Types[DataType].TypeKind = CHARTYPE) and not IsRelationOperator(Tok)) or
    ((Types[DataType].TypeKind = BOOLEANTYPE) and not IsOperatorForBooleans(Tok)) or
    ((Types[DataType].TypeKind = POINTERTYPE) and not IsRelationOperator(Tok)) 
  then
    Error('Operator is not applicable', '', -1);
  end;  
end;  



