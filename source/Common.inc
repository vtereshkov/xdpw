// XD Pascal - a 32-bit compiler for Windows
// Copyright (c) 2009-2010, 2019, Vasiliy Tereshkov

// Common structures and routines



const
  VERSIONMAJOR          = 0;
  VERSIONMINOR          = 9;
  
  NUMDELIMITERS         = 22;
  NUMKEYWORDS           = 33;

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

  ANDTOK                = NUMDELIMITERS +  1;
  ARRAYTOK              = NUMDELIMITERS +  2;
  BEGINTOK              = NUMDELIMITERS +  3;
  CASETOK               = NUMDELIMITERS +  4;
  CONSTTOK              = NUMDELIMITERS +  5;
  IDIVTOK               = NUMDELIMITERS +  6;
  DOTOK                 = NUMDELIMITERS +  7;
  DOWNTOTOK             = NUMDELIMITERS +  8;
  ELSETOK               = NUMDELIMITERS +  9;
  ENDTOK                = NUMDELIMITERS + 10;
  FILETOK               = NUMDELIMITERS + 11;
  FORTOK                = NUMDELIMITERS + 12;
  FUNCTIONTOK           = NUMDELIMITERS + 13;
  IFTOK                 = NUMDELIMITERS + 14;
  MODTOK                = NUMDELIMITERS + 15;
  NILTOK                = NUMDELIMITERS + 16;
  NOTTOK                = NUMDELIMITERS + 17;
  OFTOK                 = NUMDELIMITERS + 18;
  ORTOK                 = NUMDELIMITERS + 19;
  PROCEDURETOK          = NUMDELIMITERS + 20;
  PROGRAMTOK            = NUMDELIMITERS + 21;
  RECORDTOK             = NUMDELIMITERS + 22;
  REPEATTOK             = NUMDELIMITERS + 23;
  SHLTOK                = NUMDELIMITERS + 24;
  SHRTOK                = NUMDELIMITERS + 25;
  STRINGTOK             = NUMDELIMITERS + 26;
  THENTOK               = NUMDELIMITERS + 27;
  TOTOK                 = NUMDELIMITERS + 28;
  TYPETOK               = NUMDELIMITERS + 29;
  UNTILTOK              = NUMDELIMITERS + 30;
  VARTOK                = NUMDELIMITERS + 31;
  WHILETOK              = NUMDELIMITERS + 32;
  XORTOK                = NUMDELIMITERS + 33;

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
  FILETYPE              = 9;
  ARRAYTYPE             = 10;
  RECORDTYPE            = 11;
  SUBRANGETYPE          = 12;
  PROCEDURALTYPE        = 13;
  FORWARDTYPE           = 101;

  // Predefined type indices

  ANYTYPEINDEX          = 1;      // Untyped parameter, or base type for untyped pointers
  INTEGERTYPEINDEX      = 2;
  SMALLINTTYPEINDEX     = 3;
  SHORTINTTYPEINDEX     = 4;
  CHARTYPEINDEX         = 5;
  BOOLEANTYPEINDEX      = 6;
  REALTYPEINDEX         = 7;
  POINTERTYPEINDEX      = 8;      // Untyped pointer, compatible with any other
  FILETYPEINDEX         = 9;      // Universal file type
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
  
  PParams = array [1..MAXPARAMS] of PParam;

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
    
    // For procedural types
    NumParams: Integer;
    Param: PParams;
    ResultType: Integer;
    
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
    Param: PParams;
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
Keyword[11] := 'FILE';
Keyword[12] := 'FOR';
Keyword[13] := 'FUNCTION';
Keyword[14] := 'IF';
Keyword[15] := 'MOD';
Keyword[16] := 'NIL';
Keyword[17] := 'NOT';
Keyword[18] := 'OF';
Keyword[19] := 'OR';
Keyword[20] := 'PROCEDURE';
Keyword[21] := 'PROGRAM';
Keyword[22] := 'RECORD';
Keyword[23] := 'REPEAT';
Keyword[24] := 'SHL';
Keyword[25] := 'SHR';
Keyword[26] := 'STRING';
Keyword[27] := 'THEN';
Keyword[28] := 'TO';
Keyword[29] := 'TYPE';
Keyword[30] := 'UNTIL';
Keyword[31] := 'VAR';
Keyword[32] := 'WHILE';
Keyword[33] := 'XOR';
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

// Dispose dynamically allocated parameter and field data
for i := 1 to NumTypes do
  begin
  if Types[i].TypeKind = PROCEDURALTYPE then
    for j := 1 to Types[i].NumParams do
      Dispose(Types[i].Param[j]);
  
  if Types[i].TypeKind = RECORDTYPE then
    for j := 1 to Types[i].NumFields do
      Dispose(Types[i].Field[j]);
  end;    
end;





procedure WriteToken(TokKind: Integer);
begin
case TokKind of
  0:                                                Write('no token');
  OPARTOK:                                          Write('(');
  CPARTOK:                                          Write(')');
  MULTOK:                                           Write('*');
  PLUSTOK:                                          Write('+');
  COMMATOK:                                         Write(',');
  MINUSTOK:                                         Write('-');
  PERIODTOK:                                        Write('.');
  RANGETOK:                                         Write('..');
  DIVTOK:                                           Write('/');
  COLONTOK:                                         Write(':');
  ASSIGNTOK:                                        Write(':=');
  SEMICOLONTOK:                                     Write(';');
  LTTOK:                                            Write('<');
  LETOK:                                            Write('<=');
  NETOK:                                            Write('<>');
  EQTOK:                                            Write('=');
  GTTOK:                                            Write('>');
  GETOK:                                            Write('>=');
  ADDRESSTOK:                                       Write('@');
  OBRACKETTOK:                                      Write('[');
  CBRACKETTOK:                                      Write(']');
  DEREFERENCETOK:                                   Write('^');
  NUMDELIMITERS + 1 .. NUMDELIMITERS + NUMKEYWORDS: Write(Keyword[TokKind - NUMDELIMITERS]);
  IDENTTOK:                                         Write('identifier');
  INTNUMBERTOK, FRACNUMBERTOK:                      Write('number');
  CHARLITERALTOK, STRINGLITERALTOK:                 Write('literal')
else
  Write('unknown token');
end; //case
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
  if S > Keyword[Mid] then
    Min := Mid + 1
  else
    Max := Mid - 1;
  Found := S = Keyword[Mid];
until Found or (Min > Max);

if Found then Result := NUMDELIMITERS + Mid;
end;





function GetIdentUnsafe(const S: TString): Integer;
var
  IdentIndex, BlockStackIndex: Integer;
begin
for BlockStackIndex := BlockStackTop downto 1 do
  for IdentIndex := NumIdent downto 1 do
    if (Ident[IdentIndex].Name = S) and (Ident[IdentIndex].Block = BlockStack[BlockStackIndex]) then 
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
  if Types[RecType].Field[FieldIndex]^.Name = S then
    begin
    Result := FieldIndex;
    Exit;
    end;

Result := 0;
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
Result := IsOrdinalType(T) or (T.TypeKind = FILETYPE) or (T.TypeKind = POINTERTYPE) or (T.TypeKind = PROCEDURALTYPE);
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
  INTEGERTYPE:    Result := SizeOf(Integer);
  SMALLINTTYPE:   Result := SizeOf(SmallInt);
  SHORTINTTYPE:   Result := SizeOf(ShortInt);
  CHARTYPE:       Result := SizeOf(Char);
  BOOLEANTYPE:    Result := SizeOf(Boolean);
  REALTYPE:       Result := SizeOf(Single);
  POINTERTYPE:    Result := SizeOf(Pointer);
  FILETYPE:       Result := SizeOf(Integer);
  SUBRANGETYPE:   Result := SizeOf(Integer);
  ARRAYTYPE:      if Types[DataType].IsOpenArray then
                    Error('Illegal type', '', -1)
                  else  
                    Result := (HighBound(Types[DataType].IndexType) - LowBound(Types[DataType].IndexType) + 1) * TypeSize(Types[DataType].BaseType);
  RECORDTYPE:     begin
                  Result := 0;
                  for i := 1 to Types[DataType].NumFields do
                    Result := Result + TypeSize(Types[DataType].Field[i]^.DataType);
                  end;
  PROCEDURALTYPE: Result := SizeOf(Pointer)               
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
    
  // Procedural types are compatible with any untyped pointers
  if ((Types[LeftType].TypeKind = PROCEDURALTYPE) and (Types[RightType].TypeKind = POINTERTYPE) and (Types[RightType].BaseType = ANYTYPE)) or
     ((Types[LeftType].TypeKind = POINTERTYPE) and (Types[LeftType].BaseType = ANYTYPE) and (Types[RightType].TypeKind = PROCEDURALTYPE)) then
    Result := RightType;    

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
  if not IsOrdinalType(Types[DataType]) and 
    (Types[DataType].TypeKind <> REALTYPE) and 
    (Types[DataType].TypeKind <> POINTERTYPE) and
    (Types[DataType].TypeKind <> PROCEDURALTYPE) 
  then
    Error('Operator is not applicable', '', -1);
   
  if (IsIntegerType(Types[DataType]) and not IsOperatorForIntegers(Tok)) or
    ((Types[DataType].TypeKind = REALTYPE) and not IsOperatorForReals(Tok)) or   
    ((Types[DataType].TypeKind = CHARTYPE) and not IsRelationOperator(Tok)) or
    ((Types[DataType].TypeKind = BOOLEANTYPE) and not IsOperatorForBooleans(Tok)) or
    ((Types[DataType].TypeKind = POINTERTYPE) and not IsRelationOperator(Tok)) or
    ((Types[DataType].TypeKind = PROCEDURALTYPE) and not IsRelationOperator(Tok)) 
  then
    Error('Operator is not applicable', '', -1);
  end;  
end;  



