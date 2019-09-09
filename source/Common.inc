// XD Pascal - a 32-bit compiler for Windows
// Copyright (c) 2009-2010, 2019, Vasiliy Tereshkov

// Common structures and routines



const
  VERSIONMAJOR          = 0;
  VERSIONMINOR          = 9;
  
  NUMDELIMITERS         = 22;
  NUMKEYWORDS           = 35;

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
  INTOK                 = NUMDELIMITERS + 15;
  MODTOK                = NUMDELIMITERS + 16;
  NILTOK                = NUMDELIMITERS + 17;
  NOTTOK                = NUMDELIMITERS + 18;
  OFTOK                 = NUMDELIMITERS + 19;
  ORTOK                 = NUMDELIMITERS + 20;
  PROCEDURETOK          = NUMDELIMITERS + 21;
  PROGRAMTOK            = NUMDELIMITERS + 22;
  RECORDTOK             = NUMDELIMITERS + 23;
  REPEATTOK             = NUMDELIMITERS + 24;
  SETTOK                = NUMDELIMITERS + 25;
  SHLTOK                = NUMDELIMITERS + 26;
  SHRTOK                = NUMDELIMITERS + 27;
  STRINGTOK             = NUMDELIMITERS + 28;
  THENTOK               = NUMDELIMITERS + 29;
  TOTOK                 = NUMDELIMITERS + 30;
  TYPETOK               = NUMDELIMITERS + 31;
  UNTILTOK              = NUMDELIMITERS + 32;
  VARTOK                = NUMDELIMITERS + 33;
  WHILETOK              = NUMDELIMITERS + 34;
  XORTOK                = NUMDELIMITERS + 35;

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
  SETTYPE               = 12;
  SUBRANGETYPE          = 13;
  PROCEDURALTYPE        = 14;
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
  MAXSETELEMENTS        = 256;
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
    Buffer: PChar;
    Size, Pos, Line: Integer;
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
    
    // For pointers, arrays and sets 
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
  
  MultiplicativeOperators, AdditiveOperators, UnaryOperators, RelationOperators,
  OperatorsForIntegers, OperatorsForReals, OperatorsForBooleans,
  IntegerTypes, OrdinalTypes, NumericTypes, StructuredTypes, CastableTypes: set of Byte;

  NumIdent, NumTypes, NumImports, NumStaticStrChars, NumBlocks, BlockStackTop, NumRelocs, ForLoopNesting,
  CodeSize, CodePosStackTop, GlobalDataSize, CurBlockLocalDataSize, CurBlockTempStorageSize,
  DataSectionOrigin, ProgramEntryPoint,
  UnitStackTop: Integer;
  
  IsConsoleProgram: Integer;                                              // More convenient than Boolean
  



procedure ZeroAll;
begin
FillChar(Ident, SizeOf(Ident), #0);
FillChar(Types, SizeOf(Types), #0);
FillChar(UnitStack, SizeOf(UnitStack), #0);

NumIdent                := 0; 
NumTypes                := 0; 
NumImports              := 0; 
NumBlocks               := 0; 
BlockStackTop           := 0; 
NumRelocs               := 0;
ForLoopNesting          := 0;
CodeSize                := 0; 
CodePosStackTop         := 0;
NumStaticStrChars       := 0;
GlobalDataSize          := 0;
CurBlockLocalDataSize   := 0;
CurBlockTempStorageSize := 0;
ProgramEntryPoint       := 0;
end;



  
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
Keyword[15] := 'IN';
Keyword[16] := 'MOD';
Keyword[17] := 'NIL';
Keyword[18] := 'NOT';
Keyword[19] := 'OF';
Keyword[20] := 'OR';
Keyword[21] := 'PROCEDURE';
Keyword[22] := 'PROGRAM';
Keyword[23] := 'RECORD';
Keyword[24] := 'REPEAT';
Keyword[25] := 'SET';
Keyword[26] := 'SHL';
Keyword[27] := 'SHR';
Keyword[28] := 'STRING';
Keyword[29] := 'THEN';
Keyword[30] := 'TO';
Keyword[31] := 'TYPE';
Keyword[32] := 'UNTIL';
Keyword[33] := 'VAR';
Keyword[34] := 'WHILE';
Keyword[35] := 'XOR';
end;  




procedure FillOperatorSets;
begin
MultiplicativeOperators := [MULTOK, DIVTOK, IDIVTOK, MODTOK, SHLTOK, SHRTOK, ANDTOK];
AdditiveOperators       := [PLUSTOK, MINUSTOK, ORTOK, XORTOK];
UnaryOperators          := [PLUSTOK, MINUSTOK];
RelationOperators       := [EQTOK, NETOK, LTTOK, LETOK, GTTOK, GETOK];

OperatorsForIntegers    := MultiplicativeOperators - [DIVTOK] + AdditiveOperators + RelationOperators + [NOTTOK];
OperatorsForReals       := [MULTOK, DIVTOK, PLUSTOK, MINUSTOK] + RelationOperators;
OperatorsForBooleans    := [ANDTOK, ORTOK, XORTOK, NOTTOK] + RelationOperators;
end;




procedure FillTypeSets;
begin
IntegerTypes     := [INTEGERTYPE, SMALLINTTYPE, SHORTINTTYPE];
OrdinalTypes     := IntegerTypes + [CHARTYPE, BOOLEANTYPE, SUBRANGETYPE];
NumericTypes     := IntegerTypes + [REALTYPE];
StructuredTypes  := [ARRAYTYPE, RECORDTYPE, SETTYPE];
CastableTypes    := OrdinalTypes + [FILETYPE, POINTERTYPE, PROCEDURALTYPE];
end; 




procedure DisposeAll;
var
  Buffer: Pointer;
  i, j: Integer;
  
begin
// Dispose of dynamically allocated parameter data
for i := 1 to NumIdent do
  if (Ident[i].Kind = PROC) or (Ident[i].Kind = FUNC) then
    for j := 1 to Ident[i].NumParams do
      Dispose(Ident[i].Param[j]);

// Dispose of dynamically allocated parameter and field data
for i := 1 to NumTypes do
  begin
  if Types[i].TypeKind = PROCEDURALTYPE then
    for j := 1 to Types[i].NumParams do
      Dispose(Types[i].Param[j]);
  
  if Types[i].TypeKind = RECORDTYPE then
    for j := 1 to Types[i].NumFields do
      Dispose(Types[i].Field[j]);
  end;

// Dispose of unit input buffers
for i := UnitStackTop downto 1 do
  begin
  Buffer := UnitStack[i].Buffer;
  FreeMem(Buffer, UnitStack[i].Size);
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
if UnitStackTop >= 1 then
  Write('Error ', UnitStack[UnitStackTop].FileName, ' ', UnitStack[UnitStackTop].Line, ': ', Msg, Name)
else
  Write('Error: ', Msg, Name);  
if TokKind > -1 then WriteToken(TokKind);
WriteLn;

DisposeAll;
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
  SETTYPE:        Result := MAXSETELEMENTS;                
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
  // Sets are compatible with other sets having a compatible base type, or with an empty set constructor
  if (Types[LeftType].TypeKind = SETTYPE) and (Types[RightType].TypeKind = SETTYPE) then
    begin
    if Types[RightType].BaseType = ANYTYPEINDEX then
      Result := LeftType
    else if Types[LeftType].BaseType = ANYTYPEINDEX then
      Result := RightType
    else
      begin  
      GetCompatibleType(Types[LeftType].BaseType, Types[RightType].BaseType);
      Result := LeftType;
      end;
    end;
    
  // Untyped pointers are compatible with any other pointers
  if (Types[LeftType].TypeKind = POINTERTYPE) and (Types[RightType].TypeKind = POINTERTYPE) and
     ((Types[LeftType].BaseType = ANYTYPEINDEX) or (Types[RightType].BaseType = ANYTYPEINDEX)) then
    Result := LeftType;
    
  // Procedural types are compatible with any untyped pointers
  if ((Types[LeftType].TypeKind = PROCEDURALTYPE) and (Types[RightType].TypeKind = POINTERTYPE) and (Types[RightType].BaseType = ANYTYPEINDEX)) or
     ((Types[LeftType].TypeKind = POINTERTYPE) and (Types[LeftType].BaseType = ANYTYPEINDEX) and (Types[RightType].TypeKind = PROCEDURALTYPE)) then
    Result := RightType;    

  // Subranges are compatible with their host types
  if Types[LeftType].TypeKind = SUBRANGETYPE then
    Result := GetCompatibleType(Types[LeftType].HostType, RightType);
  if Types[RightType].TypeKind = SUBRANGETYPE then
    Result := GetCompatibleType(LeftType, Types[RightType].HostType);

  // Integers
  if (Types[LeftType].TypeKind in IntegerTypes) and (Types[RightType].TypeKind in IntegerTypes) then
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
          ((Types[SrcType].TypeKind in IntegerTypes) or
           ((Types[SrcType].TypeKind = SUBRANGETYPE) and (Types[Types[SrcType].HostType].TypeKind in IntegerTypes)));
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
  if not (Types[DataType].TypeKind in OrdinalTypes) and 
    (Types[DataType].TypeKind <> REALTYPE) and 
    (Types[DataType].TypeKind <> POINTERTYPE) and
    (Types[DataType].TypeKind <> PROCEDURALTYPE) 
  then
    Error('Operator is not applicable', '', -1);
   
  if ((Types[DataType].TypeKind in IntegerTypes)  and not (Tok.Kind in OperatorsForIntegers)) or
     ((Types[DataType].TypeKind = REALTYPE)       and not (Tok.Kind in OperatorsForReals)) or   
     ((Types[DataType].TypeKind = CHARTYPE)       and not (Tok.Kind in RelationOperators)) or
     ((Types[DataType].TypeKind = BOOLEANTYPE)    and not (Tok.Kind in OperatorsForBooleans)) or
     ((Types[DataType].TypeKind = POINTERTYPE)    and not (Tok.Kind in RelationOperators)) or
     ((Types[DataType].TypeKind = PROCEDURALTYPE) and not (Tok.Kind in RelationOperators)) 
  then
    Error('Operator is not applicable', '', -1);
  end;  
end;  



