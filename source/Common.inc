// XD Pascal - a 32-bit compiler for Windows
// Copyright (c) 2009-2010, 2019, Vasiliy Tereshkov

// Common structures and routines



const
  VERSIONMAJOR              = 0;
  VERSIONMINOR              = 9;
  
  NUMKEYWORDS               = 39;          
  MAXSTRLENGTH              = 255;
  MAXSETELEMENTS            = 256;
  MAXENUMELEMENTS           = 256;
  MAXIDENTS                 = 1000;
  MAXTYPES                  = 1000;
  MAXBLOCKS                 = 1000;
  MAXBLOCKNESTING           = 10;
  MAXPARAMS                 = 20;
  MAXUNITNESTING            = 5;
  MAXFIELDS                 = 100;
  MAXRELOCS                 = 20000;
  MAXLOOPNESTING            = 20;
  MAXWITHNESTING            = 20;
  MAXGOTOS                  = 100;
  MAXBREAKCALLS             = 100;
  MAXEXITCALLS              = 100;

  MAXCODESIZE               =  1 * 1024 * 1024;
  MAXINITIALIZEDDATASIZE    =  1 * 1024 * 1024;
  MAXUNINITIALIZEDDATASIZE  = 32 * 1024 * 1024;
  MAXSTACKSIZE              = 16 * 1024 * 1024;
  



type
  TString  = string;  
  TKeyName = string;
  
  TInFile = file;  
  TOutFile = file;  
  
  TTokenKind =
    (
    EMPTYTOK,
    
    // Delimiters        
    OPARTOK,
    CPARTOK,
    MULTOK,
    PLUSTOK,
    COMMATOK,
    MINUSTOK,
    PERIODTOK,
    RANGETOK,
    DIVTOK,
    COLONTOK,
    ASSIGNTOK,
    SEMICOLONTOK,
    LTTOK,
    LETOK,
    NETOK,
    EQTOK,
    GTTOK,
    GETOK,
    ADDRESSTOK,
    OBRACKETTOK,
    CBRACKETTOK,
    DEREFERENCETOK,

    // Keywords
    ANDTOK,
    ARRAYTOK,
    BEGINTOK,
    CASETOK,
    CONSTTOK,
    IDIVTOK,
    DOTOK,
    DOWNTOTOK,
    ELSETOK,
    ENDTOK,
    FILETOK,
    FORTOK,
    FUNCTIONTOK,
    GOTOTOK,
    IFTOK,
    INTOK,
    LABELTOK,
    MODTOK,
    NILTOK,
    NOTTOK,
    OFTOK,
    ORTOK,
    PACKEDTOK,
    PROCEDURETOK,
    PROGRAMTOK,
    RECORDTOK,
    REPEATTOK,
    SETTOK,
    SHLTOK,
    SHRTOK,
    STRINGTOK,
    THENTOK,
    TOTOK,
    TYPETOK,
    UNTILTOK,
    VARTOK,
    WHILETOK,
    WITHTOK,
    XORTOK,

    // User tokens
    IDENTTOK,
    INTNUMBERTOK,
    FRACNUMBERTOK,
    CHARLITERALTOK,
    STRINGLITERALTOK    
    );
    
  TToken = record
    Kind: TTokenKind;
    Name: TString;
    Value: LongInt;
    FracValue: Single;
    StrAddress: Integer;
    StrLength: Integer;
  end;   
  
  TPassMethod = (EMPTYPASSING, VALPASSING, CONSTPASSING, VARPASSING); 

  TParam = record
    Name: TString;
    DataType: Integer;
    PassMethod: TPassMethod;
  end;

  PParam = ^TParam;
  
  PParams = array [1..MAXPARAMS] of PParam;   
    
  TIdentKind = (EMPTYIDENT, GOTOLABEL, CONSTANT, USERTYPE, VARIABLE, PROC, FUNC);
  
  TScope = (EMPTYSCOPE, GLOBAL, LOCAL);
  
  TRelocType = (EMPTYRELOC, CODERELOC, INITDATARELOC, UNINITDATARELOC);
  
  TPredefProc = 
    (
    EMPTYPROC,
    
    // Procedures     
    INCPROC, 
    DECPROC, 
    READPROC, 
    WRITEPROC, 
    READLNPROC, 
    WRITELNPROC, 
    NEWPROC, 
    DISPOSEPROC, 
    BREAKPROC, 
    CONTINUEPROC,
    EXITPROC,
    HALTPROC,

    // Functions
    SIZEOFFUNC,
    ORDFUNC,
    CHRFUNC,
    PREDFUNC,
    SUCCFUNC,
    ROUNDFUNC,
    TRUNCFUNC,
    ABSFUNC,
    SQRFUNC,
    SINFUNC,
    COSFUNC,
    ARCTANFUNC,
    EXPFUNC,
    LNFUNC,
    SQRTFUNC
    );

  TIdentifier = record
    Kind: TIdentKind;
    Name: TString;
    Value: LongInt;                    // Value for a constant, address for a label, variable, procedure or function
    FracValue: Single;
    Block: Integer;                    // Index of a block in which the identifier is defined
    NestingLevel: Byte;
    DataType: Integer;
    RecType: Integer;                  // Parent record type code for a field
    Scope: TScope;
    RelocType: TRelocType;
    PassMethod: TPassMethod;           // Value, CONST or VAR parameter status
    NumParams: Integer;
    Param: PParams;
    ProcAsBlock: Integer;
    PredefProc: TPredefProc;
    IsUnresolvedForward: Boolean;
    IsExternal: Boolean;
    ForLoopNesting: Integer;           // Number of nested FOR loops where the label is defined
  end;

  TField = record
    Name: TString;
    DataType: Integer;
    Offset: Integer;
  end;    

  TTypeKind = (EMPTYTYPE, ANYTYPE, INTEGERTYPE, SMALLINTTYPE, SHORTINTTYPE, WORDTYPE, BYTETYPE, CHARTYPE, BOOLEANTYPE, REALTYPE,
               POINTERTYPE, FILETYPE, ARRAYTYPE, RECORDTYPE, SETTYPE, ENUMERATEDTYPE, SUBRANGETYPE, PROCEDURALTYPE, FORWARDTYPE);

  TType = record
    Kind: TTypeKind;
    Block: Integer;
    
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
  
  TRelocatable = record
    RelocType: TRelocType;
    Pos: LongInt;
    Value: LongInt;
  end;       
    
  TUnit = record
    FileName: TString;
    Buffer: PChar;
    Size, Pos, Line: Integer;
  end;  
  
  TConst = record
    Value: LongInt;       // For integer numbers 
    FracValue: Single;    // For floating-point numbers
  end;

  TGoto = record
    Pos: LongInt;
    LabelIndex: Integer;
    ForLoopNesting: Integer;
  end;  
  
  TBreakContinueExitCallList = record
    NumCalls: Integer;
    Pos: array [1..MAXBREAKCALLS] of LongInt;
  end;  
  
  TWithDesignator = record
    TempPointer: Integer;
    DataType: Integer;
  end;
  
  

  
const    
  // Predefined type indices
  ANYTYPEINDEX          = 1;      // Untyped parameter, or base type for untyped pointers
  INTEGERTYPEINDEX      = 2;
  SMALLINTTYPEINDEX     = 3;
  SHORTINTTYPEINDEX     = 4;
  WORDTYPEINDEX         = 5;
  BYTETYPEINDEX         = 6;  
  CHARTYPEINDEX         = 7;
  BOOLEANTYPEINDEX      = 8;
  REALTYPEINDEX         = 9;
  POINTERTYPEINDEX      = 10;     // Untyped pointer, compatible with any other
  FILETYPEINDEX         = 11;     // Universal file type
  STRINGTYPEINDEX       = 12;

  // Keywords
  Keyword: array [1..NUMKEYWORDS] of TKeyName = 
    (
    'AND',
    'ARRAY',
    'BEGIN',
    'CASE',
    'CONST',
    'DIV',
    'DO',
    'DOWNTO',
    'ELSE',
    'END',
    'FILE',
    'FOR',
    'FUNCTION',
    'GOTO',
    'IF',
    'IN',
    'LABEL',
    'MOD',
    'NIL',
    'NOT',
    'OF',
    'OR',
    'PACKED',
    'PROCEDURE',
    'PROGRAM',
    'RECORD',
    'REPEAT',
    'SET',
    'SHL',
    'SHR',
    'STRING',
    'THEN',
    'TO',
    'TYPE',
    'UNTIL',
    'VAR',
    'WHILE',
    'WITH',
    'XOR'
    );
    
    
 

var
  Ident: array [1..MAXIDENTS] of TIdentifier;
  Types: array [1..MAXTYPES] of TType;
  UnitStack: array [1..MAXUNITNESTING] of TUnit;
  Code: array [0..MAXCODESIZE - 1] of Byte;
  InitializedGlobalData: array [0..MAXINITIALIZEDDATASIZE - 1] of Char;
  CodePosStack: array [0..1023] of Integer;
  BlockStack: array [1..MAXBLOCKNESTING] of Integer;
  Reloc: array [1..MAXRELOCS] of TRelocatable;
  Gotos: array [1..MAXGOTOS] of TGoto;
  BreakCall, ContinueCall: array [1..MAXLOOPNESTING] of TBreakContinueExitCallList;
  ExitCall: TBreakContinueExitCallList;
  WithStack: array [1..MAXWITHNESTING] of TWithDesignator;

  Tok: TToken;
  
  MultiplicativeOperators, AdditiveOperators, UnaryOperators, RelationOperators,
  OperatorsForIntegers, OperatorsForReals, OperatorsForBooleans: set of TTokenKind;
  
  IntegerTypes, OrdinalTypes, UnsignedTypes, NumericTypes, StructuredTypes, CastableTypes: set of TTypeKind;

  NumIdent, NumTypes, NumImports, NumBlocks, BlockStackTop, NumRelocs, NumGotos, ForLoopNesting, WithNesting,
  CodeSize, CodePosStackTop, 
  InitializedGlobalDataSize, UninitializedGlobalDataSize, LocalDataSize, ParamDataSize, TempDataSize,
  ProgramEntryPoint,
  UnitStackTop: Integer;
  
  IsConsoleProgram: Integer;      // More convenient than Boolean
  



procedure ZeroAll;
begin
FillChar(Ident, SizeOf(Ident), #0);
FillChar(Types, SizeOf(Types), #0);
FillChar(UnitStack, SizeOf(UnitStack), #0);
FillChar(InitializedGlobalData, SizeOf(InitializedGlobalData), #0);

NumIdent                    := 0; 
NumTypes                    := 0; 
NumImports                  := 0; 
NumBlocks                   := 0; 
BlockStackTop               := 0; 
NumRelocs                   := 0;
NumGotos                    := 0;
ForLoopNesting              := 0;
WithNesting                 := 0;
CodeSize                    := 0; 
CodePosStackTop             := 0;
InitializedGlobalDataSize   := 0;
UninitializedGlobalDataSize := 0;
LocalDataSize               := 0;
ParamDataSize               := 0;
TempDataSize                := 0;
ProgramEntryPoint           := 0;
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
IntegerTypes     := [INTEGERTYPE, SMALLINTTYPE, SHORTINTTYPE, WORDTYPE, BYTETYPE];
OrdinalTypes     := IntegerTypes + [CHARTYPE, BOOLEANTYPE, SUBRANGETYPE, ENUMERATEDTYPE];
UnsignedTypes    := [WORDTYPE, BYTETYPE, CHARTYPE];
NumericTypes     := IntegerTypes + [REALTYPE];
StructuredTypes  := [ARRAYTYPE, RECORDTYPE, SETTYPE, FILETYPE];
CastableTypes    := OrdinalTypes + [POINTERTYPE, PROCEDURALTYPE];
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
  if Types[i].Kind = PROCEDURALTYPE then
    for j := 1 to Types[i].NumParams do
      Dispose(Types[i].Param[j]);
  
  if Types[i].Kind = RECORDTYPE then
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




function GetTokSpelling(TokKind: TTokenKind): TString;
begin
case TokKind of
  EMPTYTOK:                          Result := 'no token';
  OPARTOK:                           Result := '(';
  CPARTOK:                           Result := ')';
  MULTOK:                            Result := '*';
  PLUSTOK:                           Result := '+';
  COMMATOK:                          Result := ',';
  MINUSTOK:                          Result := '-';
  PERIODTOK:                         Result := '.';
  RANGETOK:                          Result := '..';
  DIVTOK:                            Result := '/';
  COLONTOK:                          Result := ':';
  ASSIGNTOK:                         Result := ':=';
  SEMICOLONTOK:                      Result := ';';
  LTTOK:                             Result := '<';
  LETOK:                             Result := '<=';
  NETOK:                             Result := '<>';
  EQTOK:                             Result := '=';
  GTTOK:                             Result := '>';
  GETOK:                             Result := '>=';
  ADDRESSTOK:                        Result := '@';
  OBRACKETTOK:                       Result := '[';
  CBRACKETTOK:                       Result := ']';
  DEREFERENCETOK:                    Result := '^';
  ANDTOK..XORTOK:                    Result := Keyword[Ord(TokKind) - Ord(ANDTOK) + 1];
  IDENTTOK:                          Result := 'identifier';
  INTNUMBERTOK, FRACNUMBERTOK:       Result := 'number';
  CHARLITERALTOK:                    Result := 'character literal';
  STRINGLITERALTOK:                  Result := 'string literal'
else
  Result := 'unknown token';
end; //case
end;




  
procedure Error(const Msg: TString);
begin
if UnitStackTop >= 1 then
  WriteLn('Error ', UnitStack[UnitStackTop].FileName, ' ', UnitStack[UnitStackTop].Line, ': ', Msg)
else
  WriteLn('Error: ', Msg);  

DisposeAll;
Halt(1);
end;




procedure DefineStaticString(var Tok: TToken; const StrValue: TString);
var
  i: Integer;
begin
Tok.StrAddress := InitializedGlobalDataSize;  // Relocatable
Tok.StrLength := Length(StrValue);

for i := 1 to Length(StrValue) do
  begin
  InitializedGlobalData[InitializedGlobalDataSize] := StrValue[i];
  Inc(InitializedGlobalDataSize);
  if InitializedGlobalDataSize > MAXINITIALIZEDDATASIZE - 1 then
    Error('Maximum string data size exceeded');
  end;

// Add string termination character
InitializedGlobalData[InitializedGlobalDataSize] := #0;
Inc(InitializedGlobalDataSize);
end;




function LowBound(DataType: Integer): Integer;
begin
Result := 0;
case Types[DataType].Kind of
  INTEGERTYPE:    Result := -2147483647 - 1;
  SMALLINTTYPE:   Result := -32768;
  SHORTINTTYPE:   Result := -128;
  WORDTYPE:       Result :=  0;
  BYTETYPE:       Result :=  0;
  CHARTYPE:       Result :=  0;
  BOOLEANTYPE:    Result := -1;
  SUBRANGETYPE:   Result :=  Types[DataType].Low;
  ENUMERATEDTYPE: Result :=  Types[DataType].Low
else
  Error('Ordinal type expected')
end;// case
end;
                        



function HighBound(DataType: Integer): Integer;
begin
Result := 0;
case Types[DataType].Kind of
  INTEGERTYPE:    Result := 2147483647;
  SMALLINTTYPE:   Result := 32767;
  SHORTINTTYPE:   Result := 127;
  WORDTYPE:       Result := 65535;
  BYTETYPE:       Result := 255;  
  CHARTYPE:       Result := 255;
  BOOLEANTYPE:    Result := 0;
  SUBRANGETYPE:   Result := Types[DataType].High;
  ENUMERATEDTYPE: Result := Types[DataType].High
else
  Error('Ordinal type expected')
end;// case
end;




function TypeSize(DataType: Integer): Integer;
var
  i: Integer;
begin
Result := 0;
case Types[DataType].Kind of
  INTEGERTYPE:    Result := SizeOf(Integer);
  SMALLINTTYPE:   Result := SizeOf(SmallInt);
  SHORTINTTYPE:   Result := SizeOf(ShortInt);
  WORDTYPE:       Result := SizeOf(Word);
  BYTETYPE:       Result := SizeOf(Byte);  
  CHARTYPE:       Result := SizeOf(Char);
  BOOLEANTYPE:    Result := SizeOf(Boolean);
  REALTYPE:       Result := SizeOf(Single);
  POINTERTYPE:    Result := SizeOf(Pointer);
  FILETYPE:       Result := SizeOf(TString) + SizeOf(Integer);  // Name + Handle
  SUBRANGETYPE:   Result := SizeOf(Integer);
  ARRAYTYPE:      if Types[DataType].IsOpenArray then
                    Error('Illegal type')
                  else  
                    Result := (HighBound(Types[DataType].IndexType) - LowBound(Types[DataType].IndexType) + 1) * TypeSize(Types[DataType].BaseType);
  RECORDTYPE:     begin
                  Result := 0;
                  for i := 1 to Types[DataType].NumFields do
                    Result := Result + TypeSize(Types[DataType].Field[i]^.DataType);
                  end;
  SETTYPE:        Result := MAXSETELEMENTS;
  ENUMERATEDTYPE: Result := SizeOf(Byte);                
  PROCEDURALTYPE: Result := SizeOf(Pointer)               
else
  Error('Illegal type')
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
  if (Types[LeftType].Kind = SETTYPE) and (Types[RightType].Kind = SETTYPE) then
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
  if (Types[LeftType].Kind = POINTERTYPE) and (Types[RightType].Kind = POINTERTYPE) and
     ((Types[LeftType].BaseType = ANYTYPEINDEX) or (Types[RightType].BaseType = ANYTYPEINDEX)) then
    Result := LeftType;
    
  // Procedural types are compatible with any untyped pointers
  if ((Types[LeftType].Kind = PROCEDURALTYPE) and (Types[RightType].Kind = POINTERTYPE) and (Types[RightType].BaseType = ANYTYPEINDEX)) or
     ((Types[LeftType].Kind = POINTERTYPE) and (Types[LeftType].BaseType = ANYTYPEINDEX) and (Types[RightType].Kind = PROCEDURALTYPE)) then
    Result := RightType;    

  // Subranges are compatible with their host types
  if Types[LeftType].Kind = SUBRANGETYPE then
    Result := GetCompatibleType(Types[LeftType].HostType, RightType);
  if Types[RightType].Kind = SUBRANGETYPE then
    Result := GetCompatibleType(LeftType, Types[RightType].HostType);

  // Integers
  if (Types[LeftType].Kind in IntegerTypes) and (Types[RightType].Kind in IntegerTypes) then
    Result := LeftType;

  // Booleans
  if (Types[LeftType].Kind = BOOLEANTYPE) and
     (Types[RightType].Kind = BOOLEANTYPE) then
    Result := LeftType;

  // Characters
  if (Types[LeftType].Kind = CHARTYPE) and
     (Types[RightType].Kind = CHARTYPE) then
    Result := LeftType;
  end; // if

if Result = 0 then
  Error('Incompatible types');  
end;




function GetCompatibleRefType(LeftType, RightType: Integer): Integer;
begin
// This function is asymmetric and implies Variable(LeftType) := Variable(RightType)
Result := 0;

if LeftType = RightType then                 // General rule
  Result := RightType
else                                         // Special cases
  begin
  // Open arrays are compatible with any other arrays of the same base type
  if (Types[LeftType].Kind = ARRAYTYPE) and (Types[RightType].Kind = ARRAYTYPE) and 
      Types[LeftType].IsOpenArray and (Types[LeftType].BaseType = Types[RightType].BaseType) 
  then       
    Result := RightType;

  // Untyped pointers are compatible with any other pointers 
  if (Types[LeftType].Kind = POINTERTYPE) and (Types[RightType].Kind = POINTERTYPE) and
     (Types[LeftType].BaseType = ANYTYPEINDEX) 
  then  
    Result := RightType;
    
  // Untyped parameters are compatible with any type
  if Types[LeftType].Kind = ANYTYPE then
    Result := RightType;
  end; // if  

if Result = 0 then
  Error('Incompatible types');  
end;




function ConversionToRealIsPossible(SrcType, DestType: Integer): Boolean;
begin
// Implicit type conversion is possible if DestType is real and SrcType is integer or a subrange of integer
Result := (Types[DestType].Kind = REALTYPE) and
          ((Types[SrcType].Kind in IntegerTypes) or
           ((Types[SrcType].Kind = SUBRANGETYPE) and (Types[Types[SrcType].HostType].Kind in IntegerTypes)));
end;




procedure CheckOperator(const Tok: TToken; DataType: Integer); 
begin
if Types[DataType].Kind = SUBRANGETYPE then
  CheckOperator(Tok, Types[DataType].HostType)
else 
  begin
  if not (Types[DataType].Kind in OrdinalTypes) and 
    (Types[DataType].Kind <> REALTYPE) and 
    (Types[DataType].Kind <> POINTERTYPE) and
    (Types[DataType].Kind <> PROCEDURALTYPE) 
  then
    Error('Operator ' + GetTokSpelling(Tok.Kind) + ' is not applicable');
   
  if ((Types[DataType].Kind in IntegerTypes)  and not (Tok.Kind in OperatorsForIntegers)) or
     ((Types[DataType].Kind = REALTYPE)       and not (Tok.Kind in OperatorsForReals)) or   
     ((Types[DataType].Kind = CHARTYPE)       and not (Tok.Kind in RelationOperators)) or
     ((Types[DataType].Kind = BOOLEANTYPE)    and not (Tok.Kind in OperatorsForBooleans)) or
     ((Types[DataType].Kind = POINTERTYPE)    and not (Tok.Kind in RelationOperators)) or
     ((Types[DataType].Kind = ENUMERATEDTYPE) and not (Tok.Kind in RelationOperators)) or
     ((Types[DataType].Kind = PROCEDURALTYPE) and not (Tok.Kind in RelationOperators)) 
  then
    Error('Operator ' + GetTokSpelling(Tok.Kind) + ' is not applicable');
  end;  
end;  




function GetKeyword(const Name: TKeyName): TTokenKind;
var
  Max, Mid, Min: Integer;
  Found: Boolean;
begin
Result := EMPTYTOK;

// Binary search
Min := 1;
Max := NUMKEYWORDS;

repeat
  Mid := (Min + Max) div 2;
  if Name > Keyword[Mid] then
    Min := Mid + 1
  else
    Max := Mid - 1;
  Found := Name = Keyword[Mid];
until Found or (Min > Max);

if Found then Result := TTokenKind(Ord(ANDTOK) - 1 + Mid);
end;





function GetIdentUnsafe(const Name: TString): Integer;
var
  IdentIndex, BlockStackIndex: Integer;
begin
for BlockStackIndex := BlockStackTop downto 1 do
  for IdentIndex := NumIdent downto 1 do
    if (Ident[IdentIndex].Name = Name) and (Ident[IdentIndex].Block = BlockStack[BlockStackIndex]) then 
      begin
      Result := IdentIndex;
      Exit;
      end;
      
Result := 0;
end;




function GetIdent(const Name: TString): Integer;
begin
Result := GetIdentUnsafe(Name);
if Result = 0 then
  Error('Unknown identifier ' + Name);
end;




procedure DeclareIdent(const Name: TString; Kind: TIdentKind; TotalNumParams: Integer; DataType: Integer; PassMethod: TPassMethod; ConstValue: LongInt; FracConstValue: Single; PredefProc: TPredefProc);
var
  i, AdditionalStackItems: Integer;
  Scope: TScope;
begin
if BlockStack[BlockStackTop] = 1 then Scope := GLOBAL else Scope := LOCAL;

i := GetIdentUnsafe(Name);

if (i > 0) and (Ident[i].Block = BlockStack[BlockStackTop]) then
  Error('Duplicate identifier ' + Name);

Inc(NumIdent);
if NumIdent > MAXIDENTS then
  Error('Maximum number of identifiers exceeded');
  
Ident[NumIdent].Name := Name;
Ident[NumIdent].Kind := Kind;
Ident[NumIdent].Scope := Scope;
Ident[NumIdent].RelocType := UNINITDATARELOC;
Ident[NumIdent].DataType := DataType;
Ident[NumIdent].Block := BlockStack[BlockStackTop];
Ident[NumIdent].NestingLevel := BlockStackTop;
Ident[NumIdent].NumParams := 0;
Ident[NumIdent].PassMethod := PassMethod;
Ident[NumIdent].IsUnresolvedForward := FALSE;
Ident[NumIdent].IsExternal := FALSE;
Ident[NumIdent].ForLoopNesting := 0;

case Kind of
  PROC, FUNC:
    if PredefProc = EMPTYPROC then
      begin
      Ident[NumIdent].Value := CodeSize;                              // Routine entry point address
      Ident[NumIdent].PredefProc := EMPTYPROC;
      end
    else
      begin
      Ident[NumIdent].Value := 0;
      Ident[NumIdent].PredefProc := PredefProc;                       // Predefined routine index
      end;

  VARIABLE:
    case Scope of
     GLOBAL:
       begin
       Ident[NumIdent].Value := UninitializedGlobalDataSize;                                 // Variable address (relocatable)
       UninitializedGlobalDataSize := UninitializedGlobalDataSize + TypeSize(DataType);
       end;// else

     LOCAL:
       if TotalNumParams > 0 then
         begin          
         if Ident[NumIdent].NestingLevel = 2 then                                            // Inside a non-nested routine
           AdditionalStackItems := 1                                                         // Return address
         else                                                                                // Inside a nested routine
           AdditionalStackItems := 2;                                                        // Return address, static link (hidden parameter)  

         Ident[NumIdent].Value := (AdditionalStackItems + TotalNumParams) * SizeOf(LongInt) - ParamDataSize;  // Parameter offset from EBP (>0)
         ParamDataSize := ParamDataSize + SizeOf(LongInt);                                   // Parameters always occupy 4 bytes each
         end
       else
         begin
         Ident[NumIdent].Value := -LocalDataSize - TypeSize(DataType);                       // Local variable offset from EBP (<0)
         LocalDataSize := LocalDataSize + TypeSize(DataType);
         end;
    end; // case


  CONSTANT:
    if PassMethod = VALPASSING then                                     // Untyped constant
      if Types[DataType].Kind = REALTYPE then
        Ident[NumIdent].FracValue := FracConstValue                     // Real constant value
      else
        Ident[NumIdent].Value := ConstValue                             // Ordinal constant value
    else                                                                // Typed constant (actually an initialized global variable)    
      begin
      Ident[NumIdent].Kind := VARIABLE;
      Ident[NumIdent].Scope := GLOBAL;
      Ident[NumIdent].RelocType := INITDATARELOC;
      Ident[NumIdent].PassMethod := VALPASSING;
      
      Ident[NumIdent].Value := InitializedGlobalDataSize;               // Typed constant address (relocatable)
      InitializedGlobalDataSize := InitializedGlobalDataSize + TypeSize(DataType);      
      end;
      
  GOTOLABEL:
    Ident[NumIdent].IsUnresolvedForward := TRUE;

end;// case


if InitializedGlobalDataSize >= MAXINITIALIZEDDATASIZE then
  Error('Maximum initialized data size exceeded');

if UninitializedGlobalDataSize >= MAXUNINITIALIZEDDATASIZE then
  Error('Maximum uninitialized data size exceeded');

if LocalDataSize >= MAXSTACKSIZE then
  Error('Maximum local data size exceeded');

if ParamDataSize >= MAXSTACKSIZE then
  Error('Maximum parameter data size exceeded');

end;





function GetFieldUnsafe(RecType: Integer; const Name: TString): Integer;
var
  FieldIndex: Integer;
begin
for FieldIndex := 1 to Types[RecType].NumFields do
  if Types[RecType].Field[FieldIndex]^.Name = Name then
    begin
    Result := FieldIndex;
    Exit;
    end;

Result := 0;
end;




function GetField(RecType: Integer; const Name: TString): Integer;
begin
Result := GetFieldUnsafe(RecType, Name);
if Result = 0 then
  Error('Unknown field ' + Name);
end;




function GetFieldInsideWith(var RecPointer: Integer; var RecType: Integer; const Name: TString): Integer;
var
  FieldIndex, WithIndex: Integer;
begin 
for WithIndex := WithNesting downto 1 do
  begin
  RecType := WithStack[WithIndex].DataType;
  FieldIndex := GetFieldUnsafe(RecType, Name);
  
  if FieldIndex <> 0 then
    begin
    RecPointer := WithStack[WithIndex].TempPointer;
    Result := FieldIndex;
    Exit;
    end;
  end;

Result := 0;  
end;




function FieldInsideWithFound(const Name: TString): Boolean;
var
  RecPointer: Integer; 
  RecType: Integer;        
begin
Result := GetFieldInsideWith(RecPointer, RecType, Name) <> 0;
end;

