// XD Pascal - a 32-bit compiler for Windows
// Copyright (c) 2009-2010, 2019, Vasiliy Tereshkov

{$I-}
{$H-}
{$J+}

unit Common;


interface


const
  VERSIONMAJOR              = 0;
  VERSIONMINOR              = 9;
  
  NUMKEYWORDS               = 43;          
  MAXSTRLENGTH              = 255;
  MAXSETELEMENTS            = 256;
  MAXENUMELEMENTS           = 256;
  MAXIDENTS                 = 1000;
  MAXTYPES                  = 1000;
  MAXUNITS                  = 100;
  MAXBLOCKS                 = 1000;
  MAXBLOCKNESTING           = 10;
  MAXPARAMS                 = 20;
  MAXFIELDS                 = 100;
  MAXLOOPNESTING            = 20;
  MAXWITHNESTING            = 20;
  MAXGOTOS                  = 100;
  MAXBREAKCALLS             = 100;
  MAXEXITCALLS              = 100;

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
    IMPLEMENTATIONTOK,
    INTOK,
    INTERFACETOK,
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
    UNITTOK,
    UNTILTOK,
    USESTOK,
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
    Name: TString;
  case Kind: TTokenKind of  
    INTNUMBERTOK:     (Value: LongInt);       // For all ordinal types
    FRACNUMBERTOK:    (FracValue: Single);
    STRINGLITERALTOK: (StrAddress: Integer;
                       StrLength: Integer);
  end;
  
  TTypeKind = (EMPTYTYPE, ANYTYPE, INTEGERTYPE, SMALLINTTYPE, SHORTINTTYPE, WORDTYPE, BYTETYPE, CHARTYPE, BOOLEANTYPE, REALTYPE,
               POINTERTYPE, FILETYPE, ARRAYTYPE, RECORDTYPE, SETTYPE, ENUMERATEDTYPE, SUBRANGETYPE, PROCEDURALTYPE, FORWARDTYPE);  




const
  // Predefined type indices
  ANYTYPEINDEX          = 1;      // Untyped parameter, or base type for untyped pointers and files
  INTEGERTYPEINDEX      = 2;
  SMALLINTTYPEINDEX     = 3;
  SHORTINTTYPEINDEX     = 4;
  WORDTYPEINDEX         = 5;
  BYTETYPEINDEX         = 6;  
  CHARTYPEINDEX         = 7;
  BOOLEANTYPEINDEX      = 8;
  REALTYPEINDEX         = 9;
  POINTERTYPEINDEX      = 10;     // Untyped pointer, compatible with any other
  FILETYPEINDEX         = 11;     // Untyped file, compatible with text files
  STRINGTYPEINDEX       = 12;



type
  TConst = record
  case Kind: TTypeKind of
    INTEGERTYPE: (Value: LongInt);            // For all ordinal types 
    REALTYPE:    (FracValue: Single);    
  end;   
  
  TPassMethod = (EMPTYPASSING, VALPASSING, CONSTPASSING, VARPASSING); 

  TParam = record
    Name: TString;
    DataType: Integer;
    PassMethod: TPassMethod;
    Default: TConst;
  end;

  PParam = ^TParam;
  
  PParams = array [1..MAXPARAMS] of PParam;   
    
  TIdentKind = (EMPTYIDENT, GOTOLABEL, CONSTANT, USERTYPE, VARIABLE, PROC, FUNC);
  
  TScope = (EMPTYSCOPE, GLOBAL, LOCAL);
  
  TRelocType = (EMPTYRELOC, CODERELOC, INITDATARELOC, UNINITDATARELOC, IMPORTRELOC);
  
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
    UnitIndex: Integer;
    Block: Integer;                       // Index of a block in which the identifier is defined
    NestingLevel: Byte;    
    RecType: Integer;                     // Parent record type code for a field
    Scope: TScope;
    RelocType: TRelocType;
    PassMethod: TPassMethod;              // Value, CONST or VAR parameter status
    NumParams: Integer;
    NumDefaultParams: Integer;
    Param: PParams;
    ResultIdentIndex: Integer;
    ProcAsBlock: Integer;
    PredefProc: TPredefProc;
    IsUnresolvedForward: Boolean;
    IsStdCall: Boolean;
    IsExported: Boolean;
    ForLoopNesting: Integer;              // Number of nested FOR loops where the label is defined
  case DataType: Integer of
    INTEGERTYPEINDEX: (Value: LongInt);   // Value for an ordinal constant, address for a label, variable, procedure or function
    REALTYPEINDEX:    (FracValue: Single);// Value for a real constant
  end;

  TField = record
    Name: TString;
    DataType: Integer;
    Offset: Integer;
  end;    

  TType = record    
    Block: Integer;
    BaseType: Integer;
    
  case Kind: TTypeKind of     
    SUBRANGETYPE:    (Low, High: Integer);
  
    ARRAYTYPE:       (IndexType: Integer;
                      IsOpenArray: Boolean);
                      
    RECORDTYPE:      (NumFields: Integer;
                      Field: array [1..MAXFIELDS] of ^TField);
                      
    PROCEDURALTYPE:  (NumParams: Integer;
                      NumDefaultParams: Integer;
                      Param: PParams;
                      ResultType: Integer;
                      IsStdCall: Boolean);
  
    FORWARDTYPE:     (TypeIdentName: TString);   
  end;
  
  TBlock = record
    Index: Integer;
    LocalDataSize, ParamDataSize, TempDataSize: LongInt;
  end;
  
  TUnit = record
    Name: TString;
    IsUsed: Boolean;
  end;  

  TUnitFile = record
    FileName: TString;
    Buffer: PChar;
    Size, Pos, Line: Integer;
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
    'IMPLEMENTATION',
    'IN',
    'INTERFACE',
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
    'UNIT',
    'UNTIL',
    'USES',
    'VAR',
    'WHILE',
    'WITH',
    'XOR'
    );
    
    
 

var
  Ident: array [1..MAXIDENTS] of TIdentifier;
  Types: array [1..MAXTYPES] of TType;
  InitializedGlobalData: array [0..MAXINITIALIZEDDATASIZE - 1] of Char;
  Units: array [1..MAXUNITS] of TUnit;
  BlockStack: array [1..MAXBLOCKNESTING] of TBlock;
  Gotos: array [1..MAXGOTOS] of TGoto;
  BreakCall, ContinueCall: array [1..MAXLOOPNESTING] of TBreakContinueExitCallList;
  ExitCall: TBreakContinueExitCallList;
  WithStack: array [1..MAXWITHNESTING] of TWithDesignator;

  Tok: TToken;
  UnitFile: TUnitFile;
  
  MultiplicativeOperators, AdditiveOperators, UnaryOperators, RelationOperators,
  OperatorsForIntegers, OperatorsForReals, OperatorsForBooleans: set of TTokenKind;
  
  IntegerTypes, OrdinalTypes, UnsignedTypes, NumericTypes, StructuredTypes, CastableTypes: set of TTypeKind;

  NumIdent, NumTypes, NumUnits, NumBlocks, BlockStackTop, NumGotos, ForLoopNesting, WithNesting,
  InitializedGlobalDataSize, UninitializedGlobalDataSize, ProgramEntryPoint: Integer;
  
  IsConsoleProgram: Boolean;
  


procedure InitializeCommon;
procedure FinalizeCommon;
function GetTokSpelling(TokKind: TTokenKind): TString;
procedure Error(const Msg: TString);
procedure DefineStaticString(var Tok: TToken; const StrValue: TString);
function LowBound(DataType: Integer): Integer;
function HighBound(DataType: Integer): Integer;
function TypeSize(DataType: Integer): Integer;
function GetCompatibleType(LeftType, RightType: Integer): Integer;
function GetCompatibleRefType(LeftType, RightType: Integer): Integer;
function ConversionToRealIsPossible(SrcType, DestType: Integer): Boolean;
procedure CheckOperator(const Tok: TToken; DataType: Integer); 
function GetKeyword(const Name: TKeyName): TTokenKind;
function GetUnit(const Name: TString): Integer;
function GetIdentUnsafe(const Name: TString; AllowForwardReference: Boolean = FALSE): Integer;
function GetIdent(const Name: TString; AllowForwardReference: Boolean = FALSE): Integer;
function GetFieldUnsafe(RecType: Integer; const Name: TString): Integer;
function GetField(RecType: Integer; const Name: TString): Integer;
function GetFieldInsideWith(var RecPointer: Integer; var RecType: Integer; const Name: TString): Integer;
function FieldInsideWithFound(const Name: TString): Boolean;



implementation



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




procedure InitializeCommon;
begin
FillChar(Ident, SizeOf(Ident), #0);
FillChar(Types, SizeOf(Types), #0);
FillChar(Units, SizeOf(Units), #0);
FillChar(InitializedGlobalData, SizeOf(InitializedGlobalData), #0);

NumIdent                    := 0; 
NumTypes                    := 0;
NumUnits                    := 0; 
NumBlocks                   := 0; 
BlockStackTop               := 0; 
NumGotos                    := 0;
ForLoopNesting              := 0;
WithNesting                 := 0;
InitializedGlobalDataSize   := 0;
UninitializedGlobalDataSize := 0;
ProgramEntryPoint           := 0;

IsConsoleProgram            := TRUE;  // Console program by default 

FillOperatorSets;
FillTypeSets;
end;




procedure FinalizeCommon;
var
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

// Dispose of unit input buffer
with UnitFile do
  if Buffer <> nil then
    begin
    FreeMem(Buffer, Size);
    Buffer := nil;
    end;
end;




function GetTokSpelling{(TokKind: TTokenKind): TString};
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




  
procedure Error{(const Msg: TString)};
begin
if NumUnits >= 1 then
  WriteLn('Error ', UnitFile.FileName, ' ', UnitFile.Line, ': ', Msg)
else
  WriteLn('Error: ', Msg);  

FinalizeCommon;
Halt(1);
end;




procedure DefineStaticString{(var Tok: TToken; const StrValue: TString)};
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




function LowBound{(DataType: Integer): Integer};
begin
Result := 0;
case Types[DataType].Kind of
  INTEGERTYPE:    Result := -2147483647 - 1;
  SMALLINTTYPE:   Result := -32768;
  SHORTINTTYPE:   Result := -128;
  WORDTYPE:       Result :=  0;
  BYTETYPE:       Result :=  0;
  CHARTYPE:       Result :=  0;
  BOOLEANTYPE:    Result :=  0;
  SUBRANGETYPE:   Result :=  Types[DataType].Low;
  ENUMERATEDTYPE: Result :=  Types[DataType].Low
else
  Error('Ordinal type expected')
end;// case
end;
                        



function HighBound{(DataType: Integer): Integer};
begin
Result := 0;
case Types[DataType].Kind of
  INTEGERTYPE:    Result := 2147483647;
  SMALLINTTYPE:   Result := 32767;
  SHORTINTTYPE:   Result := 127;
  WORDTYPE:       Result := 65535;
  BYTETYPE:       Result := 255;  
  CHARTYPE:       Result := 255;
  BOOLEANTYPE:    Result := 1;
  SUBRANGETYPE:   Result := Types[DataType].High;
  ENUMERATEDTYPE: Result := Types[DataType].High
else
  Error('Ordinal type expected')
end;// case
end;




function TypeSize{(DataType: Integer): Integer};
var
  CurSize: Integer;
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
  RECORDTYPE:     for i := 1 to Types[DataType].NumFields do
                    begin
                    CurSize := Types[DataType].Field[i]^.Offset + TypeSize(Types[DataType].Field[i]^.DataType);
                    if CurSize > Result then Result := CurSize;
                    end;  
  SETTYPE:        Result := MAXSETELEMENTS;
  ENUMERATEDTYPE: Result := SizeOf(Byte);                
  PROCEDURALTYPE: Result := SizeOf(Pointer)               
else
  Error('Illegal type')
end;// case
end;    




function GetCompatibleType{(LeftType, RightType: Integer): Integer};
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
    Result := GetCompatibleType(Types[LeftType].BaseType, RightType);
  if Types[RightType].Kind = SUBRANGETYPE then
    Result := GetCompatibleType(LeftType, Types[RightType].BaseType);

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




function GetCompatibleRefType{(LeftType, RightType: Integer): Integer};
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
    
  // Untyped files are compatible with any other files 
  if (Types[LeftType].Kind = FILETYPE) and (Types[RightType].Kind = FILETYPE) and
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




function ConversionToRealIsPossible{(SrcType, DestType: Integer): Boolean};
begin
// Implicit type conversion is possible if DestType is real and SrcType is integer or a subrange of integer
Result := (Types[DestType].Kind = REALTYPE) and
          ((Types[SrcType].Kind in IntegerTypes) or
           ((Types[SrcType].Kind = SUBRANGETYPE) and (Types[Types[SrcType].BaseType].Kind in IntegerTypes)));
end;




procedure CheckOperator{(const Tok: TToken; DataType: Integer)}; 
begin
if Types[DataType].Kind = SUBRANGETYPE then
  CheckOperator(Tok, Types[DataType].BaseType)
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




function GetKeyword{(const Name: TKeyName): TTokenKind};
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




function GetUnit{(const Name: TString): Integer};
var
  UnitIndex: Integer;
begin
for UnitIndex := 1 to NumUnits do
  if Units[UnitIndex].Name = Name then 
    begin
    Result := UnitIndex;
    Exit;
    end;
      
Result := 0;
Error('Unknown unit ' + Name);
end;




function GetIdentUnsafe{(const Name: TString; AllowForwardReference: Boolean = FALSE): Integer};
var
  IdentIndex, BlockStackIndex: Integer;
begin
// First search the current unit 
for BlockStackIndex := BlockStackTop downto 1 do
  for IdentIndex := NumIdent downto 1 do
    if (Ident[IdentIndex].Name = Name) and
       (Ident[IdentIndex].UnitIndex = NumUnits) and 
       (Ident[IdentIndex].Block = BlockStack[BlockStackIndex].Index) and       
       (AllowForwardReference or (Ident[IdentIndex].Kind <> USERTYPE) or (Types[Ident[IdentIndex].DataType].Kind <> FORWARDTYPE))
    then 
      begin
      Result := IdentIndex;
      Exit;
      end;          

// If unsuccessful, search other used units
for IdentIndex := NumIdent downto 1 do
  if (Ident[IdentIndex].Name = Name) and
     (Ident[IdentIndex].UnitIndex <> NumUnits) and 
      Ident[IdentIndex].IsExported and
      Units[Ident[IdentIndex].UnitIndex].IsUsed 
  then 
    begin
    Result := IdentIndex;
    Exit;
    end; 
     
Result := 0;
end;




function GetIdent{(const Name: TString; AllowForwardReference: Boolean = FALSE): Integer};
begin
Result := GetIdentUnsafe(Name, AllowForwardReference);
if Result = 0 then
  Error('Unknown identifier ' + Name);
end;




function GetFieldUnsafe{(RecType: Integer; const Name: TString): Integer};
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




function GetField{(RecType: Integer; const Name: TString): Integer};
begin
Result := GetFieldUnsafe(RecType, Name);
if Result = 0 then
  Error('Unknown field ' + Name);
end;




function GetFieldInsideWith{(var RecPointer: Integer; var RecType: Integer; const Name: TString): Integer};
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




function FieldInsideWithFound{(const Name: TString): Boolean};
var
  RecPointer: Integer; 
  RecType: Integer;        
begin
Result := GetFieldInsideWith(RecPointer, RecType, Name) <> 0;
end;


end.
