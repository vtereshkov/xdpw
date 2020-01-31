// XD Pascal - a 32-bit compiler for Windows
// Copyright (c) 2009-2010, 2019-2020, Vasiliy Tereshkov

{$I-}
{$H-}
{$J+}

unit Common;


interface


const
  VERSIONMAJOR              = 0;
  VERSIONMINOR              = 10;
  
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
  MAXWITHNESTING            = 20;

  MAXINITIALIZEDDATASIZE    =  1 * 1024 * 1024;
  MAXUNINITIALIZEDDATASIZE  = 32 * 1024 * 1024;
  MAXSTACKSIZE              = 16 * 1024 * 1024;  



type
  TCharacter     = Char;
  PCharacter     = PChar;
  TString        = string;  
  TShortString   = string;
  TGenericString = string;
  
  PLongInt = ^LongInt;
  
  TInFile  = file;  
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
    IDENTTOK:         (NonUppercaseName: TShortString);  
    INTNUMBERTOK:     (Value: LongInt);                   // For all ordinal types
    FRACNUMBERTOK:    (FracValue: Single);
    STRINGLITERALTOK: (StrAddress: Integer;
                       StrLength: Integer);
  end;
  
  TTypeKind = (EMPTYTYPE, ANYTYPE, INTEGERTYPE, SMALLINTTYPE, SHORTINTTYPE, WORDTYPE, BYTETYPE, CHARTYPE, BOOLEANTYPE, REALTYPE,
               POINTERTYPE, FILETYPE, ARRAYTYPE, RECORDTYPE, INTERFACETYPE, SETTYPE, ENUMERATEDTYPE, SUBRANGETYPE, 
               PROCEDURALTYPE, METHODTYPE, FORWARDTYPE);  



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
    
  TSignature = record
    NumParams: Integer;
    NumDefaultParams: Integer;
    Param: PParams;
    ResultType: Integer;
    IsStdCall: Boolean;
  end;      

  TIdentifier = record
    Kind: TIdentKind;
    Name: TString;
    UnitIndex: Integer;
    Block: Integer;                       // Index of a block in which the identifier is defined
    NestingLevel: Byte;
    ReceiverName: TString;                // Receiver variable name for a method    
    ReceiverType: Integer;                // Receiver type for a method
    Scope: TScope;
    RelocType: TRelocType;
    PassMethod: TPassMethod;              // Value, CONST or VAR parameter status
    Signature: TSignature;
    ResultIdentIndex: Integer;
    ProcAsBlock: Integer;
    PredefProc: TPredefProc;
    IsUnresolvedForward: Boolean;
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

  PField = ^TField;    

  TType = record    
    Block: Integer;
    BaseType: Integer;
    
  case Kind: TTypeKind of     
    SUBRANGETYPE:              (Low, High: Integer);
  
    ARRAYTYPE:                 (IndexType: Integer;
                                IsOpenArray: Boolean);
                      
    RECORDTYPE, INTERFACETYPE: (NumFields: Integer;
                                Field: array [1..MAXFIELDS] of PField);
                      
    PROCEDURALTYPE:            (Signature: TSignature;
                                SelfPointerOffset: LongInt);  // For interface method variables as temporary results
    
    METHODTYPE:                (MethodIdentIndex: Integer);   // For static methods as temporary results
  
    FORWARDTYPE:               (TypeIdentName: TShortString);   
  end;
  
  TBlock = record
    Index: Integer;
    LocalDataSize, ParamDataSize, TempDataSize: LongInt;
  end;
  
  TUnit = record
    Name: TString;
  end;

  TUnitStatus = record
    Index: Integer;
    UsedUnits: set of Byte;
  end;      
  
  TWithDesignator = record
    TempPointer: Integer;
    DataType: Integer;
  end;
  
  TErrorProc = procedure (const Msg: TString);
  
  

var
  Ident: array [1..MAXIDENTS] of TIdentifier;
  Types: array [1..MAXTYPES] of TType;
  InitializedGlobalData: array [0..MAXINITIALIZEDDATASIZE - 1] of TCharacter;
  Units: array [1..MAXUNITS] of TUnit;
  BlockStack: array [1..MAXBLOCKNESTING] of TBlock;
  WithStack: array [1..MAXWITHNESTING] of TWithDesignator;
  
  MultiplicativeOperators, AdditiveOperators, UnaryOperators, RelationOperators,
  OperatorsForIntegers, OperatorsForReals, OperatorsForBooleans: set of TTokenKind;
  
  IntegerTypes, OrdinalTypes, UnsignedTypes, NumericTypes, StructuredTypes, CastableTypes: set of TTypeKind;

  NumIdent, NumTypes, NumUnits, NumBlocks, BlockStackTop, ForLoopNesting, WithNesting,
  InitializedGlobalDataSize, UninitializedGlobalDataSize: Integer;
  
  IsConsoleProgram: Boolean;
  
  SourceFolder, UnitsFolder: TString;
  


procedure InitializeCommon;
procedure FinalizeCommon;
procedure DisposeParams(var Signature: TSignature);
procedure DisposeFields(var DataType: TType);
function GetTokSpelling(TokKind: TTokenKind): TString;
function GetTypeSpelling(DataType: Integer): TString;
procedure SetErrorProc(Err: TErrorProc);
procedure Error(const Msg: TString);
procedure DefineStaticString(var Tok: TToken; const StrValue: TString);
function LowBound(DataType: Integer): Integer;
function HighBound(DataType: Integer): Integer;
function TypeSize(DataType: Integer): Integer;
function GetCompatibleType(LeftType, RightType: Integer): Integer;
function GetCompatibleRefType(LeftType, RightType: Integer): Integer;
procedure CheckOperator(const Tok: TToken; DataType: Integer);
procedure CheckSignatures(var Signature1, Signature2: TSignature; const Name: TString; CheckParamNames: Boolean = TRUE); 
procedure SetUnitStatus(var NewUnitStatus: TUnitStatus);
function GetUnitUnsafe(const UnitName: TString): Integer;
function GetUnit(const UnitName: TString): Integer;
function GetKeyword(const KeywordName: TString): TTokenKind;
function GetIdentUnsafe(const IdentName: TString; AllowForwardReference: Boolean = FALSE; RecType: Integer = 0): Integer;
function GetIdent(const IdentName: TString; AllowForwardReference: Boolean = FALSE; RecType: Integer = 0): Integer;
function GetFieldUnsafe(RecType: Integer; const FieldName: TString): Integer;
function GetField(RecType: Integer; const FieldName: TString): Integer;
function GetFieldInsideWith(var RecPointer: Integer; var RecType: Integer; const FieldName: TString): Integer;
function GetMethodUnsafe(RecType: Integer; const MethodName: TString): Integer;
function GetMethod(RecType: Integer; const MethodName: TString): Integer;
function GetMethodInsideWith(var RecPointer: Integer; var RecType: Integer; const MethodName: TString): Integer;
function FieldOrMethodInsideWithFound(const Name: TString): Boolean;



implementation


const
  Keyword: array [1..NUMKEYWORDS] of TString = 
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
  ErrorProc: TErrorProc;
  UnitStatus: TUnitStatus;
  
  


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
StructuredTypes  := [ARRAYTYPE, RECORDTYPE, INTERFACETYPE, SETTYPE, FILETYPE];
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
ForLoopNesting              := 0;
WithNesting                 := 0;
InitializedGlobalDataSize   := 0;
UninitializedGlobalDataSize := 0;

IsConsoleProgram            := TRUE;  // Console program by default

SourceFolder                := '';
UnitsFolder                 := ''; 

FillOperatorSets;
FillTypeSets;
end;




procedure FinalizeCommon;
var
  i: Integer;
  
begin
// Dispose of dynamically allocated parameter data
for i := 1 to NumIdent do
  if (Ident[i].Kind = PROC) or (Ident[i].Kind = FUNC) then
    DisposeParams(Ident[i].Signature);

// Dispose of dynamically allocated parameter and field data
for i := 1 to NumTypes do
  begin
  if Types[i].Kind = PROCEDURALTYPE then
    DisposeParams(Types[i].Signature) 
  else if Types[i].Kind in [RECORDTYPE, INTERFACETYPE] then
    DisposeFields(Types[i]);
  end;
end;




procedure DisposeParams(var Signature: TSignature);
var
  i: Integer;
begin
for i := 1 to Signature.NumParams do
  Dispose(Signature.Param[i]);
end; 




procedure DisposeFields(var DataType: TType);
var
  i: Integer;
begin
for i := 1 to DataType.NumFields do
  Dispose(DataType.Field[i]);
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




function GetTypeSpelling(DataType: Integer): TString;
begin
case Types[DataType].Kind of
  EMPTYTYPE:      Result := 'no type';
  ANYTYPE:        Result := 'any type';
  INTEGERTYPE:    Result := 'integer';
  SMALLINTTYPE:   Result := 'small integer';
  SHORTINTTYPE:   Result := 'short integer';
  WORDTYPE:       Result := 'word';
  BYTETYPE:       Result := 'byte';
  CHARTYPE:       Result := 'character';
  BOOLEANTYPE:    Result := 'Boolean';
  REALTYPE:       Result := 'real';
  POINTERTYPE:    begin
                  Result := 'pointer';
                  if Types[Types[DataType].BaseType].Kind <> ANYTYPE then
                    Result := Result + ' to ' + GetTypeSpelling(Types[DataType].BaseType);
                  end;  
  FILETYPE:       begin
                  Result := 'file';
                  if Types[Types[DataType].BaseType].Kind <> ANYTYPE then
                    Result := Result + ' of ' + GetTypeSpelling(Types[DataType].BaseType);
                  end;  
  ARRAYTYPE:      Result := 'array of ' + GetTypeSpelling(Types[DataType].BaseType); 
  RECORDTYPE:     Result := 'record';
  INTERFACETYPE:  Result := 'interface';
  SETTYPE:        Result := 'set of ' + GetTypeSpelling(Types[DataType].BaseType);
  ENUMERATEDTYPE: Result := 'enumeration';
  SUBRANGETYPE:   Result := 'subrange of ' + GetTypeSpelling(Types[DataType].BaseType); 
  PROCEDURALTYPE: Result := 'procedural type';
else
  Result := 'unknown type';
end; //case
end;




procedure SetErrorProc(Err: TErrorProc);
begin
ErrorProc := Err;
end;



  
procedure Error(const Msg: TString);
begin
ErrorProc(Msg);
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
  BOOLEANTYPE:    Result :=  0;
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
  BOOLEANTYPE:    Result := 1;
  SUBRANGETYPE:   Result := Types[DataType].High;
  ENUMERATEDTYPE: Result := Types[DataType].High
else
  Error('Ordinal type expected')
end;// case
end;




function TypeSize(DataType: Integer): Integer;
var
  CurSize: Integer;
  i: Integer;
begin
Result := 0;
case Types[DataType].Kind of
  INTEGERTYPE:              Result := SizeOf(Integer);
  SMALLINTTYPE:             Result := SizeOf(SmallInt);
  SHORTINTTYPE:             Result := SizeOf(ShortInt);
  WORDTYPE:                 Result := SizeOf(Word);
  BYTETYPE:                 Result := SizeOf(Byte);  
  CHARTYPE:                 Result := SizeOf(TCharacter);
  BOOLEANTYPE:              Result := SizeOf(Boolean);
  REALTYPE:                 Result := SizeOf(Single);
  POINTERTYPE:              Result := SizeOf(Pointer);
  FILETYPE:                 Result := SizeOf(TString) + SizeOf(Integer);  // Name + Handle
  SUBRANGETYPE:             Result := SizeOf(Integer);
  ARRAYTYPE:                if Types[DataType].IsOpenArray then
                              Error('Illegal type')
                            else  
                              Result := (HighBound(Types[DataType].IndexType) - LowBound(Types[DataType].IndexType) + 1) * TypeSize(Types[DataType].BaseType);
  RECORDTYPE, INTERFACETYPE:for i := 1 to Types[DataType].NumFields do
                              begin
                              CurSize := Types[DataType].Field[i]^.Offset + TypeSize(Types[DataType].Field[i]^.DataType);
                              if CurSize > Result then Result := CurSize;
                              end;  
  SETTYPE:                  Result := MAXSETELEMENTS div 8;
  ENUMERATEDTYPE:           Result := SizeOf(Byte);                
  PROCEDURALTYPE:           Result := SizeOf(Pointer)               
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
  Error('Incompatible types: ' + GetTypeSpelling(LeftType) + ' and ' + GetTypeSpelling(RightType));  
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
  Error('Incompatible types: ' + GetTypeSpelling(LeftType) + ' and ' + GetTypeSpelling(RightType));  
end;




procedure CheckOperator(const Tok: TToken; DataType: Integer); 
begin
with Types[DataType] do
  if Kind = SUBRANGETYPE then
    CheckOperator(Tok, BaseType)
  else 
    begin
    if not (Kind in OrdinalTypes) and (Kind <> REALTYPE) and (Kind <> POINTERTYPE) and (Kind <> PROCEDURALTYPE) then
      Error('Operator ' + GetTokSpelling(Tok.Kind) + ' is not applicable to ' + GetTypeSpelling(DataType));
     
    if ((Kind in IntegerTypes)  and not (Tok.Kind in OperatorsForIntegers)) or
       ((Kind = REALTYPE)       and not (Tok.Kind in OperatorsForReals)) or   
       ((Kind = CHARTYPE)       and not (Tok.Kind in RelationOperators)) or
       ((Kind = BOOLEANTYPE)    and not (Tok.Kind in OperatorsForBooleans)) or
       ((Kind = POINTERTYPE)    and not (Tok.Kind in RelationOperators)) or
       ((Kind = ENUMERATEDTYPE) and not (Tok.Kind in RelationOperators)) or
       ((Kind = PROCEDURALTYPE) and not (Tok.Kind in RelationOperators)) 
    then
      Error('Operator ' + GetTokSpelling(Tok.Kind) + ' is not applicable to ' + GetTypeSpelling(DataType));
    end;  
end;




procedure CheckSignatures(var Signature1, Signature2: TSignature; const Name: TString; CheckParamNames: Boolean = TRUE);
var
  i: Integer;
begin
if Signature1.NumParams <> Signature2.NumParams then
  Error('Incompatible number of parameters in ' + Name);
  
if Signature1.NumDefaultParams <> Signature2.NumDefaultParams then
  Error('Incompatible number of default parameters in ' + Name);
  
for i := 1 to Signature1.NumParams do
  begin
  if (Signature1.Param[i]^.Name <> Signature2.Param[i]^.Name) and CheckParamNames then
    Error('Incompatible parameter names in ' + Name);
  
  if Signature1.Param[i]^.DataType <> Signature2.Param[i]^.DataType then
    if not Types[Signature1.Param[i]^.DataType].IsOpenArray or not Types[Signature2.Param[i]^.DataType].IsOpenArray or
       (Types[Signature1.Param[i]^.DataType].BaseType <> Types[Signature2.Param[i]^.DataType].BaseType) 
    then 
      Error('Incompatible parameter types in ' + Name + ': ' + GetTypeSpelling(Signature1.Param[i]^.DataType) + ' and ' + GetTypeSpelling(Signature2.Param[i]^.DataType));
    
  if Signature1.Param[i]^.PassMethod <> Signature2.Param[i]^.PassMethod then
    Error('Incompatible CONST/VAR modifiers in ' + Name);

  if Signature1.Param[i]^.Default.Value <> Signature2.Param[i]^.Default.Value then
    Error('Incompatible default values in ' + Name);   
  end; // if

if Signature1.ResultType <> Signature2.ResultType then
  Error('Incompatible result types in ' + Name + ': ' + GetTypeSpelling(Signature1.ResultType) + ' and ' + GetTypeSpelling(Signature2.ResultType));
  
if Signature1.IsStdCall <> Signature2.IsStdCall then
  Error('STDCALL is incompatible with non-STDCALL in ' + Name);

end;




procedure SetUnitStatus(var NewUnitStatus: TUnitStatus);
begin 
UnitStatus := NewUnitStatus;
end;




function GetUnitUnsafe(const UnitName: TString): Integer;
var
  UnitIndex: Integer;
begin
for UnitIndex := 1 to NumUnits do
  if Units[UnitIndex].Name = UnitName then 
    begin
    Result := UnitIndex;
    Exit;
    end;
      
Result := 0;
end;




function GetUnit(const UnitName: TString): Integer;
begin
Result := GetUnitUnsafe(UnitName);
if Result = 0 then
  Error('Unknown unit ' + UnitName);
end;




function GetKeyword(const KeywordName: TString): TTokenKind;
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
  if KeywordName > Keyword[Mid] then
    Min := Mid + 1
  else
    Max := Mid - 1;
  Found := KeywordName = Keyword[Mid];
until Found or (Min > Max);

if Found then Result := TTokenKind(Ord(ANDTOK) - 1 + Mid);
end;




function GetIdentUnsafe(const IdentName: TString; AllowForwardReference: Boolean = FALSE; RecType: Integer = 0): Integer;
var
  IdentIndex: Integer;
begin
for IdentIndex := NumIdent downto 1 do
  with Ident[IdentIndex] do
    if ((UnitIndex = UnitStatus.Index) or (IsExported and (UnitIndex in UnitStatus.UsedUnits))) and       
       (AllowForwardReference or (Kind <> USERTYPE) or (Types[DataType].Kind <> FORWARDTYPE)) and
       (ReceiverType = RecType) and  // Receiver type for methods, 0 otherwise
       (Name = IdentName)
    then 
      begin
      Result := IdentIndex;
      Exit;
      end;          
     
Result := 0;
end;




function GetIdent(const IdentName: TString; AllowForwardReference: Boolean = FALSE; RecType: Integer = 0): Integer;
begin
Result := GetIdentUnsafe(IdentName, AllowForwardReference, RecType);
if Result = 0 then
  Error('Unknown identifier ' + IdentName);
end;




function GetFieldUnsafe(RecType: Integer; const FieldName: TString): Integer;
var
  FieldIndex: Integer;
begin
for FieldIndex := 1 to Types[RecType].NumFields do
  if Types[RecType].Field[FieldIndex]^.Name = FieldName then
    begin
    Result := FieldIndex;
    Exit;
    end;

Result := 0;
end;




function GetField(RecType: Integer; const FieldName: TString): Integer;
begin
Result := GetFieldUnsafe(RecType, FieldName);
if Result = 0 then
  Error('Unknown field ' + FieldName);
end;




function GetFieldInsideWith(var RecPointer: Integer; var RecType: Integer; const FieldName: TString): Integer;
var
  FieldIndex, WithIndex: Integer;
begin 
for WithIndex := WithNesting downto 1 do
  begin
  RecType := WithStack[WithIndex].DataType;
  FieldIndex := GetFieldUnsafe(RecType, FieldName);
  
  if FieldIndex <> 0 then
    begin
    RecPointer := WithStack[WithIndex].TempPointer;
    Result := FieldIndex;
    Exit;
    end;
  end;

Result := 0;  
end;




function GetMethodUnsafe(RecType: Integer; const MethodName: TString): Integer;
begin
Result := GetIdentUnsafe(MethodName, FALSE, RecType);
end;




function GetMethod(RecType: Integer; const MethodName: TString): Integer;
begin
Result := GetIdent(MethodName, FALSE, RecType);
if (Ident[Result].Kind <> PROC) and (Ident[Result].Kind <> FUNC) then
  Error('Method expected');
end;




function GetMethodInsideWith(var RecPointer: Integer; var RecType: Integer; const MethodName: TString): Integer;
var
  MethodIndex, WithIndex: Integer;
begin 
for WithIndex := WithNesting downto 1 do
  begin
  RecType := WithStack[WithIndex].DataType;
  MethodIndex := GetMethodUnsafe(RecType, MethodName);
  
  if MethodIndex <> 0 then
    begin
    RecPointer := WithStack[WithIndex].TempPointer;
    Result := MethodIndex;
    Exit;
    end;
  end;

Result := 0;  
end;




function FieldOrMethodInsideWithFound(const Name: TString): Boolean;
var
  RecPointer: Integer; 
  RecType: Integer;        
begin
Result := (GetFieldInsideWith(RecPointer, RecType, Name) <> 0) or (GetMethodInsideWith(RecPointer, RecType, Name) <> 0);
end;



end.
