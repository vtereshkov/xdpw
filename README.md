<img src="logo.png">

# XD Pascal for Windows

_Dedicated to my father Mikhail Tereshkov, who instilled in me a taste for engineering_ 

## Summary
XD Pascal is a small educational self-hosting compiler for a Pascal language dialect. Any comments, suggestions, or bug reports are appreciated. Feel free to contact the author on GitHub or by e-mail VTereshkov@mail.ru. Enjoy.

### Features
__* Go-style methods and interfaces__
* Native x86 code generation (32 bit Windows executables)
* Support for both console and GUI applications
* No external assembler or linker needed
* Floating-point arithmetic using the x87 FPU
* Compiler source for Delphi 6/7, Free Pascal and XD Pascal itself 

## Detailed description

### Usage
Type in the command prompt:
```
xdpw <unit1.pas> <unit2.pas> ... <prog.pas>
```
The source file should be specified with its extension (.pas).
 
### Language

XD Pascal is similar to Turbo Pascal with the following changes:

#### Enhancements
* The target operating system is Windows
* The compiler is self-hosting
* Methods and interfaces (Go style) are supported
* Functions can return arrays, records or sets (Delphi style)
* Functions can be called as procedures (Delphi style)
* Parameters can have default values (Delphi style)
* The predefined `Result` variable can be used instead of the function name in assignments (Delphi style)
* Single-line comments (`//`) are supported (Delphi style)

#### Differences
* Strings are null-terminated arrays of characters (C style), but indexed from 1 for Pascal compatibility
* The `Text` type is equivalent to `file`. It can be used for both text and untyped files
* Calls via procedural variables require parentheses even for empty parameter lists

#### Limitations
* No "classical" object-oriented programming
* Units cannot be compiled separately
* No `Double` and `Extended` types
* Arrays, records and sets cannot serve as untyped constants. Typed constants should be used instead
* Arrays, records and sets cannot be passed to subroutines without `const` or `var`
* No `High` and `Low` functions for open arrays. Open array length should be explicitly passed to a subroutine 
* Statement labels cannot be numerical

#### Formal grammar
```
ProgramOrUnit = ("program" | "unit") Ident ";" 
                ["interface"] [UsesClause] Block "." .
                
UsesClause = "uses" Ident {"," Ident} ";" .                

Block = { Declarations } (CompoundStatement | "end") .

Declarations = DeclarationSection ["implementation" DeclarationSection] .

DeclarationSection = LabelDeclarations |
                     ConstDeclarations | 
                     TypeDeclarations |
                     VarDeclarations |
                     ProcFuncDeclarations .
               
LabelDeclarations = "label" Ident {"," Ident} ";"               
             
ConstDeclarations = (UntypedConstDeclaration | TypedConstDeclaration)
               {";" (UntypedConstDeclaration | TypedConstDeclaration)} .

UntypedConstDeclaration = "const" Ident "=" ConstExpression .
                                 
TypedConstDeclaration = "const" Ident ":" Type "=" TypedConstConstr .

TypedConstConstr = ConstExpression |
                   StringLiteral |
                   "(" TypedConstConstr {"," TypedConstConstr} ")" |
                   "(" Ident ":" TypedConstConstr {";" Ident ":" TypedConstConstr} ")" |
                   SetConstructor .

TypeDeclarations = "type" Ident "=" Type ";" {Ident "=" Type ";"} .

VarDeclarations = "var" IdentList ":" Type ";" {IdentList ":" Type ";"} .

ProcFuncDeclarations = ("procedure" | "function") Ident 
                       [Receiver] [FormalParams] [":" TypeIdent] 
                       ["stdcall"] ";" [(Directive | Block) ";"] .

Receiver = "for" Ident ":" TypeIdent .

Directive = "forward" | ("external" StringLiteral "name" StringLiteral) .         

ActualParams = "(" [ (Expression | Designator) |
              {"," (Expression | Designator)} ] ")" .

FormalParams = "(" FormalParamList {";" FormalParamList} ")" .
              
FormalParamList = ["const" | "var"] IdentList [":" ["array" "of"] TypeIdent] 
                                              ["=" ConstExpression] .             

IdentList = Ident {"," Ident} .

Type = "(" Ident {"," Ident} ")" |
       "^" TypeIdent |
       ["packed"] "array" "[" Type {"," Type} "]" "of" Type |
       ["packed"] "record" Fields 
          ["case" Ident ":" Type "of" 
               ConstExpression {"," ConstExpression} ":" "(" Fields ")"
          {";" ConstExpression {"," ConstExpression} ":" "(" Fields ")"}] [";"] "end" |
       ["packed"] "interface" Fields [";"] "end" |
       ["packed"] "set" "of" Type |
       ["packed"] "file" ["of" Type] |
       ConstExpression ".." ConstExpression |
       ("procedure" | "function") [FormalParams] [":" TypeIdent] ["stdcall"] |
       Ident .
       
Fields = IdentList ":" Type {";" IdentList ":" Type} .       
       
TypeIdent = "string" | "file" | Ident .       

Designator = Ident {"^" | 
                    "[" Expression {"," Expression} "]" | 
                    "." Ident [ActualParams] | .

Statement = [Label ":"] [ (Designator | Ident) ":=" Expression | 
                          (Designator | Ident) [ActualParams] |
                          CompoundStatement |
                          IfStatement |
                          CaseStatement |
                          WhileStatement |
                          RepeatStatement | 
                          ForStatement |
                          GotoStatement |
                          WithStatement ] .
                          
Label = Ident .                          

StatementList = Statement {";" Statement} .

CompoundStatement = "begin" StatementList "end" .

IfStatement = "if" Expression "then" Statement ["else" Statement] .

CaseStatement = "case" Expression "of" CaseElement {";" CaseElement} 
                    ["else" StatementList] [";"] "end" .
                    
WhileStatement = "while" Expression "do" Statement .

RepeatStatement = "repeat" StatementList "until" Expression .

ForStatement = "for" Ident ":=" Expression ("to" | "downto") Expression "do" Statement.

GotoStatement = "goto" Label .

WithStatement = "with" Designator {"," Designator} "do" Statement .                    
 
CaseElement = CaseLabel {"," CaseLabel} ":" Statement .

CaseLabel = ConstExpression [".." ConstExpression] .

ConstExpression = Expression .

Expression = SimpleExpression [("="|"<>"|"<"|"<="|">"|">="|"in") SimpleExpression] .

SimpleExpression = ["+"|"-"] Term {("+"|"-"|"or"|"xor") Term}.

Term = Factor {("*"|"/"|"div"|"mod"|"shl"|"shr"|"and") Factor}.

Factor = (Designator | Ident) [ActualParams] |
         Designator |
         "@" Designator | 
         Number | 
         CharLiteral |
         StringLiteral |  
         "(" Expression ")" | 
         "not" Factor |
         SetConstructor |
         "nil" |
         Ident "(" Expression ")" .
         
SetConstructor = "[" [Expression [".." Expression] 
                     {"," Expression [".." Expression]}] "]" .         

Ident = (Letter | "_") {Letter | "_" | Digit}.

Number = "$" HexDigit {HexDigit} | 
         Digit {Digit} ["." {Digit}] ["e" ["+" | "-"] Digit {Digit}] .

CharLiteral = "'" (Character | "'" "'") "'" | 
              "#" Number .

StringLiteral = "'" {Character | "'" "'"} "'".
```

### Compiler 
The compiler is based on a recursive descent parser. It directly builds a Windows PE executable without using any external assembler or linker.

#### Directives
* `$APPTYPE` - Set application type. Examples: `{$APPTYPE GUI}`, `{$APPTYPE CONSOLE}`

#### Optimizations
Some simple peephole optimizations are performed:
* Push/pop elimination
* FPU push/pop elimination
* Local variable loading optimizations (32-bit variables only)
* Array element access optimizations
* Record field access optimizations
* Assignment optimizations
* Comparison optimizations
* Condition testing optimizations

#### Inlined procedures and functions
The following identifiers are implemented as part of the compiler. Their names are not reserved words and can be locally redefined by the user.
```pascal
procedure Inc(var x: Integer);
procedure Dec(var x: Integer);
procedure Read([var F: file;] var x1 {; var xi});
procedure Write([var F: file;] x1[:w[:d]] {; xi[:w[:d]]});
procedure ReadLn([var F: file;] var x1 {; var xi});
procedure WriteLn([var F: file;] x1[:w[:d]] {; xi[:w[:d]]});
procedure New(var P: Pointer);
procedure Dispose(var P: Pointer);
procedure Break;
procedure Continue;
procedure Exit;
procedure Halt[(const error: Integer)];
function SizeOf(var x | T): Integer;
function Ord(x: T): Integer;
function Chr(x: Integer): Char;
function Pred(x: T): T;
function Succ(x: T): T;
function Round(x: Real): Integer;
function Abs(x: T): T;
function Sqr(x: T): T;
function Sin(x: Real): Real;  
function Cos(x: Real): Real;  
function Arctan(x: Real): Real;  
function Exp(x: Real): Real;
function Ln(x: Real): Real;
function SqRt(x: Real): Real;
```

### System library
```pascal
function Timer: Integer;
procedure Randomize;
function Random: Real;
procedure Assign(var F: file; const Name: string);
procedure Rewrite(var F: file[; BlockSize: Integer]);
procedure Reset(var F: file[; BlockSize: Integer]);
procedure Close(var F: file);
procedure BlockRead(var F: file; var Buf; Len: Integer; var LenRead: Integer);
procedure BlockWrite(var F: file; var Buf; Len: Integer);
procedure Seek(var F: file; Pos: Integer);
function FileSize(var F: file): Integer;
function FilePos(var F: file): Integer;
function EOF(var F: file): Boolean;
function IOResult: Integer;
function Length(const s: string): Integer;
procedure AppendStr(var Dest: string; const Source: string);
function CompareStr(const s1, s2: string): Integer;
procedure Move(const Source; var Dest; Count: Integer);
procedure FillChar(var Data; Count: Integer; Value: Char);
function ParamCount: Integer;
function ParamStr(Index: Integer): string;
procedure Val(const s: string; var Number: Real; var Code: Integer);
procedure Str(Number: Real; var s: string[; DecPlaces: Integer]);
procedure IVal(const s: string; var Number: Integer; var Code: Integer);
procedure IStr(Number: Integer; var s: string);
function UpCase(ch: Char): Char;
```

### Samples
* `factor.pas`   - Integer factorization demo
* `lineq.pas`    - Linear equation solver. Uses `gauss.pas` unit. Requires `eq.txt`, `eqerr.txt`, or similar data file
* `life.pas`     - The Game of Life
* `sort.pas`     - Array sorting demo
* `fft.pas`      - Fast Fourier Transform demo
* `inserr.pas`   - Inertial navigation system error estimation demo. Uses `kalman.pas` unit
* `list.pas`     - Linked list operations demo
* `gui.pas`      - GUI application demo. Uses `windows.pas` unit
* `raytracer.pas`- Raytracer demo. Demonstrates XD Pascal methods and interfaces

<img src="scene.png">

### Known issues

The Windows Defender is known to give false positive results on some programs compiled with XD Pascal.
