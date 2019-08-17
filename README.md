# XD Pascal Compiler for Windows

## Summary
XD Pascal is a small educational self-hosting compiler for a subset of the Pascal language. The software is in the public domain. It comes with absolutely no warranty. Any comments, suggestions, or bug reports are appreciated. Feel free to contact the author on GitHub or by e-mail VTereshkov@mail.ru. Enjoy.

### Features
* Recursive descent parsing
* Native x86 code generation (Windows executables)
* Support for both console and GUI applications
* No external assembler or linker needed
* Source file inclusion facility
* Single-precision floating-point arithmetic (using the x87 FPU)
* Compiler source for Delphi 6/7, Free Pascal and XD Pascal itself 

## Detailed description

### Usage
Type in the command prompt:
```
xdpw <file.pas>
```
The source file should be specified with its extension (.pas).
 
### Language

#### Overview
XD Pascal is a dialect of Pascal programming language similar to Turbo Pascal with the following enhancements, differences and limitations:
* The target operating system is Windows
* The compiler is self-hosting
* Object-oriented programming is not supported
* There are no labels, `goto` and `with` statements
* There is no `Continue` procedure
* There are no unsigned integers, double-precision floating-point numbers, sets, enumerations, variant records
* There are no procedural types, but pointers to procedures can be used for implementing Windows API callbacks
* Open array parameters do not have `High` and `Low` functions. Array length should be explicitly passed to a subroutine 
* Strings are null-terminated arrays of characters (C style). String manipulation routines should be used instead of direct concatenation or comparison
* The only file type is `Text`, which is equivalent to `file`. It can be used for both text and untyped files
* Arrays, strings and records cannot be passed to subroutines by value or used as function results
* The `external` directive is used for Windows API function declarations. It implies the `stdcall` calling convention
* The predefined `Result` variable can be used instead of the function name in assignments (Delphi style)
* Single-line comments (`//`) are supported (Delphi style)

#### Formal grammar
```
Program = "program" Ident ";" Block "." .

Block = { Declarations } CompoundStatement .

Declarations = ConstDeclarations | 
               TypeDeclarations |
               VarDeclarations |
               ProcFuncDeclarations .
             
ConstDeclarations = "const" Ident "=" ConstExpression ";"
                   {Ident "=" ConstExpression ";"} .

TypeDeclarations = "type" Ident "=" Type ";" {Ident "=" Type ";"} .

VarDeclarations = "var" IdentList ":" Type ";" {IdentList ":" Type ";"} .

ProcFuncDeclarations = ("procedure" | "function") Ident [FormalParam] [":" TypeIdent] ";" 
                       (Directive | Block) .

Directive = ("forward" | ("external" StringLiteral "name" StringLiteral)) ";" .         

ActualParam = "(" (Expression | Designator) |
             {"," (Expression | Designator)} ")" .

FormalParam = "(" ["const" | "var"] IdentList ":" ["array" "of"] TypeIdent 
             {";" ["const" | "var"] IdentList ":" ["array" "of"] TypeIdent} ")" .

IdentList = Ident {"," Ident} .

Type = "^" TypeIdent |
       "array" "[" Type {"," Type} "]" "of" Type |
       "record" IdentList ":" Type {";" IdentList ":" Type} [";"] "end" |
       ConstExpression ".." ConstExpression |
       TypeIdent .

Designator = Ident {"^" | ("[" Expression {"," Expression} "]") | ("." Ident)} .

Statement = [ (Designator | Ident) ":=" Expression | 
              Ident [ActualParam] |
              CompoundStatement |
              "if" Expression "then" Statement ["else" Statement] |
              "case" Expression "of" CaseElement {";" CaseElement} 
                    ["else" StatementList] [";"] "end" |
              "while" Expression "do" Statement |
              "repeat" StatementList "until" Expression | 
              "for" Ident ":=" Expression ("to" | "downto") Expression "do"
                    Statement ].

StatementList = Statement {";" Statement} .

CompoundStatement = "begin" StatementList "end" .
 
CaseElement = CaseLabel {"," CaseLabel} ":" Statement .

CaseLabel = ConstExpression [".." ConstExpression] .

ConstExpression = Expression .

Expression = SimpleExpression [("="|"<>"|"<"|"<="|">"|">=") SimpleExpression] .

SimpleExpression = ["+"|"-"] Term {("+"|"-"|"or"|"xor") Term}.

Term = Factor {("*"|"/"|"div"|"mod"|"shl"|"shr"|"and") Factor}.

Factor = Ident [ActualParam] |
         Designator |
         "@" Designator | 
         Number | 
         CharLiteral |
         StringLiteral |  
         "(" Expression ")" | 
         "not" Factor |
         "nil" |
         TypeIdent "(" Expression ")" .

TypeIdent = Ident .

Ident = (Letter | "_") {Letter | "_" | Digit}.

Number = "$" HexDigit {HexDigit} | 
         Digit {Digit} ["." {Digit}] ["e" ["+" | "-"] Digit {Digit}] .

CharLiteral = "'" (Character | "'" "'") "'" | 
              "#" Number .

StringLiteral = "'" {Character | "'" "'"} "'".
```

### Compiler 
The compiler directly builds a Windows PE executable without using any external assembler or linker.

#### Directives
* `$I` - Include source file. Examples: `{$I windows.inc}`, `{$I samples\gauss.inc}`
* `$A` - Set application type. Examples: `{$A GUI}`, `{$A CONSOLE}`

#### Inlined procedures and functions
The following identifiers are implemented as part of the compiler. Their names are not reserved words and can be locally redefined by the user.
```pascal
procedure Inc(var x: Integer)
procedure Dec(var x: Integer)
procedure Read([F: Text;] var x1 {; var xi})
procedure Write([F: Text;] x1 {; xi})
procedure ReadLn([F: Text;] var x1 {; var xi})
procedure WriteLn([F: Text;] x1 {; xi})
procedure New(var P: Pointer)
procedure Dispose(var P: Pointer)
procedure Break
procedure Exit
procedure Halt[(const error: Integer)]
function SizeOf(var x | T): Integer
function Ord(x: T): Integer
function Chr(x: Integer): Char
function Pred(x: T): T
function Succ(x: T): T
function Round(x: Real): Integer 
function Abs(x: T): T
function Sqr(x: T): T
function Sin(x: Real): Real  
function Cos(x: Real): Real  
function Arctan(x: Real): Real  
function Exp(x: Real): Real
function Ln(x: Real): Real
function SqRt(x: Real): Real
```

### System library
```pascal
function Timer: Integer
procedure Randomize
function Random: Real
function Min(x, y: Real): Real
function IMin(x, y: Integer): Integer
function Max(x, y: Real): Real
function IMax(x, y: Integer): Integer
procedure Rewrite(var F: Text; const Name: string)
procedure Reset(var F: Text; const Name: string)
procedure Close(F: Text)
procedure BlockRead(F: Text; Buf: PChar; Len: Integer; var LenRead: Integer)
procedure BlockWrite(F: Text; Buf: PChar; Len: Integer)
procedure Seek(F: Text; Pos: Integer)
function FileSize(F: Text): Integer
function FilePos(F: Text): Integer
function EOF(F: Text): Boolean
function IOResult: Integer
function Length(const s: string): Integer
procedure StrCopy(var Dest: string; const Source: string)
procedure StrCat(var Dest: string; const Source: string)
function StrComp(const s1, s2: string): Integer
procedure CopyMemory(Dest, Source: Pointer; Len: LongInt)
function ParseCmdLine(Index: Integer; var Str: string): Integer
procedure Val(const s: string; var Number: Real; var Code: Integer)
procedure Str(Number: Real; var s: string)
procedure IVal(const s: string; var Number: Integer; var Code: Integer)
procedure IStr(Number: Integer; var s: string)
function UpCase(ch: Char): Char
```

### Samples
* `FACTOR.PAS`   - Integer factorization demo
* `LINEQ.PAS`    - Linear algebraic equation systems solver. Uses `GAUSS.INC` unit. Requires `EQ.TXT`, `EQERR.TXT`, or similar data file
* `LIFE.PAS`     - The Game of Life
* `SORT.PAS`     - Array sorting demo
* `FFT.PAS`      - Fast Fourier Transform
* `INSERR.PAS`   - Inertial navigation system error estimator. Uses `KALMAN.INC` unit
* `LIST.PAS`     - Linked list operations demo
* `GUI.PAS`      - GUI application demo. Uses `WINDOWS.INC` unit

