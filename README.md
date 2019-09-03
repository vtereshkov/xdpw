<img src="logo.png">

# XD Pascal Compiler for Windows

## Summary
XD Pascal is a small educational self-hosting compiler for a subset of the Pascal language. Any comments, suggestions, or bug reports are appreciated. Feel free to contact the author on GitHub or by e-mail VTereshkov@mail.ru. Enjoy.

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
* There are no units. Source file inclusion directives should be used instead 
* There are no labels, `goto` and `with` statements
* There are no unsigned integers, double-precision floating-point numbers, sets, enumerations, and variant records
* There are no typed constants
* Open array parameters do not have `High` and `Low` functions. Array length should be explicitly passed to a subroutine 
* Strings are null-terminated arrays of characters (C style). String manipulation routines should be used instead of direct concatenation or comparison
* The only file type is `Text`, which is equivalent to `file`. It can be used for both text and untyped files
* Arrays, strings and records cannot be passed to subroutines without `const` or `var`, or used as function results
* Calls via procedural variables require parentheses even for empty parameter lists
* The `external` directive is used for Windows API function declarations. It implies the `stdcall` calling convention
* The predefined `Result` variable can be used instead of the function name in assignments (Delphi style)
* Functions can be called as procedures (Delphi style)
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

ProcFuncDeclarations = ("procedure" | "function") Ident [FormalParams] [":" TypeIdent] ";" 
                       (Directive | Block) .

Directive = ("forward" | ("external" StringLiteral "name" StringLiteral)) ";" .         

ActualParams = "(" [ (Expression | Designator) |
              {"," (Expression | Designator)} ] ")" .

FormalParams = "(" FormalParamList {";" FormalParamList} ")" .
              
FormalParamList = ["const" | "var"] IdentList [":" ["array" "of"] TypeIdent] .             

IdentList = Ident {"," Ident} .

Type = "^" TypeIdent |
       "array" "[" Type {"," Type} "]" "of" Type |
       "record" IdentList ":" Type {";" IdentList ":" Type} [";"] "end" |
       ConstExpression ".." ConstExpression |
       ("procedure" | "function") [FormalParams] [":" TypeIdent] |
       Ident .
       
TypeIdent = "string" | "file" | Ident .       

Designator = Ident {"^" | ("[" Expression {"," Expression} "]") | ("." Ident)} .

Statement = [ (Designator | Ident) ":=" Expression | 
              (Designator | Ident) [ActualParams] |
              CompoundStatement |
              IfStatement |
              CaseStatement |
              WhileStatement |
              RepeatStatement | 
              ForStatement ] .

StatementList = Statement {";" Statement} .

CompoundStatement = "begin" StatementList "end" .

IfStatement = "if" Expression "then" Statement ["else" Statement] .

CaseStatement = "case" Expression "of" CaseElement {";" CaseElement} 
                    ["else" StatementList] [";"] "end" .
                    
WhileStatement = "while" Expression "do" Statement .

RepeatStatement = "repeat" StatementList "until" Expression .

ForStatement = "for" Ident ":=" Expression ("to" | "downto") Expression "do" Statement.                    
 
CaseElement = CaseLabel {"," CaseLabel} ":" Statement .

CaseLabel = ConstExpression [".." ConstExpression] .

ConstExpression = Expression .

Expression = SimpleExpression [("="|"<>"|"<"|"<="|">"|">=") SimpleExpression] .

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
         "nil" |
         Ident "(" Expression ")" .

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
procedure Inc(var x: Integer);
procedure Dec(var x: Integer);
procedure Read([F: Text;] var x1 {; var xi});
procedure Write([F: Text;] x1 {; xi});
procedure ReadLn([F: Text;] var x1 {; var xi});
procedure WriteLn([F: Text;] x1 {; xi});
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
function Min(x, y: Real): Real;
function IMin(x, y: Integer): Integer;
function Max(x, y: Real): Real;
function IMax(x, y: Integer): Integer;
procedure Rewrite(var F: file; const Name: string);
procedure Reset(var F: file; const Name: string);
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
function ParseCmdLine(Index: Integer; var Str: string): Integer;
procedure Val(const s: string; var Number: Real; var Code: Integer);
procedure Str(Number: Real; var s: string);
procedure IVal(const s: string; var Number: Integer; var Code: Integer);
procedure IStr(Number: Integer; var s: string);
function UpCase(ch: Char): Char;
```

### Samples
* `factor.pas`   - Integer factorization demo
* `lineq.pas`    - Linear algebraic equation systems solver. Uses `gauss.inc` unit. Requires `eq.txt`, `eqerr.txt`, or similar data file
* `life.pas`     - The Game of Life
* `sort.pas`     - Array sorting demo
* `fft.pas`      - Fast Fourier Transform demo
* `inserr.pas`   - Inertial navigation system error estimation demo. Uses `kalman.inc` unit
* `list.pas`     - Linked list operations demo
* `gui.pas`      - GUI application demo. Uses `windows.inc` unit

### Known issues

The AVG antivirus gives false positive results on some programs compiled with XD Pascal.
