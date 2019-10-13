// XD Pascal - a 32-bit compiler for Windows
// Copyright (c) 2009-2010, 2019, Vasiliy Tereshkov

// Code generator



type
  TRegister = (EMPTYREG, EAX, ECX, ESI, EDI, EBP);
   
  TInstructionKind = (EMPTYOP, PUSHREGOP, PUSHCONSTOP);
  
  TInstruction = record
    Kind: TInstructionKind;
    Reg: TRegister;
    Value: LongInt;
  end;
  
  
  
  
const
  OptimizedInstruction: TInstruction = (Kind: EMPTYOP; Reg: EMPTYREG; Value: 0);
  


  
procedure Gen(b: Byte);
begin
if CodeSize >= MAXCODESIZE then
  Error('Maximum code size exceeded');
Code[CodeSize] := b;
Inc(CodeSize);

// If an instruction has not been optimized immediately, it will not be optimized at all
OptimizedInstruction.Kind := EMPTYOP;
end;




procedure GenAt(Pos: LongInt; b: Byte);
begin
Code[Pos] := b;
end;




procedure GenWord(w: Integer);
var
  i: Integer;
begin
for i := 1 to 2 do
  begin
  Gen(Byte(w and $FF));
  w := w shr 8;
  end;
end;




procedure GenWordAt(Pos: LongInt; w: Integer);
var
  i: Integer;
begin
for i := 0 to 1 do
  begin
  GenAt(Pos + i, Byte(w and $FF));
  w := w shr 8;
  end;
end;




procedure GenDWord(dw: LongInt);
var
  i: Integer;
begin
for i := 1 to 4 do
  begin
  Gen(Byte(dw and $FF));
  dw := dw shr 8;
  end;
end;




procedure GenDWordAt(Pos: LongInt; dw: LongInt);
var
  i: Integer;
begin
for i := 0 to 3 do
  begin
  GenAt(Pos + i, Byte(dw and $FF));
  dw := dw shr 8;
  end;
end;




procedure GenRelocDWord(dw: LongInt; RelocType: TRelocType);
begin
Inc(NumRelocs);
if NumRelocs > MAXRELOCS then
  Error('Maximum number of relocations exceeded');  
Reloc[NumRelocs].RelocType := RelocType;
Reloc[NumRelocs].Pos := GetCodeSize;
Reloc[NumRelocs].Value := dw;

GenDWord(dw);
end;




procedure PushConst(Value: LongInt);
begin
Gen($68); GenDWord(Value);                            // push Value

OptimizedInstruction.Kind := PUSHCONSTOP;
OptimizedInstruction.Value := Value;
end;




procedure PushRelocConst(Value: LongInt; RelocType: TRelocType);
begin
Gen($68); GenRelocDWord(Value, RelocType);            // push Value  ; relocatable
end;




procedure Relocate(CodeDeltaAddr, InitDataDeltaAddr, UninitDataDeltaAddr, ImportDeltaAddr: Integer);
var
  i, DeltaAddr: Integer;
begin
DeltaAddr := 0;

for i := 1 to NumRelocs do
  begin
  case Reloc[i].RelocType of
    CODERELOC:        DeltaAddr := CodeDeltaAddr;
    INITDATARELOC:    DeltaAddr := InitDataDeltaAddr;
    UNINITDATARELOC:  DeltaAddr := UninitDataDeltaAddr;
    IMPORTRELOC:      DeltaAddr := ImportDeltaAddr
  else 
    Error('Internal fault: Illegal relocation type');
  end;  
  
  GenDWordAt(Reloc[i].Pos, Reloc[i].Value + DeltaAddr);
  end;
end;




function OptimizePop(Reg: TRegister): Boolean;
begin
Result := FALSE;

if OptimizedInstruction.Kind = PUSHREGOP then
  begin
  if Reg = OptimizedInstruction.Reg then                            // Meaningless 'push Reg / pop Reg' - remove 'push Reg' from the stack  
    begin
    Dec(CodeSize);
    Result := TRUE;
    end
                                         
  else if (Reg = ECX) and (OptimizedInstruction.Reg = EAX) then     // Special case - replace push eax / pop ecx
    begin
    Dec(CodeSize);                                       
    Gen($89); Gen($C1);                                             // mov ecx, eax
    Result := TRUE;
    end
    
  else if (Reg = ESI) and (OptimizedInstruction.Reg = EAX) then     // Special case - replace push eax / pop esi
    begin
    Dec(CodeSize);                                       
    Gen($89); Gen($C6);                                             // mov esi, eax
    Result := TRUE;
    end
  end  

else if OptimizedInstruction.Kind = PUSHCONSTOP then
  begin  
  if Reg = EAX then                                                 // Special case - replace push Value / pop eax
    begin
    CodeSize := CodeSize - 5;                                       
    Gen($B8); GenDWord(OptimizedInstruction.Value);                 // mov eax, Value
    Result := TRUE;
    end
    
  else if Reg = ECX then                                            // Special case - replace push Value / pop ecx
    begin
    CodeSize := CodeSize - 5;                                       
    Gen($B9); GenDWord(OptimizedInstruction.Value);                 // mov ecx, Value
    Result := TRUE;
    end
  end
  
end;




procedure GenPushReg(Reg: TRegister);
begin
case Reg of
  EAX: Gen($50);            // push eax
  ECX: Gen($51);            // push ecx
  ESI: Gen($56);            // push esi
  EDI: Gen($57);            // push edi
  EBP: Gen($55)             // push ebp
else
  Error('Internal fault: Illegal register');  
end;

OptimizedInstruction.Kind := PUSHREGOP;
OptimizedInstruction.Reg := Reg;
end;  




procedure GenPopReg(Reg: TRegister);
begin
if not OptimizePop(Reg) then
  case Reg of
    EAX: Gen($58);            // pop eax
    ECX: Gen($59);            // pop ecx
    ESI: Gen($5E);            // pop esi
    EDI: Gen($5F);            // pop edi
    EBP: Gen($5D)             // pop ebp
  else
    Error('Internal fault: Illegal register');  
  end;
  
OptimizedInstruction.Kind := EMPTYOP;  
end;  




procedure PushVarPtr(Addr: Integer; Scope: TScope; DeltaNesting: Byte; RelocType: TRelocType);
const
  StaticLinkAddr = 2 * 4;
var
  i: Integer;  
begin
case Scope of
  GLOBAL:                                     // Global variable
    PushRelocConst(Addr, RelocType);
  LOCAL:
    begin
    if DeltaNesting = 0 then                  // Strictly local variable
      begin
      Gen($8D); Gen($B5); GenDWord(Addr);                       // lea esi, [ebp + Addr]
      end
    else                                      // Intermediate level variable
      begin
      Gen($8B); Gen($75); Gen(StaticLinkAddr);                  // mov esi, [ebp + StaticLinkAddr]
      for i := 1 to DeltaNesting - 1 do
        begin
        Gen($8B); Gen($76); Gen(StaticLinkAddr);                // mov esi, [esi + StaticLinkAddr]
        end;
      Gen($8D); Gen($B6); GenDWord(Addr);                       // lea esi, [esi + Addr]
      end;
    GenPushReg(ESI);                                            // push esi
    end;// if
end;// case
end;




procedure DerefPtr(DataType: Integer);
begin
GenPopReg(ESI);                                                   // pop esi

case TypeSize(DataType) of

  1: if Types[DataType].Kind in UnsignedTypes then
       begin
       Gen($31); Gen($C0);                                        // xor eax, eax
       Gen($8A); Gen($06);                                        // mov al, [esi]
       end
     else  
       begin
       Gen($8A); Gen($06);                                        // mov al, [esi]
       Gen($66); Gen($98);                                        // cbw
       Gen($98);                                                  // cwde
       end;
       
  2: if Types[DataType].Kind in UnsignedTypes then
       begin
       Gen($31); Gen($C0);                                        // xor eax, eax
       Gen($66); Gen($8B); Gen($06);                              // mov ax, [esi]
       end
     else  
       begin
       Gen($66); Gen($8B); Gen($06);                              // mov ax, [esi]
       Gen($98);                                                  // cwde
       end;
     
  4: begin
     Gen($8B); Gen($06);                                          // mov eax, [esi]
     end
     
else
  Error('Internal fault: Illegal designator size');
end;

GenPushReg(EAX);                                                  // push eax
end;




procedure GetArrayElementPtr(ArrType: Integer);

  function Log2(x: LongInt): ShortInt;
  var
    i: Integer;
  begin
  Result := -1;
  i := 0;
  while (i <= 31) and (Result = -1) do
    begin
    if x = 1 shl i then Result := i;
    Inc(i);
    end;
  end;

var
  BaseTypeSize, IndexLowBound: Integer;
  Log2BaseTypeSize: ShortInt;

begin
GenPopReg(EAX);                                                 // pop eax           ; Array index

IndexLowBound := LowBound(Types[ArrType].IndexType);
if IndexLowBound = 1 then
  Gen($48)                                                      // dec eax
else if IndexLowBound <> 0 then
  begin
  Gen($2D); GenDWord(IndexLowBound);                            // sub eax, IndexLowBound
  end;

BaseTypeSize := TypeSize(Types[ArrType].BaseType);
Log2BaseTypeSize := Log2(BaseTypeSize);

if Log2BaseTypeSize > 0 then
  begin
  Gen($C1); Gen($E0); Gen(Log2BaseTypeSize);                    // shl eax, Log2BaseTypeSize
  end
else if Log2BaseTypeSize < 0 then
  begin
  Gen($69); Gen($C0); GenDWord(BaseTypeSize);                   // imul eax, BaseTypeSize
  end;

GenPopReg(ESI);                                                 // pop esi            ; Array base offset
Gen($01); Gen($C6);                                             // add esi, eax
GenPushReg(ESI);                                                // push esi
end;




procedure GetFieldPtr(RecType: Integer; FieldIndex: Integer);
var
  Offset: Integer;
begin
Offset := Types[RecType].Field[FieldIndex]^.Offset;
if Offset > 0 then
  begin
  GenPopReg(ESI);                                                 // pop esi
  Gen($81); Gen($C6); GenDWord(Offset);                           // add esi, Offset
  GenPushReg(ESI);                                                // push esi
  end;
end;




procedure GetCharAsTempString;
begin
GenPopReg(ESI);                                                   // pop esi                  ; Temporary string address
GenPopReg(EAX);                                                   // pop eax                  ; Character
Gen($88); Gen($06);                                               // mov byte ptr [esi], al
Gen($C6); Gen($46); Gen($01); Gen($00);                           // mov byte ptr [esi + 1], 0
GenPushReg(ESI);                                                  // push esi
end;




procedure SaveStackTopToEAX;
begin
GenPopReg(EAX);                                                    // pop eax
end;




procedure RestoreStackTopFromEAX;
begin
GenPushReg(EAX);                                                   // push eax
end;




procedure DiscardStackTop(NumItems: Byte);
begin
Gen($83); Gen($C4); Gen(SizeOf(LongInt) * NumItems);                                    // add esp, 4 * NumItems
end;




procedure DiscardStackTopAt(Pos: LongInt; NumItems: Byte);
begin
GenAt(Pos, $83); GenAt(Pos + 1, $C4); GenAt(Pos + 2, SizeOf(LongInt) * NumItems);       // add esp, 4 * NumItems
end;




procedure DuplicateStackTop;
begin
Gen($FF); Gen($34); Gen($24);                                                           // push dword ptr [esp]
end;




function GetCodeSize{: LongInt};
begin
Result := CodeSize;

// If code size is requested, disable the last instruction optimization
OptimizedInstruction.Kind := EMPTYOP;
end;




procedure SaveCodePos;
begin
Inc(CodePosStackTop);
CodePosStack[CodePosStackTop] := GetCodeSize;
end;




function RestoreCodePos: LongInt;
begin
Result := CodePosStack[CodePosStackTop];
Dec(CodePosStackTop);
end;




procedure GenerateIncDec(proc: TPredefProc; Size: Byte);
begin
GenPopReg(ESI);                                                       // pop esi

case Size of
  1: begin
     Gen($FE);                                                        // ... byte ptr ...
     end;
  2: begin
     Gen($66); Gen($FF);                                              // ... word ptr ...
     end;
  4: begin
     Gen($FF);                                                        // ... dword ptr ...
     end;
  end;

case proc of
  INCPROC: Gen($06);                                                  // inc ... [esi]
  DECPROC: Gen($0E);                                                  // dec ... [esi]
  end;
end;




procedure GenerateRound(TruncMode: Boolean);
begin
Gen($89); Gen($E3);                                                   // mov ebx, esp
Gen($D9); Gen($03);                                                   // fld dword ptr [ebx]   ;  st := operand
if TruncMode then
  begin
  Gen($66); Gen($C7); Gen($43); Gen(Byte(-4)); GenWord($0F7F);        // mov word ptr [ebx - 4], 0F7Fh
  Gen($D9); Gen($6B); Gen(Byte(-4));                                  // fldcw word ptr [ebx - 4]
  end;
Gen($DB); Gen($1B);                                                   // fistp dword ptr [ebx] ;  [ebx] := round(result);  pop
if TruncMode then
  begin
  Gen($66); Gen($C7); Gen($43); Gen(Byte(-4)); GenWord($037F);        // mov word ptr [ebx - 4], 037Fh
  Gen($D9); Gen($6B); Gen(Byte(-4));                                  // fldcw word ptr [ebx - 4]
  end;
end;// GenerateRound




procedure GenerateFloat(Depth: Byte);
begin
Gen($89); Gen($E3);                                                   // mov ebx, esp

if Depth > 0 then
  begin
  Gen($83); Gen($C3); Gen(Depth);                                     // add ebx, Depth
  end;

Gen($DB); Gen($03);                                                   // fild dword ptr [ebx]  ;  st := float(operand)
Gen($D9); Gen($1B);                                                   // fstp dword ptr [ebx]  ;  [ebx] := result;  pop
end;// GenerateFloat




procedure GenerateMathFunction(func: TPredefProc; ResultType: Integer);
begin
if Types[ResultType].Kind = REALTYPE then       // Real type
  begin
  Gen($89); Gen($E3);                                                   // mov ebx, esp
  Gen($D9); Gen($03);                                                   // fld dword ptr [ebx]  ; st = x
  case func of
    ABSFUNC:
      begin
      Gen($D9); Gen($E1);                                               // fabs
      end;
    SQRFUNC:
      begin
      Gen($DC); Gen($C8);                                               // fmul st, st
      end;
    SINFUNC:
      begin
      Gen($D9); Gen($FE);                                               // fsin
      end;
    COSFUNC:
      begin
      Gen($D9); Gen($FF);                                               // fcos
      end;
    ARCTANFUNC:
      begin
      Gen($D9); Gen($E8);                                               // fld1
      Gen($D9); Gen($F3);                                               // fpatan    ; st := arctan(x / 1.0)
      end;
    EXPFUNC:
      begin
      Gen($D9); Gen($EA);                                               // fldl2e
      Gen($DE); Gen($C9);                                               // fmul
      Gen($D9); Gen($C0);                                               // fld st
      Gen($D9); Gen($FC);                                               // frndint
      Gen($DD); Gen($D2);                                               // fst st(2) ; st(2) := round(x * log2(e))
      Gen($DE); Gen($E9);                                               // fsub
      Gen($D9); Gen($F0);                                               // f2xm1     ; st := 2 ^ frac(x * log2(e)) - 1
      Gen($D9); Gen($E8);                                               // fld1
      Gen($DE); Gen($C1);                                               // fadd
      Gen($D9); Gen($FD);                                               // fscale    ; st := 2 ^ frac(x * log2(e)) * 2 ^ round(x * log2(e)) = exp(x)
      end;
    LNFUNC:
      begin
      Gen($D9); Gen($ED);                                               // fldln2
      Gen($D9); Gen($C9);                                               // fxch
      Gen($D9); Gen($F1);                                               // fyl2x     ; st := ln(2) * log2(x) = ln(x)
      end;
    SQRTFUNC:
      begin
      Gen($D9); Gen($FA);                                               // fsqrt
      end;

  end;// case

  Gen($D9); Gen($1B);                                                   // fstp dword ptr [ebx]  ;  [ebx] := result;  pop
  end
else                                // Ordinal types
  case func of
    ABSFUNC:
      begin
      GenPopReg(EAX);                                                   // pop eax
      Gen($83); Gen($F8); Gen($00);                                     // cmp eax, 0
      Gen($7D); Gen($02);                                               // jge +2
      Gen($F7); Gen($D8);                                               // neg eax
      GenPushReg(EAX);                                                  // push eax
      end;
    SQRFUNC:
      begin
      GenPopReg(EAX);                                                   // pop eax
      Gen($F7); Gen($E8);                                               // imul eax
      GenPushReg(EAX);                                                  // push eax
      end;
  end;// case
end;// GenerateMathFunction





procedure GenerateUnaryOperator(op: TTokenKind; ResultType: Integer);
begin
if Types[ResultType].Kind = REALTYPE then     // Real type
  begin
  if op = MINUSTOK then
    begin
    Gen($89); Gen($E3);                                                 // mov ebx, esp
    Gen($D9); Gen($03);                                                 // fld dword ptr [ebx]  ; st = x
    Gen($D9); Gen($E0);                                                 // fchs
    Gen($D9); Gen($1B);                                                 // fstp dword ptr [ebx]  ;  [ebx] := result;  pop
    end;
  end
else                                              // Ordinal types
  begin
  GenPopReg(EAX);                                                       // pop eax
  case op of
    MINUSTOK:
      begin
      Gen($F7); Gen($D8);                                               // neg eax
      end;
    NOTTOK:
      begin
      Gen($F7); Gen($D0);                                               // not eax
      end;
  end;// case
  
  if Types[ResultType].Kind = BOOLEANTYPE then
    begin
    Gen($83); Gen($E0); Gen($01);                                       // and eax, 1
    end;
    
  GenPushReg(EAX);                                                      // push eax
  end;// else
  
end;




procedure GenerateBinaryOperator(op: TTokenKind; ResultType: Integer);
begin
if Types[ResultType].Kind = REALTYPE then     // Real type
  begin
  Gen($89); Gen($E3);                                                   // mov ebx, esp
  Gen($D9); Gen($03);                                                   // fld dword ptr [ebx]  ; st = operand2
  GenPopReg(EAX);                                                       // pop eax
  Gen($89); Gen($E3);                                                   // mov ebx, esp
  Gen($D9); Gen($03);                                                   // fld dword ptr [ebx]  ; st(1) = operand2;  st = operand1
  Gen($D9); Gen($C9);                                                   // fxch                 ; st = operand2;  st(1) = operand1

  case op of
    PLUSTOK:
      begin
      Gen($DE); Gen($C1);                                               // fadd  ;  st(1) := st(1) + st;  pop
      end;
    MINUSTOK:
      begin
      Gen($DE); Gen($E9);                                               // fsub  ;  st(1) := st(1) - st;  pop
      end;
    MULTOK:
      begin
      Gen($DE); Gen($C9);                                               // fmul  ;  st(1) := st(1) * st;  pop
      end;
    DIVTOK:
      begin
      Gen($DE); Gen($F9);                                               // fdiv  ;  st(1) := st(1) / st;  pop
      end;
  end;// case

  Gen($D9); Gen($1B);                                                   // fstp dword ptr [ebx]  ;  [ebx] := result;  pop

  end // if
else                                          // Ordinal types
  begin
  GenPopReg(ECX);                                                       // pop ecx
  GenPopReg(EAX);                                                       // pop eax

  case op of
    PLUSTOK:
      begin
      Gen($03); Gen($C1);                                               // add eax, ecx
      end;
    MINUSTOK:
      begin
      Gen($2B); Gen($C1);                                               // sub eax, ecx
      end;
    MULTOK:
      begin
      Gen($F7); Gen($E9);                                               // imul ecx
      end;
    IDIVTOK, MODTOK:
      begin
      Gen($99);                                                         // cdq
      Gen($F7); Gen($F9);                                               // idiv ecx
      if op = MODTOK then
        begin
        Gen($8B); Gen($C2);                                             // mov eax, edx         ; save remainder
        end;
      end;
    SHLTOK:
      begin
      Gen($D3); Gen($E0);                                               // shl eax, cl
      end;
    SHRTOK:
      begin
      Gen($D3); Gen($E8);                                               // shr eax, cl
      end;
    ANDTOK:
      begin
      Gen($23); Gen($C1);                                               // and eax, ecx
      end;
    ORTOK:
      begin
      Gen($0B); Gen($C1);                                               // or eax, ecx
      end;
    XORTOK:
      begin
      Gen($33); Gen($C1);                                               // xor eax, ecx
      end;

  end;// case

  if Types[ResultType].Kind = BOOLEANTYPE then
    begin
    Gen($83); Gen($E0); Gen($01);                                       // and eax, 1
    end;  
  
  GenPushReg(EAX);                                                      // push eax
  end;// else
end;




procedure GenerateRelation(rel: TTokenKind; ValType: Integer);
begin
if Types[ValType].Kind = REALTYPE then        // Real type
  begin
  Gen($89); Gen($E3);                                                   // mov ebx, esp
  Gen($D9); Gen($03);                                                   // fld dword ptr [ebx]  ; st = operand2
  GenPopReg(EAX);                                                       // pop eax
  Gen($89); Gen($E3);                                                   // mov ebx, esp
  Gen($D9); Gen($03);                                                   // fld dword ptr [ebx]  ; st(1) = operand2;  st = operand1
  GenPopReg(EAX);                                                       // pop eax
  Gen($89); Gen($E3);                                                   // mov ebx, esp
  Gen($DE); Gen($D9);                                                   // fcompp               ;  test st - st(1)
  Gen($DF); Gen($E0);                                                   // fnstsw ax
  Gen($68); GenDWord(1);                                                // push 1               ;  TRUE
  Gen($9E);                                                             // sahf
  case rel of
    EQTOK: Gen($74);                                                    // je  ...
    NETOK: Gen($75);                                                    // jne ...
    GTTOK: Gen($77);                                                    // ja  ...
    GETOK: Gen($73);                                                    // jae ...
    LTTOK: Gen($72);                                                    // jb  ...
    LETOK: Gen($76);                                                    // jbe ...
  end;// case
  end
else                                          // Ordinal types
  begin
  GenPopReg(ECX);                                                       // pop ecx
  GenPopReg(EAX);                                                       // pop eax
  Gen($68); GenDWord(1);                                                // push 1               ;  TRUE
  Gen($3B); Gen($C1);                                                   // cmp eax, ecx
  case rel of
    EQTOK: Gen($74);                                                    // je  ...
    NETOK: Gen($75);                                                    // jne ...
    GTTOK: Gen($7F);                                                    // jg  ...
    GETOK: Gen($7D);                                                    // jge ...
    LTTOK: Gen($7C);                                                    // jl  ...
    LETOK: Gen($7E);                                                    // jle ...
  end;// case
  end;// else

Gen($06);                                                               // ... +6
GenPopReg(ECX);                                                         // pop ecx
Gen($68); GenDWord(0);                                                  // push 0               ;  FALSE
end;





procedure GenerateAssignment(DesignatorType: Integer);
begin
// ECX should be preserved

// Source value
GenPopReg(EAX);                                                         // pop eax
// Destination address
GenPopReg(ESI);                                                         // pop esi

case TypeSize(DesignatorType) of
  1: begin
     Gen($88); Gen($06);                                                // mov [esi], al
     end;
  2: begin
     Gen($66); Gen($89); Gen($06);                                      // mov [esi], ax
     end;
  4: begin
     Gen($89); Gen($06);                                                // mov [esi], eax
     end
else
  Error('Internal fault: Illegal designator size');
end;

end;




procedure GenerateForAssignment(DesignatorType: Integer);
begin
GenPopReg(ECX);                                                         // pop ecx  ; save final counter value
GenerateAssignment(DesignatorType);
GenPushReg(ECX);                                                        // push ecx  ; restore final counter value
end;




procedure GenerateStructuredAssignment(DesignatorType: Integer);
begin
// Source address
GenPopReg(ESI);                                                         // pop esi
// Destination address
GenPopReg(EDI);                                                         // pop edi

// Copy source to destination
Gen($B9); GenDWord(TypeSize(DesignatorType));                           // mov ecx, TypeSize(DesignatorType)
Gen($FC);                                                               // cld          ; increment esi, edi after each step
Gen($F3); Gen($A4);                                                     // rep movsb
end;




procedure InverseStack(Depth: Integer);
var
  i: Integer;
begin
for i := 0 to Depth div 2 - 1 do
  begin
  Gen($8B); Gen($84); Gen($24); GenDWord(SizeOf(LongInt) * i);                        // mov eax, [esp + 4 * i]
  Gen($8B); Gen($9C); Gen($24); GenDWord(SizeOf(LongInt) * (Depth - i - 1));          // mov ebx, [esp + 4 * (Depth - i - 1)]
  Gen($89); Gen($84); Gen($24); GenDWord(SizeOf(LongInt) * (Depth - i - 1));          // mov [esp + 4 * (Depth - i - 1)], eax
  Gen($89); Gen($9C); Gen($24); GenDWord(SizeOf(LongInt) * i);                        // mov [esp + 4 * i], ebx  
  end;
end;




procedure GenerateImportFuncStub(EntryPoint: LongInt);
begin
Gen($FF); Gen($25); GenRelocDWord(EntryPoint, IMPORTRELOC);                           // jmp ds:EntryPoint  ; relocatable
end;




procedure GenerateCall(EntryPoint: LongInt; CallerNesting, CalleeNesting: Integer);
const
  StaticLinkAddr = 2 * 4;
var
  CodePos: Integer;
  i: Integer;
begin
if (CallerNesting < 0) or (CalleeNesting < 1) or (CallerNesting - CalleeNesting < -1) then
  Error('Internal fault: Illegal nesting level');
  
if CalleeNesting > 1 then                        // If a nested routine is called, push static link as the last hidden parameter
  if CallerNesting - CalleeNesting = -1 then     // The caller and the callee's enclosing routine are at the same nesting level
    begin
    GenPushReg(EBP);                                                      // push ebp
    end
  else                                           // The caller is deeper
    begin
    Gen($8B); Gen($75); Gen(StaticLinkAddr);                              // mov esi, [ebp + StaticLinkAddr]
    for i := 1 to CallerNesting - CalleeNesting do
      begin
      Gen($8B); Gen($76); Gen(StaticLinkAddr);                            // mov esi, [esi + StaticLinkAddr]
      end;
    GenPushReg(ESI);                                                      // push esi
    end;

// Call the routine  
CodePos := GetCodeSize;
Gen($E8); GenDWord(EntryPoint - (CodePos + 5));                           // call EntryPoint
end;




procedure GenerateIndirectCall(NumParam: Integer);
begin
Gen($FF); Gen($94); Gen($24); GenDWord(SizeOf(LongInt) * NumParam);       // call dword ptr [esp + 4 * NumParam]
GenPopReg(ECX);                                                           // pop ecx  ; pop and discard call address
end;




procedure GenerateReturn(TotalParamsSize, Nesting: Integer);
begin
Gen($C2);                                                               // ret ... 
if Nesting = 1 then
  GenWord(TotalParamsSize)                                              // ... TotalParamsSize
else  
  GenWord(TotalParamsSize + 4);                                         // ... TotalParamsSize + 4   ; + 4 is for static link
end;




procedure GenerateForwardReference;
begin
Gen($90);                                                     // nop   ; jump to the procedure entry point will be inserted here
Gen($90);                                                     // nop
Gen($90);                                                     // nop
Gen($90);                                                     // nop
Gen($90);                                                     // nop
end;




procedure GenerateForwardResolution(CodePos: Integer);
begin
GenAt(CodePos, $E9); GenDWordAt(CodePos + 1, GetCodeSize - (CodePos + 5));      // jmp GetCodeSize
end;




procedure GenerateForwardResolutionToDestination(CodePos, DestPos: Integer);
begin
GenAt(CodePos, $E9); GenDWordAt(CodePos + 1, DestPos - (CodePos + 5));          // jmp DestPos
end;




procedure GenerateIfCondition;
begin
GenPopReg(EAX);                                             // pop eax
Gen($83); Gen($F8); Gen($00);                               // cmp eax, 0
Gen($75); Gen($05);                                         // jne +5
end;




procedure GenerateIfProlog;
begin
SaveCodePos;

Gen($90);                                                   // nop   ; jump to the IF block end will be inserted here
Gen($90);                                                   // nop
Gen($90);                                                   // nop
Gen($90);                                                   // nop
Gen($90);                                                   // nop
end;




procedure GenerateElseProlog;
var
  CodePos: Integer;
begin
CodePos := RestoreCodePos;
GenAt(CodePos, $E9); GenDWordAt(CodePos + 1, GetCodeSize - (CodePos + 5) + 5);  // jmp (IF..THEN block end)

GenerateIfProlog;
end;




procedure GenerateIfElseEpilog;
var
  CodePos: Integer;
begin
CodePos := RestoreCodePos;
GenAt(CodePos, $E9); GenDWordAt(CodePos + 1, GetCodeSize - (CodePos + 5));      // jmp (IF..THEN block end)
end;




procedure GenerateCaseProlog;
begin
GenPopReg(ECX);                                             // pop ecx           ; CASE switch value
Gen($B0); Gen($00);                                         // mov al, 00h       ; initial flag mask
end;




procedure GenerateCaseEpilog(NumCaseStatements: Integer);
var
  i: Integer;
begin
for i := 1 to NumCaseStatements do
  GenerateIfElseEpilog;
end;




procedure GenerateCaseEqualityCheck(Value: LongInt);
begin
Gen($81); Gen($F9); GenDWord(Value);                        // cmp ecx, Value
Gen($9F);                                                   // lahf
Gen($0A); Gen($C4);                                         // or al, ah
end;




procedure GenerateCaseRangeCheck(Value1, Value2: LongInt);
begin
Gen($81); Gen($F9); GenDWord(Value1);                       // cmp ecx, Value1
Gen($7C); Gen($0A);                                         // jl +10
Gen($81); Gen($F9); GenDWord(Value2);                       // cmp ecx, Value2
Gen($7F); Gen($02);                                         // jg +2
Gen($0C); Gen($40);                                         // or al, 40h     ; set zero flag on success
end;




procedure GenerateCaseStatementProlog;
begin
Gen($24); Gen($40);                                         // and al, 40h    ; test zero flag
Gen($75); Gen($05);                                         // jnz +5         ; if set, jump to the case statement
GenerateIfProlog;
end;




procedure GenerateCaseStatementEpilog;
var
  StoredCodeSize: LongInt;
begin
StoredCodeSize := GetCodeSize;

Gen($90);                                                   // nop   ; jump to the CASE block end will be inserted here
Gen($90);                                                   // nop
Gen($90);                                                   // nop
Gen($90);                                                   // nop
Gen($90);                                                   // nop

GenerateIfElseEpilog;

Inc(CodePosStackTop);
CodePosStack[CodePosStackTop] := StoredCodeSize;
end;




procedure GenerateWhileCondition;
begin
GenerateIfCondition;
end;




procedure GenerateWhileProlog;
begin
GenerateIfProlog;
end;




procedure GenerateWhileEpilog;
var
  CodePos, CurPos, ReturnPos: Integer;
begin
CodePos := RestoreCodePos;
GenAt(CodePos, $E9); GenDWordAt(CodePos + 1, GetCodeSize - (CodePos + 5) + 5);  // jmp (WHILE..DO block end)

ReturnPos := RestoreCodePos;
CurPos := GetCodeSize;
Gen($E9); GenDWord(ReturnPos - (CurPos + 5));                                   // jmp ReturnPos
end;




procedure GenerateRepeatCondition;
begin
GenerateIfCondition;
end;




procedure GenerateRepeatProlog;
begin
SaveCodePos;
end;




procedure GenerateRepeatEpilog;
var
  CurPos, ReturnPos: Integer;
begin
ReturnPos := RestoreCodePos;
CurPos := GetCodeSize;
Gen($E9); GenDWord(ReturnPos - (CurPos + 5));               // jmp ReturnPos
end;




procedure GenerateForNumberOfIterations(Down: Boolean);
begin
if Down then
  begin
  GenPopReg(ECX);                                                 // pop ecx       ; final value
  GenPopReg(EAX);                                                 // pop eax       ; initial value  
  end
else
  begin  
  GenPopReg(EAX);                                                 // pop eax       ; final value
  GenPopReg(ECX);                                                 // pop ecx       ; initial value
  end;    
  
Gen($2B); Gen($C1);                                               // sub eax, ecx
Gen($40);                                                         // inc eax
GenPushReg(EAX);                                                  // push eax  
end;




procedure GenerateForCondition;
begin
GenPopReg(EAX);                                                   // pop eax       ; remaining number of iterations
GenPushReg(EAX);                                                  // push eax
Gen($83); Gen($F8); Gen($00);                                     // cmp eax, 0
Gen($7F); Gen($05);                                               // jg +5
end;




procedure GenerateForProlog;
begin
Inc(ForLoopNesting);
GenerateIfProlog;
end;




procedure GenerateForEpilog(CounterType: Integer; Down: Boolean);
begin
// Increment/decrement counter variable
if Down then
  GenerateIncDec(DECPROC, TypeSize(CounterType))
else
  GenerateIncDec(INCPROC, TypeSize(CounterType));
  
// Decrement remaining number of iterations
GenPopReg(EAX);                                                   // pop eax
Gen($48);                                                         // dec eax
GenPushReg(EAX);                                                  // push eax
  
GenerateWhileEpilog;

Dec(ForLoopNesting);
end;




procedure GenerateGotoProlog;
begin
NumGotos := 0;
end;




procedure GenerateGoto(LabelIndex: Integer);
begin
Inc(NumGotos);
Gotos[NumGotos].Pos := GetCodeSize;
Gotos[NumGotos].LabelIndex := LabelIndex;
Gotos[NumGotos].ForLoopNesting := ForLoopNesting;

Gen($90);               // nop   ; the remaining numbers of iterations of all nested FOR loops will be removed from stack here 
Gen($90);               // nop
Gen($90);               // nop

GenerateForwardReference;
end;




procedure GenerateGotoEpilog;
var
  CodePos: LongInt;
  i: Integer;
begin
for i := 1 to NumGotos do
  begin
  CodePos := Gotos[i].Pos;
  DiscardStackTopAt(CodePos, Gotos[i].ForLoopNesting - Ident[Gotos[i].LabelIndex].ForLoopNesting); // Remove the remaining numbers of iterations of all nested FOR loops
  GenerateForwardResolutionToDestination(CodePos + 3, Ident[Gotos[i].LabelIndex].Value);
  end;
end;




procedure GenerateNestedProcsProlog;
begin
GenerateIfProlog;
end;




procedure GenerateNestedProcsEpilog;
begin
GenerateIfElseEpilog;
end;




procedure GenerateProgramProlog;
begin
Gen($DB); Gen($E3);                                           // fninit
end;




procedure GenerateProgramEpilog;
begin

end;




procedure GenerateStackFrameProlog(TotalLocalVarsSize: Integer);
begin
GenPushReg(EBP);                                              // push ebp
Gen($8B); Gen($EC);                                           // mov ebp, esp
Gen($81); Gen($EC); GenDWord(TotalLocalVarsSize);             // sub esp, TotalLocalVarsSize
end;




procedure GenerateStackFrameEpilog;
begin
Gen($8B); Gen($E5);                                           // mov esp, ebp
GenPopReg(EBP);                                               // pop ebp
end;




procedure GenerateTempStorageProlog;
begin
SaveCodePos;

Gen($90);                                                     // nop   ; actual stack storage size will be inserted here 
Gen($90);                                                     // nop
Gen($90);                                                     // nop
Gen($90);                                                     // nop
Gen($90);                                                     // nop
Gen($90);                                                     // nop
end;




procedure GenerateTempStorageEpilog(TotalTempStorageSize: Integer);
var
  CodePos: Integer;
begin
CodePos := RestoreCodePos;
GenAt(CodePos, $81); GenAt(CodePos + 1, $EC); GenDWordAt(CodePos + 2, TotalTempStorageSize);      // sub esp, TotalTempStorageSize
end;




procedure GenerateBreakProlog(LoopNesting: Integer);
begin
BreakCall[LoopNesting].NumCalls := 0;
end;




procedure GenerateBreakCall(LoopNesting: Integer);
begin
Inc(BreakCall[LoopNesting].NumCalls);
BreakCall[LoopNesting].Pos[BreakCall[LoopNesting].NumCalls] := GetCodeSize;

GenerateForwardReference;
end;




procedure GenerateBreakEpilog(LoopNesting: Integer);
var
  i: Integer;
begin
for i := 1 to BreakCall[LoopNesting].NumCalls do
  GenerateForwardResolution(BreakCall[LoopNesting].Pos[i]);
end;




procedure GenerateContinueProlog(LoopNesting: Integer);
begin
ContinueCall[LoopNesting].NumCalls := 0;
end;




procedure GenerateContinueCall(LoopNesting: Integer);
begin
Inc(ContinueCall[LoopNesting].NumCalls);
ContinueCall[LoopNesting].Pos[ContinueCall[LoopNesting].NumCalls] := GetCodeSize;

GenerateForwardReference;
end;




procedure GenerateContinueEpilog(LoopNesting: Integer);
var
  i: Integer;
begin
for i := 1 to ContinueCall[LoopNesting].NumCalls do
  GenerateForwardResolution(ContinueCall[LoopNesting].Pos[i]);
end;




procedure GenerateExitProlog;
begin
ExitCall.NumCalls := 0;
end;




procedure GenerateExitCall;
begin
DiscardStackTop(ForLoopNesting);      // Remove the remaining numbers of iterations of all nested FOR loops

Inc(ExitCall.NumCalls);
ExitCall.Pos[ExitCall.NumCalls] := GetCodeSize;

GenerateForwardReference;
end;




procedure GenerateExitEpilog;
var
  i: Integer;
begin
for i := 1 to ExitCall.NumCalls do
  GenerateForwardResolution(ExitCall.Pos[i]);
end;


