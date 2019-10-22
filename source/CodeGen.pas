// XD Pascal - a 32-bit compiler for Windows
// Copyright (c) 2009-2010, 2019, Vasiliy Tereshkov

{$I-}
{$H-}
{$J+}

unit CodeGen;


interface


uses Common;


const
  MAXCODESIZE =  1 * 1024 * 1024;


var
  Code: array [0..MAXCODESIZE - 1] of Byte;


// Peephole optimizations: [PretriggerOp +] TriggerOp + TargetOp -> TargetOp' [+ PretriggerOp]

procedure InitializeCodeGen;
function GetCodeSize: LongInt;
procedure PushConst(Value: LongInt);
procedure PushRelocConst(Value: LongInt; RelocType: TRelocType);
procedure Relocate(CodeDeltaAddr, InitDataDeltaAddr, UninitDataDeltaAddr, ImportDeltaAddr: Integer);
procedure PushVarPtr(Addr: Integer; Scope: TScope; DeltaNesting: Byte; RelocType: TRelocType);
procedure DerefPtr(DataType: Integer);
procedure GetArrayElementPtr(ArrType: Integer);
procedure GetFieldPtr(RecType: Integer; FieldIndex: Integer);
procedure GetCharAsTempString;
procedure SaveStackTopToEAX;
procedure RestoreStackTopFromEAX;
procedure DiscardStackTop(NumItems: Byte);
procedure DuplicateStackTop;
procedure SaveCodePos;
procedure GenerateIncDec(proc: TPredefProc; Size: Byte);
procedure GenerateRound(TruncMode: Boolean);
procedure GenerateFloat(Depth: Byte);
procedure GenerateMathFunction(func: TPredefProc; ResultType: Integer);
procedure GenerateUnaryOperator(op: TTokenKind; ResultType: Integer);
procedure GenerateBinaryOperator(op: TTokenKind; ResultType: Integer);
procedure GenerateRelation(rel: TTokenKind; ValType: Integer);
procedure GenerateAssignment(DesignatorType: Integer);
procedure GenerateForAssignment(DesignatorType: Integer);
procedure GenerateStructuredAssignment(DesignatorType: Integer);
procedure InverseStack(Depth: Integer);
procedure GenerateImportFuncStub(EntryPoint: LongInt);
procedure GenerateCall(EntryPoint: LongInt; CallerNesting, CalleeNesting: Integer);
procedure GenerateIndirectCall(NumParam: Integer);
procedure GenerateReturn(TotalParamsSize, Nesting: Integer);
procedure GenerateForwardReference;
procedure GenerateForwardResolution(CodePos: Integer);
procedure GenerateIfCondition;
procedure GenerateIfProlog;
procedure GenerateElseProlog;
procedure GenerateIfElseEpilog;
procedure GenerateCaseProlog;
procedure GenerateCaseEpilog(NumCaseStatements: Integer);
procedure GenerateCaseEqualityCheck(Value: LongInt);
procedure GenerateCaseRangeCheck(Value1, Value2: LongInt);
procedure GenerateCaseStatementProlog;
procedure GenerateCaseStatementEpilog;
procedure GenerateWhileCondition;
procedure GenerateWhileProlog;
procedure GenerateWhileEpilog;
procedure GenerateRepeatCondition;
procedure GenerateRepeatProlog;
procedure GenerateRepeatEpilog;
procedure GenerateForNumberOfIterations(Down: Boolean);
procedure GenerateForCondition;
procedure GenerateForProlog;
procedure GenerateForEpilog(CounterType: Integer; Down: Boolean);
procedure GenerateGotoProlog;
procedure GenerateGoto(LabelIndex: Integer);
procedure GenerateGotoEpilog;
procedure GenerateShortCircuitProlog(op: TTokenKind);
procedure GenerateShortCircuitEpilog;
procedure GenerateNestedProcsProlog;
procedure GenerateNestedProcsEpilog;
procedure GenerateFPUInit;
procedure GenerateStackFrameProlog;
procedure GenerateStackFrameEpilog(TotalStackStorageSize: LongInt);
procedure GenerateBreakProlog(LoopNesting: Integer);
procedure GenerateBreakCall(LoopNesting: Integer);
procedure GenerateBreakEpilog(LoopNesting: Integer);
procedure GenerateContinueProlog(LoopNesting: Integer);
procedure GenerateContinueCall(LoopNesting: Integer);
procedure GenerateContinueEpilog(LoopNesting: Integer);
procedure GenerateExitProlog;
procedure GenerateExitCall;
procedure GenerateExitEpilog;



implementation


const
  MAXRELOCS       = 20000;
  MAXGOTOS        = 100;
  MAXLOOPNESTING  = 20;
  MAXBREAKCALLS   = 100;
  


type
  TRelocatable = record
    RelocType: TRelocType;
    Pos: LongInt;
    Value: LongInt;
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
   

  // Optimization-related types
  
  TRegister = (NOREG, EAX, ECX, ESI, EDI, EBP);
   
  TInstruction = 
    (
    NOP, 
    PUSH_REG, 
    PUSH_CONST, 
    PUSH_RELOCCONST, 
    PUSH_LOCALADDR, 
    FSTP, 
    LEA_PUSH_REG,
    CMP
    );
  
  TOptimizationTrigger = record
    Kind: TInstruction;
    Reg: TRegister;
    Value: LongInt;
    BaseTypeSize: LongInt;
  end;
  
  
  
var
  CodePosStack: array [0..1023] of Integer;
  CodeSize, CodePosStackTop: Integer;
  
  Reloc: array [1..MAXRELOCS] of TRelocatable;
  NumRelocs: Integer;

  Gotos: array [1..MAXGOTOS] of TGoto;
  NumGotos: Integer;
  
  BreakCall, ContinueCall: array [1..MAXLOOPNESTING] of TBreakContinueExitCallList;
  ExitCall: TBreakContinueExitCallList;   

  GenPopRegOptimizationPretrigger, 
  GenPopRegOptimizationTrigger,
  PushToFPUOptimizationTrigger,
  GenerateRelationOptimizationTrigger, 
  DerefPtrOptimizationTrigger,
  GetFieldPtrOptimizationTrigger: TOptimizationTrigger;




procedure SetOptimizationTrigger(var Trigger: TOptimizationTrigger; SetKind: TInstruction; SetReg: TRegister; SetValue: LongInt; SetBaseTypeSize: LongInt);
begin
with Trigger do
  begin
  Kind := SetKind; 
  Reg := SetReg;
  Value := SetValue;
  BaseTypeSize := SetBaseTypeSize;
  end;
end;



  
procedure ResetOptimizationTrigger(var Trigger: TOptimizationTrigger);
begin
Trigger.Kind := NOP;
end;




procedure ResetOptimizationTriggers;
begin
ResetOptimizationTrigger(GenPopRegOptimizationTrigger);
ResetOptimizationTrigger(GenPopRegOptimizationPretrigger);
ResetOptimizationTrigger(PushToFPUOptimizationTrigger);
ResetOptimizationTrigger(GenerateRelationOptimizationTrigger);
ResetOptimizationTrigger(DerefPtrOptimizationTrigger);
ResetOptimizationTrigger(GetFieldPtrOptimizationTrigger);
end;




procedure InitializeCodeGen;
begin
CodeSize        := 0; 
CodePosStackTop := 0;
NumRelocs       := 0;
NumGotos        := 0;

ResetOptimizationTriggers;
end;




function GetCodeSize{: LongInt};
begin
Result := CodeSize;
ResetOptimizationTriggers;
end;
 


 
procedure Gen(b: Byte);
begin
if CodeSize >= MAXCODESIZE then
  Error('Maximum code size exceeded');
Code[CodeSize] := b;
Inc(CodeSize);

// If the last instruction has not been optimized immediately, it will not be optimized at all
ResetOptimizationTriggers;   
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




procedure PushConst{(Value: LongInt)};
var
  SavedGenPopRegOptimizationTrigger: TOptimizationTrigger;
begin
SavedGenPopRegOptimizationTrigger := GenPopRegOptimizationTrigger;

Gen($68); GenDWord(Value);                            // push Value

SetOptimizationTrigger(GenPopRegOptimizationTrigger, PUSH_CONST, NOREG, Value, 0);
GenPopRegOptimizationPretrigger := SavedGenPopRegOptimizationTrigger;
end;




procedure PushRelocConst{(Value: LongInt; RelocType: TRelocType)};
begin
Gen($68); GenRelocDWord(Value, RelocType);            // push Value  ; relocatable

SetOptimizationTrigger(GenPopRegOptimizationTrigger, PUSH_RELOCCONST, NOREG, Value, 0);
end;




procedure Relocate{(CodeDeltaAddr, InitDataDeltaAddr, UninitDataDeltaAddr, ImportDeltaAddr: Integer)};
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

SetOptimizationTrigger(GenPopRegOptimizationTrigger, PUSH_REG, Reg, 0, 0);
end;  




procedure GenPopReg(Reg: TRegister);


  function OptimizePopReg: Boolean;
  var
    DoublePush: Boolean;
    PushedValue: LongInt;
    
  begin
  Result := FALSE;

  case GenPopRegOptimizationTrigger.Kind of
    PUSH_REG:
      begin
      // Optimization: (push Reg) + (pop Reg) -> 0
      if Reg = GenPopRegOptimizationTrigger.Reg then                                 
        begin
        Dec(CodeSize);                                                         // Remove: push Reg
        ResetOptimizationTriggers;
        Result := TRUE;
        end
                                             
      // Optimization: (push eax) + (pop ecx) -> (mov ecx, eax)
      else if (Reg = ECX) and (GenPopRegOptimizationTrigger.Reg = EAX) then         
        begin
        Dec(CodeSize);                                                         // Remove: push eax
        Gen($89); Gen($C1);                                                    // mov ecx, eax
        Result := TRUE;
        end
        
      // Optimization: (push eax) + (pop esi) -> (mov esi, eax)
      else if (Reg = ESI) and (GenPopRegOptimizationTrigger.Reg = EAX) then        
        begin
        Dec(CodeSize);                                                         // Remove: push eax                                       
        Gen($89); Gen($C6);                                                    // mov esi, eax
        Result := TRUE;
        end
        
      // Optimization: (push esi) + (pop eax) -> (mov eax, esi)
      else if (Reg = EAX) and (GenPopRegOptimizationTrigger.Reg = ESI) then        
        begin
        Dec(CodeSize);                                                         // Remove: push esi                                       
        Gen($89); Gen($F0);                                                    // mov eax, esi
        Result := TRUE;
        end        
      end;
    

    PUSH_CONST:
      begin
      // Optimization: (push Value) + (pop eax) -> (mov eax, Value)
      if Reg = EAX then                                                      
        begin        
        CodeSize := CodeSize - 5;                                              // Remove: push Value                                       
        
        // Special case: (push esi) + (push Value) + (pop eax) -> (mov eax, Value) + (push esi);   subject to further push/pop optimizations 
        DoublePush := (GenPopRegOptimizationPretrigger.Kind = PUSH_REG) and (GenPopRegOptimizationPretrigger.Reg = ESI);   
        PushedValue := GenPopRegOptimizationTrigger.Value;
        
        if DoublePush then
          Dec(CodeSize);                                                       // Remove: push esi
        
        Gen($B8); GenDWord(PushedValue);                                       // mov eax, Value
        
        if DoublePush then
          GenPushReg(ESI);                                                     // push esi
         
        Result := TRUE;
        end
        
      // Optimization: (push Value) + (pop ecx) -> (mov ecx, Value)  
      else if Reg = ECX then                                                
        begin
        CodeSize := CodeSize - 5;                                              // Remove: push Value
        
        // Special case: (push eax) + (push Value) + (pop ecx) -> (mov ecx, Value) + (push eax);   subject to further push/pop optimizations
        DoublePush := (GenPopRegOptimizationPretrigger.Kind = PUSH_REG) and (GenPopRegOptimizationPretrigger.Reg = EAX);
        PushedValue := GenPopRegOptimizationTrigger.Value;
        
        if DoublePush then
          Dec(CodeSize);                                                       // Remove: push eax
        
        Gen($B9); GenDWord(PushedValue);                                       // mov ecx, Value
        
        if DoublePush then
          begin
          GenPushReg(EAX);                                                     // push eax
        
          // Try further cmp eax, ecx optimizations
          SetOptimizationTrigger(GenerateRelationOptimizationTrigger, CMP, NOREG, PushedValue, 0);
          end;
          
        Result := TRUE;
        end
      end;
      

    PUSH_RELOCCONST:
      begin
      // Optimization: (push Value) + (pop esi) -> (mov esi, Value)  
      if Reg = ESI then
        begin
        CodeSize := CodeSize - 5;                                              // Remove: push Value                                       
        Gen($BE); GenDWord(GenPopRegOptimizationTrigger.Value);                // mov esi, Value
        Result := TRUE;
        end;
      end;
      

    PUSH_LOCALADDR:
      begin
      // Optimization: (push dword ptr [ebp + Value]) + (pop eax) -> (mov eax, dword ptr [ebp + Value])
      if (Reg = EAX) and (GenPopRegOptimizationTrigger.Reg = EBP) then
        begin
        CodeSize := CodeSize - 6;                                              // Remove: push dword ptr [ebp + Value]                                       
        Gen($8B); Gen($85); GenDWord(GenPopRegOptimizationTrigger.Value);      // mov eax, dword ptr [ebp + Value]
        Result := TRUE;
        end

      // Optimization: (push dword ptr [ebp + Value]) + (pop ecx) -> (mov ecx, dword ptr [ebp + Value])
      else if (Reg = ECX) and (GenPopRegOptimizationTrigger.Reg = EBP) then
        begin
        CodeSize := CodeSize - 6;                                              // Remove: push dword ptr [ebp + Value]                                       
        Gen($8B); Gen($8D); GenDWord(GenPopRegOptimizationTrigger.Value);      // mov ecx, dword ptr [ebp + Value]
        Result := TRUE;
        end

      // Optimization: (push dword ptr [ebp + Value]) + (pop esi) -> (mov esi, dword ptr [ebp + Value])
      else if (Reg = ESI) and (GenPopRegOptimizationTrigger.Reg = EBP) then
        begin
        CodeSize := CodeSize - 6;                                              // Remove: push dword ptr [ebp + Value]                                       
        Gen($8B); Gen($B5); GenDWord(GenPopRegOptimizationTrigger.Value);      // mov esi, dword ptr [ebp + Value]
        Result := TRUE;
        end
    
      // Optimization: (push dword ptr [esi]) + (pop eax) -> (mov eax, dword ptr [esi])
      else if (Reg = EAX) and (GenPopRegOptimizationTrigger.Reg = ESI) then
        begin
        CodeSize := CodeSize - 2;                                              // Remove: push dword ptr [esi]                                       
        Gen($8B); Gen($06);                                                    // mov eax, dword ptr [esi]
        Result := TRUE;
        end
        
      // Optimization: (push dword ptr [esi]) + (pop ecx) -> (mov ecx, dword ptr [esi])
      else if (Reg = ECX) and (GenPopRegOptimizationTrigger.Reg = ESI) then
        begin
        CodeSize := CodeSize - 2;                                              // Remove: push dword ptr [esi]                                       
        Gen($8B); Gen($0E);                                                    // mov ecx, dword ptr [esi]
        Result := TRUE;
        end;      
      end;
    
    end; // case
    
  end;


begin // GenPopReg
if not OptimizePopReg then
  case Reg of
    EAX: Gen($58);            // pop eax
    ECX: Gen($59);            // pop ecx
    ESI: Gen($5E);            // pop esi
    EDI: Gen($5F);            // pop edi
    EBP: Gen($5D)             // pop ebp
  else
    Error('Internal fault: Illegal register');  
  end;

end;




procedure GenPushToFPU;


  function OptimizeGenPushToFPU: Boolean;
  begin
  Result := FALSE;
  
  // Optimization: (fstp dword ptr [esp]) + (fld dword ptr [esp]) -> (fst dword ptr [esp])
  if PushToFPUOptimizationTrigger.Kind = FSTP then
    begin
    CodeSize := CodeSize - 3;                                         // Remove: fstp dword ptr [esp]
    Gen($D9); Gen($14); Gen($24);                                     // fst dword ptr [esp]
    Result := TRUE;
    end;

  ResetOptimizationTrigger(PushToFPUOptimizationTrigger);   
  end;
  
  
begin
if not OptimizeGenPushToFPU then
  begin
  Gen($D9); Gen($04); Gen($24);                                       // fld dword ptr [esp]
  end;
end;




procedure GenPopFromFPU;
begin
Gen($D9); Gen($1C); Gen($24);                                         // fstp dword ptr [esp]
SetOptimizationTrigger(PushToFPUOptimizationTrigger, FSTP, NOREG, 0, 0);
end; 




procedure PushVarPtr{(Addr: Integer; Scope: TScope; DeltaNesting: Byte; RelocType: TRelocType)};
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
      GenPushReg(ESI);                                          // push esi
      
      // One of the two optimizations may succeed
      SetOptimizationTrigger(DerefPtrOptimizationTrigger, LEA_PUSH_REG, EBP, Addr, 0);
      SetOptimizationTrigger(GetFieldPtrOptimizationTrigger, LEA_PUSH_REG, EBP, Addr, 0); 
      end
    else                                      // Intermediate level variable
      begin
      Gen($8B); Gen($75); Gen(StaticLinkAddr);                  // mov esi, [ebp + StaticLinkAddr]
      for i := 1 to DeltaNesting - 1 do
        begin
        Gen($8B); Gen($76); Gen(StaticLinkAddr);                // mov esi, [esi + StaticLinkAddr]
        end;
      Gen($8D); Gen($B6); GenDWord(Addr);                       // lea esi, [esi + Addr]
      GenPushReg(ESI);                                          // push esi
      end;
    
    end;// if
end;// case
end;




procedure DerefPtr{(DataType: Integer)};


  function OptimizeDerefPtr: Boolean;
  begin
  Result := FALSE;
  
  // Optimization: (lea esi, [ebp + Addr]) + (push esi) -> (push dword ptr [ebp + Addr])
  if DerefPtrOptimizationTrigger.Kind = LEA_PUSH_REG then        
    begin
    CodeSize := CodeSize - 7;                                           // Remove: lea esi, [ebp + Addr], push esi
    Gen($FF); Gen($B5); GenDWord(DerefPtrOptimizationTrigger.Value);    // push dword ptr [ebp + Addr]
    
    // Try further push/pop optimizations
    SetOptimizationTrigger(GenPopRegOptimizationTrigger, PUSH_LOCALADDR, EBP, DerefPtrOptimizationTrigger.Value, 0);   
    Result := TRUE;
    end;

  ResetOptimizationTrigger(DerefPtrOptimizationTrigger);  
  end;


begin // DerefPtr
case TypeSize(DataType) of

  1: begin
     GenPopReg(ESI);                                              // pop esi
     if Types[DataType].Kind in UnsignedTypes then
       begin
       Gen($0F); Gen($B6); Gen($06);                              // movzx eax, byte ptr [esi]
       end
     else  
       begin
       Gen($0F); Gen($BE); Gen($06);                              // movsx eax, byte ptr [esi]
       end;
     GenPushReg(EAX);                                             // push eax
     end;  
       
  2: begin
     GenPopReg(ESI);                                              // pop esi
     if Types[DataType].Kind in UnsignedTypes then
       begin
       Gen($0F); Gen($B7); Gen($06);                              // movzx eax, word ptr [esi]
       end
     else  
       begin
       Gen($0F); Gen($BF); Gen($06);                              // movsx eax, word ptr [esi]
       end;
     GenPushReg(EAX);                                             // push eax
     end;  
     
  4: begin
     // If DerefPtr immediately follows PushVarPtr, try to optimize it
     if not OptimizeDerefPtr then           
       begin
       GenPopReg(ESI);                                            // pop esi
       Gen($FF); Gen($36);                                        // push dword ptr [esi]
       
       // Optimization was unsuccessful, try a simpler one
       SetOptimizationTrigger(GenPopRegOptimizationTrigger, PUSH_LOCALADDR, ESI, 0, 0);   
       end
     end
else
  Error('Internal fault: Illegal designator size');
end;

end;




procedure GetArrayElementPtr{(ArrType: Integer)};


  function Log2(x: LongInt): ShortInt;
  var
    i: Integer;
  begin
  for i := 0 to 31 do
    if x = 1 shl i then 
      begin
      Result := i;
      Exit;
      end;  
  Result := -1;
  end;


var
  BaseTypeSize, IndexLowBound: Integer;
  Log2BaseTypeSize: ShortInt;


begin
GenPopReg(EAX);                                                 // pop eax           ; Array index
GenPopReg(ESI);                                                 // pop esi           ; Array base offset

BaseTypeSize := TypeSize(Types[ArrType].BaseType);
IndexLowBound := LowBound(Types[ArrType].IndexType);

if IndexLowBound = 1 then
  Gen($48)                                                      // dec eax
else if IndexLowBound <> 0 then
  begin
  Gen($2D); GenDWord(IndexLowBound);                            // sub eax, IndexLowBound
  end;

if (BaseTypeSize <> 1) and (BaseTypeSize <> 2) and (BaseTypeSize <> 4) and (BaseTypeSize <> 8) then
  begin
  Log2BaseTypeSize := Log2(BaseTypeSize);  
  if Log2BaseTypeSize > 0 then
    begin
    Gen($C1); Gen($E0); Gen(Log2BaseTypeSize);                  // shl eax, Log2BaseTypeSize
    end
  else
    begin
    Gen($69); Gen($C0); GenDWord(BaseTypeSize);                 // imul eax, BaseTypeSize
    end;  
  end; // if

Gen($8D); Gen($34);                                             // lea esi, [esi + eax * ...
case BaseTypeSize of
  1:   Gen($06);                                                // ... * 1]
  2:   Gen($46);                                                // ... * 2]
  4:   Gen($86);                                                // ... * 4]
  8:   Gen($C6)                                                 // ... * 8]
  else Gen($06)                                                 // ... * 1]  ; already multiplied above
end;  
GenPushReg(ESI);                                                // push esi

SetOptimizationTrigger(GetFieldPtrOptimizationTrigger, LEA_PUSH_REG, ESI, 0, BaseTypeSize);
end;




procedure GetFieldPtr{(RecType: Integer; FieldIndex: Integer)};
var
  Offset: Integer;
  

  function OptimizeGetFieldPtr: Boolean;
  begin
  Result := FALSE;
  
  if GetFieldPtrOptimizationTrigger.Kind = LEA_PUSH_REG then        
    case GetFieldPtrOptimizationTrigger.Reg of
      // Optimization: (lea esi, [ebp + Addr]) + (push esi) + (pop esi) + (add esi, Offset) -> (lea esi, [ebp + Addr + Offset])      
      EBP:
        begin
        CodeSize := CodeSize - 7;                                                     // Remove: lea esi, [ebp + Addr], push esi
        Gen($8D); Gen($B5); GenDWord(GetFieldPtrOptimizationTrigger.Value + Offset);  // lea esi, [ebp + Addr + Offset]
        Result := TRUE;
        end;
        
      // Optimization: (lea esi, [esi + eax * BaseTypeSize]) + (push esi) + (pop esi) + (add esi, Offset) -> (lea esi, [esi + eax * BaseTypeSize + Offset])
      ESI:  
        begin
        CodeSize := CodeSize - 4;                                       // Remove: lea esi, [esi + eax * BaseTypeSize], push esi
       
        Gen($8D); Gen($B4);                                             // lea esi, [esi + eax * ... + ...
        case GetFieldPtrOptimizationTrigger.BaseTypeSize of
          1:   Gen($06);                                                // ... * 1 + ...
          2:   Gen($46);                                                // ... * 2 + ...
          4:   Gen($86);                                                // ... * 4 + ...
          8:   Gen($C6)                                                 // ... * 8 + ...
          else Gen($06)                                                 // ... * 1 + ...  ; already multiplied above
        end; 
        GenDWord(Offset);                                               // ... + Offset] 
        
        Result := TRUE;
        end;
    end; // case    

  ResetOptimizationTrigger(GetFieldPtrOptimizationTrigger);  
  end;


begin
Offset := Types[RecType].Field[FieldIndex]^.Offset;

if Offset > 0 then
  begin
  if not OptimizeGetFieldPtr then
    begin
    GenPopReg(ESI);                                                 // pop esi
    Gen($81); Gen($C6); GenDWord(Offset);                           // add esi, Offset    
    end;
    
  GenPushReg(ESI);                                                  // push esi
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




procedure DiscardStackTop{(NumItems: Byte)};
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




procedure GenerateIncDec{(proc: TPredefProc; Size: Byte)};
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




procedure GenerateRound{(TruncMode: Boolean)};
begin
GenPushToFPU;                                                               // fld dword ptr [esp]  ;  st = operand

if TruncMode then
  begin
  Gen($66); Gen($C7); Gen($44); Gen($24); Gen(Byte(-4)); GenWord($0F7F);    // mov word ptr [esp - 4], 0F7Fh
  Gen($D9); Gen($6C); Gen($24); Gen(Byte(-4));                              // fldcw word ptr [esp - 4]
  end;
  
Gen($DB); Gen($1C); Gen($24);                                               // fistp dword ptr [esp] ;  [esp] := round(st);  pop

if TruncMode then
  begin
  Gen($66); Gen($C7); Gen($44); Gen($24); Gen(Byte(-4)); GenWord($037F);    // mov word ptr [esp - 4], 037Fh
  Gen($D9); Gen($6C); Gen($24); Gen(Byte(-4));                              // fldcw word ptr [esp - 4]
  end;
  
end;// GenerateRound




procedure GenerateFloat{(Depth: Byte)};
begin
if Depth = 0 then
  begin
  Gen($DB); Gen($04); Gen($24);                                         // fild dword ptr [esp]  ;  st := float(operand)
  GenPopFromFPU;                                                        // fstp dword ptr [esp]  ;  [esp] := st;  pop
  end
else
  begin  
  Gen($DB); Gen($44); Gen($24); Gen(Depth);                             // fild dword ptr [esp + Depth]  ;  st := float(operand)
  Gen($D9); Gen($5C); Gen($24); Gen(Depth);                             // fstp dword ptr [esp + Depth]  ;  [esp] := st;  pop
  end;
end;// GenerateFloat




procedure GenerateMathFunction{(func: TPredefProc; ResultType: Integer)};
begin
if Types[ResultType].Kind = REALTYPE then       // Real type
  begin
  GenPushToFPU;                                                         // fld dword ptr [esp]  ;  st = operand
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

  GenPopFromFPU;                                                        // fstp dword ptr [esp]  ;  [esp] := st;  pop
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





procedure GenerateUnaryOperator{(op: TTokenKind; ResultType: Integer)};
begin
if Types[ResultType].Kind = REALTYPE then     // Real type
  begin
  if op = MINUSTOK then
    begin
    GenPushToFPU;                                                       // fld dword ptr [esp]  ;  st = operand
    Gen($D9); Gen($E0);                                                 // fchs
    GenPopFromFPU;                                                      // fstp dword ptr [esp] ;  [esp] := st;  pop
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




procedure GenerateBinaryOperator{(op: TTokenKind; ResultType: Integer)};
begin
if Types[ResultType].Kind = REALTYPE then     // Real type
  begin
  GenPushToFPU;                                                         // fld dword ptr [esp]  ;  st = operand2
  GenPopReg(EAX);                                                       // pop eax
  GenPushToFPU;                                                         // fld dword ptr [esp]  ;  st(1) = operand2;  st = operand1
  Gen($D9); Gen($C9);                                                   // fxch                 ;  st = operand2;  st(1) = operand1

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

  GenPopFromFPU;                                                        // fstp dword ptr [esp]  ;  [esp] := st;  pop
  end // if
else                                          // Ordinal types
  begin
  // For commutative operators, use reverse operand order for better optimization
  if (op = PLUSTOK) or (op = ANDTOK) or (op = ORTOK) or (op = XORTOK) then
    begin
    GenPopReg(EAX);                                                     // pop eax
    GenPopReg(ECX);                                                     // pop ecx
    end
  else
    begin    
    GenPopReg(ECX);                                                     // pop ecx
    GenPopReg(EAX);                                                     // pop eax
    end;

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




procedure GenerateRelation{(rel: TTokenKind; ValType: Integer)};


  function OptimizeGenerateRelation: Boolean;
  begin
  Result := FALSE;
  
  // Optimization: (mov ecx, Value) + (push eax) + (pop eax) + (cmp eax, ecx) -> (cmp eax, Value)
  if GenerateRelationOptimizationTrigger.Kind = CMP then
    begin
    CodeSize := CodeSize - 6;                                           // Remove: mov ecx, Value, push eax
    Gen($3D); GenDWord(GenerateRelationOptimizationTrigger.Value);      // cmp eax, Value
    Result := TRUE;
    end;

  ResetOptimizationTrigger(GenerateRelationOptimizationTrigger);
  end;


begin
if Types[ValType].Kind = REALTYPE then        // Real type
  begin
  GenPushToFPU;                                                         // fld dword ptr [esp]  ;  st = operand2
  GenPopReg(EAX);                                                       // pop eax
  GenPushToFPU;                                                         // fld dword ptr [esp]  ;  st(1) = operand2;  st = operand1
  GenPopReg(EAX);                                                       // pop eax
  Gen($DE); Gen($D9);                                                   // fcompp               ;  test st - st(1)
  Gen($DF); Gen($E0);                                                   // fnstsw ax
  Gen($9E);                                                             // sahf  
  Gen($B8); GenDWord(1);                                                // mov eax, 1           ;  TRUE

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
  if not OptimizeGenerateRelation then
    begin
    GenPopReg(EAX);                                                     // pop eax
    Gen($39); Gen($C8);                                                 // cmp eax, ecx
    end;   
  Gen($B8); GenDWord(1);                                                // mov eax, 1           ;  TRUE
  case rel of
    EQTOK: Gen($74);                                                    // je  ...
    NETOK: Gen($75);                                                    // jne ...
    GTTOK: Gen($7F);                                                    // jg  ...
    GETOK: Gen($7D);                                                    // jge ...
    LTTOK: Gen($7C);                                                    // jl  ...
    LETOK: Gen($7E);                                                    // jle ...
  end;// case
  end;// else

Gen($02);                                                               // ... +2
Gen($31); Gen($C0);                                                     // xor eax, eax             ;  FALSE
GenPushReg(EAX);                                                        // push eax
end;





procedure GenerateAssignment{(DesignatorType: Integer)};
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




procedure GenerateForAssignment{(DesignatorType: Integer)};
begin
GenPopReg(ECX);                                                         // pop ecx  ; save final counter value
GenerateAssignment(DesignatorType);
GenPushReg(ECX);                                                        // push ecx  ; restore final counter value
end;




procedure GenerateStructuredAssignment{(DesignatorType: Integer)};
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




procedure InverseStack{(Depth: Integer)};
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




procedure GenerateImportFuncStub{(EntryPoint: LongInt)};
begin
Gen($FF); Gen($25); GenRelocDWord(EntryPoint, IMPORTRELOC);                           // jmp ds:EntryPoint  ; relocatable
end;




procedure GenerateCall{(EntryPoint: LongInt; CallerNesting, CalleeNesting: Integer)};
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




procedure GenerateIndirectCall{(NumParam: Integer)};
begin
Gen($FF); Gen($94); Gen($24); GenDWord(SizeOf(LongInt) * NumParam);       // call dword ptr [esp + 4 * NumParam]
GenPopReg(ECX);                                                           // pop ecx  ; pop and discard call address
end;




procedure GenerateReturn{(TotalParamsSize, Nesting: Integer)};
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




procedure GenerateForwardResolution{(CodePos: Integer)};
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




procedure GenerateCaseEpilog{(NumCaseStatements: Integer)};
var
  i: Integer;
begin
for i := 1 to NumCaseStatements do
  GenerateIfElseEpilog;
end;




procedure GenerateCaseEqualityCheck{(Value: LongInt)};
begin
Gen($81); Gen($F9); GenDWord(Value);                        // cmp ecx, Value
Gen($9F);                                                   // lahf
Gen($0A); Gen($C4);                                         // or al, ah
end;




procedure GenerateCaseRangeCheck{(Value1, Value2: LongInt)};
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




procedure GenerateForNumberOfIterations{(Down: Boolean)};
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
// Check remaining number of iterations
Gen($83); Gen($3C); Gen($24); Gen($00);                           // cmp dword ptr [esp], 0
Gen($7F); Gen($05);                                               // jg +5
end;




procedure GenerateForProlog;
begin
Inc(ForLoopNesting);
GenerateIfProlog;
end;




procedure GenerateForEpilog{(CounterType: Integer; Down: Boolean)};
begin
// Increment/decrement counter variable
if Down then
  GenerateIncDec(DECPROC, TypeSize(CounterType))
else
  GenerateIncDec(INCPROC, TypeSize(CounterType));
  
// Decrement remaining number of iterations
Gen($FF); Gen($0C); Gen($24);                                     // dec dword ptr [esp]
  
GenerateWhileEpilog;

Dec(ForLoopNesting);
end;




procedure GenerateGotoProlog;
begin
NumGotos := 0;
end;




procedure GenerateGoto{(LabelIndex: Integer)};
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




procedure GenerateShortCircuitProlog{(op: TTokenKind)};
begin
GenPopReg(EAX);                                                 // pop eax
Gen($83); Gen($F8); Gen($00);                                   // cmp eax, 0

case op of
  ANDTOK: Gen($75);                                             // jne ...
  ORTOK:  Gen($74);                                             // je  ...
end;
Gen($05);                                                       // ... +5                                         

GenerateIfProlog; 
end;  




procedure GenerateShortCircuitEpilog;
begin
GenPopReg(EAX);                                                 // pop eax
GenerateIfElseEpilog;
GenPushReg(EAX);                                                // push eax
end;




procedure GenerateNestedProcsProlog;
begin
GenerateIfProlog;
end;




procedure GenerateNestedProcsEpilog;
begin
GenerateIfElseEpilog;
end;




procedure GenerateFPUInit;
begin
Gen($DB); Gen($E3);                                           // fninit
end;




procedure GenerateStackFrameProlog;
begin
GenPushReg(EBP);                                              // push ebp
Gen($8B); Gen($EC);                                           // mov ebp, esp

SaveCodePos;

Gen($90);                                                     // nop   ; actual stack storage size will be inserted here 
Gen($90);                                                     // nop
Gen($90);                                                     // nop
Gen($90);                                                     // nop
Gen($90);                                                     // nop
Gen($90);                                                     // nop
end;




procedure GenerateStackFrameEpilog{(TotalStackStorageSize: LongInt)};
var
  CodePos: Integer;
begin
CodePos := RestoreCodePos;
GenAt(CodePos, $81); GenAt(CodePos + 1, $EC); GenDWordAt(CodePos + 2, TotalStackStorageSize);     // sub esp, TotalStackStorageSize

Gen($8B); Gen($E5);                                                                               // mov esp, ebp
GenPopReg(EBP);                                                                                   // pop ebp
end;




procedure GenerateBreakProlog{(LoopNesting: Integer)};
begin
BreakCall[LoopNesting].NumCalls := 0;
end;




procedure GenerateBreakCall{(LoopNesting: Integer)};
begin
Inc(BreakCall[LoopNesting].NumCalls);
BreakCall[LoopNesting].Pos[BreakCall[LoopNesting].NumCalls] := GetCodeSize;

GenerateForwardReference;
end;




procedure GenerateBreakEpilog{(LoopNesting: Integer)};
var
  i: Integer;
begin
for i := 1 to BreakCall[LoopNesting].NumCalls do
  GenerateForwardResolution(BreakCall[LoopNesting].Pos[i]);
end;




procedure GenerateContinueProlog{(LoopNesting: Integer)};
begin
ContinueCall[LoopNesting].NumCalls := 0;
end;




procedure GenerateContinueCall{(LoopNesting: Integer)};
begin
Inc(ContinueCall[LoopNesting].NumCalls);
ContinueCall[LoopNesting].Pos[ContinueCall[LoopNesting].NumCalls] := GetCodeSize;

GenerateForwardReference;
end;




procedure GenerateContinueEpilog{(LoopNesting: Integer)};
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


end.
