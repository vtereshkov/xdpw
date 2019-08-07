// XD Pascal - a 32-bit compiler for Windows
// Developed by Vasiliy Tereshkov, 2009-2010, 2019

// Parser



procedure CompileConstExpression(var ConstVal: TConst; var ConstValType: Byte); forward;
procedure CompileDesignator(var ValType: Byte); forward;
procedure CompileExpression(var ValType: Byte); forward;
procedure CompileStatement; forward;
procedure CompileType(var DataType: Byte); forward;




procedure CompileConstFactor(var ConstVal: TConst; var ConstValType: Byte);
var
  IdentIndex: Integer;
begin
case Tok.Kind of
  IDENTTOK:
    begin
    IdentIndex := GetIdent(Tok.Name);
    if Ident[IdentIndex].Kind <> CONSTANT then
      Error('Expected constant but found ', Ident[IdentIndex].Name, -1)
    else
      begin
      ConstValType := Ident[IdentIndex].DataType;
      if Types[ConstValType].TypeKind = REALTYPE then
        ConstVal.FracValue := Ident[IdentIndex].FracValue
      else
        ConstVal.Value := Ident[IdentIndex].Value;
      NextTok;
      end;
    end;


  INTNUMBERTOK:
    begin
    ConstVal.Value := Tok.Value;
    ConstValType := INTEGERTYPEINDEX;
    NextTok;
    end;


  FRACNUMBERTOK:
    begin
    ConstVal.FracValue := Tok.FracValue;
    ConstValType := REALTYPEINDEX;
    NextTok;
    end;


  CHARLITERALTOK:
    begin
    ConstVal.Value := Tok.Value;
    ConstValType := CHARTYPEINDEX;
    NextTok;
    end;


  OPARTOK:       // Expression in parentheses expected
    begin
    NextTok;
    CompileConstExpression(ConstVal, ConstValType);
    EatTok(CPARTOK);
    end;


  NOTTOK:
    begin
    CompileConstFactor(ConstVal, ConstValType);
    ConstVal.Value := not ConstVal.Value;
    end 

else
  Error('Expected expression but found ', '', Tok.Kind);
end;// case

end;// CompileConstFactor




procedure CompileConstTerm(var ConstVal: TConst; var ConstValType: Byte);
var
  OpTok: TToken;
  RightConstVal: TConst;
  RightConstValType: Byte;

begin
CompileConstFactor(ConstVal, ConstValType);

while IsMultiplicativeOperator(Tok) do
  begin
  OpTok := Tok;
  NextTok;
  CompileConstFactor(RightConstVal, RightConstValType);

  // Try to convert integer to real
  if ConversionIsPossible(ConstValType, RightConstValType) then
    begin
    ConstVal.FracValue := ConstVal.Value;
    ConstValType := REALTYPEINDEX;
    end;
  if ConversionIsPossible(RightConstValType, ConstValType) then
    begin
    RightConstVal.FracValue := RightConstVal.Value;
    RightConstValType := REALTYPEINDEX;
    end;

  // Special case: real division of two integers
  if (OpTok.Kind = DIVTOK) and ConversionIsPossible(ConstValType, REALTYPEINDEX) and ConversionIsPossible(RightConstValType, REALTYPEINDEX) then
    begin
    ConstVal.FracValue := ConstVal.Value;
    RightConstVal.FracValue := RightConstVal.Value;
    ConstValType := REALTYPEINDEX;
    RightConstValType := REALTYPEINDEX;
    end;

  ConstValType := GetCompatibleType(ConstValType, RightConstValType);
  CheckOperator(OpTok, ConstValType);

  if Types[ConstValType].TypeKind = REALTYPE then        // Real constants
    case OpTok.Kind of
      MULTOK:  ConstVal.FracValue := ConstVal.FracValue * RightConstVal.FracValue;
      DIVTOK:  if RightConstVal.FracValue <> 0 then
                 ConstVal.FracValue := ConstVal.FracValue / RightConstVal.FracValue
               else
                 Error('Constant division by zero', '', -1)
    end
  else                                                    // Integer constants
    case OpTok.Kind of             
      MULTOK:  ConstVal.Value := ConstVal.Value  *  RightConstVal.Value;
      IDIVTOK: if RightConstVal.Value <> 0 then
                 ConstVal.Value := ConstVal.Value div RightConstVal.Value
               else
                 Error('Constant division by zero', '', -1);  
      MODTOK:  if RightConstVal.Value <> 0 then
                 ConstVal.Value := ConstVal.Value mod RightConstVal.Value
               else
                 Error('Constant division by zero', '', -1);
      SHLTOK:  ConstVal.Value := ConstVal.Value shl RightConstVal.Value;
      SHRTOK:  ConstVal.Value := ConstVal.Value shr RightConstVal.Value;
      ANDTOK:  ConstVal.Value := ConstVal.Value and RightConstVal.Value;
    end;

  end;// while

end;// CompileConstTerm



procedure CompileSimpleConstExpression(var ConstVal: TConst; var ConstValType: Byte);
var
  UnaryOpTok, OpTok: TToken;
  RightConstVal: TConst;
  RightConstValType: Byte;

begin
UnaryOpTok := Tok;
if IsUnaryOperator(UnaryOpTok) then
  NextTok;

CompileConstTerm(ConstVal, ConstValType);

if IsUnaryOperator(UnaryOpTok) then
  CheckOperator(UnaryOpTok, ConstValType);

if UnaryOpTok.Kind = MINUSTOK then      // Unary minus
  if Types[ConstValType].TypeKind = REALTYPE then
    ConstVal.FracValue := -ConstVal.FracValue
  else
    ConstVal.Value := -ConstVal.Value;

while IsAdditiveOperator(Tok) do
  begin
  OpTok := Tok;
  NextTok;
  CompileConstTerm(RightConstVal, RightConstValType);

  // Try to convert integer to real
  if ConversionIsPossible(ConstValType, RightConstValType) then
    begin
    ConstVal.FracValue := ConstVal.Value;
    ConstValType := REALTYPEINDEX;
    end;
  if ConversionIsPossible(RightConstValType, ConstValType) then
    begin
    RightConstVal.FracValue := RightConstVal.Value;
    RightConstValType := REALTYPEINDEX;
    end;  

  ConstValType := GetCompatibleType(ConstValType, RightConstValType);
  CheckOperator(OpTok, ConstValType);

  if Types[ConstValType].TypeKind = REALTYPE then       // Real constants
    case OpTok.Kind of
      PLUSTOK:  ConstVal.FracValue := ConstVal.FracValue  +  RightConstVal.FracValue;
      MINUSTOK: ConstVal.FracValue := ConstVal.FracValue  -  RightConstVal.FracValue;
    end
  else                                                  // Integer constants
    case OpTok.Kind of
      PLUSTOK:  ConstVal.Value := ConstVal.Value  +  RightConstVal.Value;
      MINUSTOK: ConstVal.Value := ConstVal.Value  -  RightConstVal.Value;
      ORTOK:    ConstVal.Value := ConstVal.Value  or RightConstVal.Value;
      XORTOK:   ConstVal.Value := ConstVal.Value xor RightConstVal.Value;
    end;

  end;// while

end;// CompileSimpleConstExpression



procedure CompileConstExpression{(var ConstVal: TConst; var ConstValType: Byte)};
var
  OpTok: TToken;
  RightConstVal: TConst;
  RightConstValType, DummyType: Byte;
  Yes: Boolean;

begin
Yes := FALSE;
CompileSimpleConstExpression(ConstVal, ConstValType);

if IsRelationOperator(Tok) then
  begin
  OpTok := Tok;
  NextTok;
  CompileSimpleConstExpression(RightConstVal, RightConstValType);

  // Try to convert integer to real
  if ConversionIsPossible(ConstValType, RightConstValType) then
    begin
    ConstVal.FracValue := ConstVal.Value;
    ConstValType := REALTYPEINDEX;
    end;
  if ConversionIsPossible(RightConstValType, ConstValType) then
    begin
    RightConstVal.FracValue := RightConstVal.Value;
    RightConstValType := REALTYPEINDEX;
    end;

  DummyType := GetCompatibleType(ConstValType, RightConstValType);
  CheckOperator(OpTok, ConstValType);

  if Types[ConstValType].TypeKind = REALTYPE then
    case OpTok.Kind of
      EQTOK: Yes := ConstVal.FracValue =  RightConstVal.FracValue;
      NETOK: Yes := ConstVal.FracValue <> RightConstVal.FracValue;
      LTTOK: Yes := ConstVal.FracValue <  RightConstVal.FracValue;
      LETOK: Yes := ConstVal.FracValue <= RightConstVal.FracValue;
      GTTOK: Yes := ConstVal.FracValue >  RightConstVal.FracValue;
      GETOK: Yes := ConstVal.FracValue >= RightConstVal.FracValue;
    end
  else
    case OpTok.Kind of
      EQTOK: Yes := ConstVal.Value =  RightConstVal.Value;
      NETOK: Yes := ConstVal.Value <> RightConstVal.Value;
      LTTOK: Yes := ConstVal.Value <  RightConstVal.Value;
      LETOK: Yes := ConstVal.Value <= RightConstVal.Value;
      GTTOK: Yes := ConstVal.Value >  RightConstVal.Value;
      GETOK: Yes := ConstVal.Value >= RightConstVal.Value;
    end;

  if Yes then ConstVal.Value := -1 else ConstVal.Value := 0;
  
  ConstValType := BOOLEANTYPEINDEX;
  end;

end;// CompileConstExpression




procedure CompilePredefinedProc(proc: Byte);
var
  DesignatorType, ExpressionType, DummyType: Byte;
  ErrorCode: TConst;
  ExitLoop: Boolean;
  LibProcIdentIndex: Integer;
  IsFirstParam, FileSpecified: Boolean;
begin
NextTok;

case proc of
  INCPROC, DECPROC:
    begin
    EatTok(OPARTOK);
    AssertIdent;
    CompileDesignator(DesignatorType);
    DummyType := GetCompatibleType(DesignatorType, INTEGERTYPEINDEX);
    GenerateIncDec(proc, TypeSize(DesignatorType));
    EatTok(CPARTOK);
    end;

  READPROC, READLNPROC:
    begin
    FileSpecified := FALSE;              // By default, use standard output device, i.e. console
    IsFirstParam := TRUE;

    if Tok.Kind = OPARTOK then
      begin
      NextTok;
      repeat
        // 1st argument - file handle
        if FileSpecified then
          RestoreFileHandle
        else
          PushConst(0);                  // Console handle

        // 2nd argument - stream handle
        PushConst(0);
        
        // 3rd argument - designator
        CompileDesignator(DesignatorType);

        if Types[DesignatorType].TypeKind = TEXTTYPE then           // Text file handle
          begin
          if not IsFirstParam then
            Error('Incompatible types', '', -1);
          FileSpecified := TRUE;
          DerefPtr(DesignatorType);
          SaveFileHandle;
          end
        else                                                        // Any output expression
          begin
          LibProcIdentIndex := 0;

          if IsIntegerType(Types[DesignatorType]) or
            ((Types[DesignatorType].TypeKind = SUBRANGETYPE) and IsIntegerType(Types[Types[DesignatorType].HostType])) then
                LibProcIdentIndex := GetIdent('READINT')                 // Integer or boolean argument
          else if (Types[DesignatorType].TypeKind = CHARTYPE) or
            ((Types[DesignatorType].TypeKind = SUBRANGETYPE) and (Types[Types[DesignatorType].HostType].TypeKind = CHARTYPE)) then
                LibProcIdentIndex := GetIdent('READCH')                  // Character argument
          else if Types[DesignatorType].TypeKind = REALTYPE then
                LibProcIdentIndex := GetIdent('READREAL')                // Real argument
          else if (Types[DesignatorType].TypeKind = ARRAYTYPE) and (Types[DesignatorType].BaseType = CHARTYPEINDEX) then
                LibProcIdentIndex := GetIdent('READSTRING')              // String argument
          else
            Error('Incompatible types', '', -1);

          // Call the specific output subroutine. Interface: FileHandle; StreamHandle; var Designator
          if Pass = CALLDETERMPASS then AddCallGraphChild(BlockStack[BlockStackTop], Ident[LibProcIdentIndex].ProcAsBlock);
          GenerateCall(Ident[LibProcIdentIndex].Value, BlockStackTop - Ident[LibProcIdentIndex].NestingLevel);
          end; // else

        IsFirstParam := FALSE;

        ExitLoop := FALSE;
        if Tok.Kind = COMMATOK then
          NextTok
        else
          ExitLoop := TRUE;
      until ExitLoop;
      EatTok(CPARTOK);
      end; // if OPARTOR
      
    // Add CR+LF, if necessary
    if proc = READLNPROC then
      begin
      LibProcIdentIndex := GetIdent('READNEWLINE');
      
      // 1st argument - file handle
      if FileSpecified then
        RestoreFileHandle
      else
        PushConst(0);   // Console handle
        
      // 2nd argument - stream handle
      PushConst(0);        

      if Pass = CALLDETERMPASS then AddCallGraphChild(BlockStack[BlockStackTop], Ident[LibProcIdentIndex].ProcAsBlock);
      GenerateCall(Ident[LibProcIdentIndex].Value, BlockStackTop - Ident[LibProcIdentIndex].NestingLevel);
      end;

    end;// READPROC, READLNPROC


  WRITEPROC, WRITELNPROC:
    begin
    FileSpecified := FALSE;              // By default, use standard output device, i.e. console
    IsFirstParam := TRUE;

    if Tok.Kind = OPARTOK then
      begin
      NextTok;
      repeat
        // 1st argument - file handle
        if FileSpecified then
          RestoreFileHandle
        else
          PushConst(0);                  // Console handle

        // 2nd argument - stream handle
        PushConst(0);
        
        // 3rd argument - expression
        CompileExpression(ExpressionType);

        if Types[ExpressionType].TypeKind = TEXTTYPE then           // Text file handle
          begin
          if not IsFirstParam then
            Error('Incompatible types', '', -1);
          FileSpecified := TRUE;
          SaveFileHandle;
          end
        else                                                        // Any output expression
          begin
          LibProcIdentIndex := 0;
          
          if IsIntegerType(Types[ExpressionType]) or
            ((Types[ExpressionType].TypeKind = SUBRANGETYPE) and IsIntegerType(Types[Types[ExpressionType].HostType])) then
                LibProcIdentIndex := GetIdent('WRITEINT')                 // Integer argument
          else if (Types[ExpressionType].TypeKind = BOOLEANTYPE) or
            ((Types[ExpressionType].TypeKind = SUBRANGETYPE) and (Types[Types[ExpressionType].HostType].TypeKind = BOOLEANTYPE)) then
                LibProcIdentIndex := GetIdent('WRITEBOOLEAN')             // Boolean argument
          else if (Types[ExpressionType].TypeKind = CHARTYPE) or
            ((Types[ExpressionType].TypeKind = SUBRANGETYPE) and (Types[Types[ExpressionType].HostType].TypeKind = CHARTYPE)) then
                LibProcIdentIndex := GetIdent('WRITECH')                  // Character argument
          else if Types[ExpressionType].TypeKind = REALTYPE then
                LibProcIdentIndex := GetIdent('WRITEREAL')                // Real argument
          else if Types[ExpressionType].TypeKind = POINTERTYPE then
                LibProcIdentIndex := GetIdent('WRITEPOINTER')             // Pointer argument
          else if (Types[ExpressionType].TypeKind = ARRAYTYPE) and (Types[ExpressionType].BaseType = CHARTYPEINDEX) then
                LibProcIdentIndex := GetIdent('WRITESTRING')              // String argument
          else
            Error('Incompatible types', '', -1);

          // Call the specific output subroutine. Interface: FileHandle; StreamHandle; Expression
          if Pass = CALLDETERMPASS then AddCallGraphChild(BlockStack[BlockStackTop], Ident[LibProcIdentIndex].ProcAsBlock);
          GenerateCall(Ident[LibProcIdentIndex].Value, BlockStackTop - Ident[LibProcIdentIndex].NestingLevel);
          end; // else

        IsFirstParam := FALSE;

        ExitLoop := FALSE;
        if Tok.Kind = COMMATOK then
          NextTok
        else
          ExitLoop := TRUE;
      until ExitLoop;
      EatTok(CPARTOK);
      end; // if OPARTOR
      
    // Add CR+LF, if necessary
    if proc = WRITELNPROC then
      begin
      LibProcIdentIndex := GetIdent('WRITENEWLINE');
      
      // 1st argument - file handle
      if FileSpecified then
        RestoreFileHandle
      else
        PushConst(0);   // Console handle
        
      // 2nd argument - stream handle
      PushConst(0);         

      if Pass = CALLDETERMPASS then AddCallGraphChild(BlockStack[BlockStackTop], Ident[LibProcIdentIndex].ProcAsBlock);
      GenerateCall(Ident[LibProcIdentIndex].Value, BlockStackTop - Ident[LibProcIdentIndex].NestingLevel);
      end;

    end;// WRITEPROC, WRITELNPROC

  NEWPROC, DISPOSEPROC:
    begin
    EatTok(OPARTOK);
    AssertIdent;
    CompileDesignator(DesignatorType);
    DummyType := GetCompatibleType(DesignatorType, POINTERTYPEINDEX);
    
    if proc = NEWPROC then
      LibProcIdentIndex := GetIdent('GETMEM')
    else
      LibProcIdentIndex := GetIdent('FREEMEM');
      
    PushConst(TypeSize(Types[DesignatorType].BaseType));
    
    if Pass = CALLDETERMPASS then AddCallGraphChild(BlockStack[BlockStackTop], Ident[LibProcIdentIndex].ProcAsBlock);
    GenerateCall(Ident[LibProcIdentIndex].Value, BlockStackTop - Ident[LibProcIdentIndex].NestingLevel);
    
    EatTok(CPARTOK);
    end;

  HALTPROC:
    begin
    if Tok.Kind = OPARTOK then
      begin
      NextTok;
      CompileConstExpression(ErrorCode, ExpressionType);
      DummyType := GetCompatibleType(ExpressionType, INTEGERTYPEINDEX);
      EatTok(CPARTOK);
      end
    else
      ErrorCode.Value := 0;
      
    LibProcIdentIndex := GetIdent('EXITPROCESS');
    GenerateExternalCall(Ident[LibProcIdentIndex].Value, FALSE);
    end;

end;// case

end;// CompilePredefinedProc




procedure CompilePredefinedFunc(func: Byte; var ValType: Byte);
var
  IdentIndex: Integer;
  DummyType: Byte;
begin
NextTok;
EatTok(OPARTOK);

case func of
  SIZEOFFUNC:
    begin
    AssertIdent;
    IdentIndex := GetIdent(Tok.Name);
    if Ident[IdentIndex].Kind = USERTYPE then
      begin
      NextTok;
      PushConst(TypeSize(Ident[IdentIndex].DataType));
      end
    else
      begin
      CompileDesignator(ValType);
      SaveStackTop;                        // Save result to EDX
      PushConst(TypeSize(ValType));
      end;
    ValType := INTEGERTYPEINDEX;
    end;

  ROUNDFUNC, TRUNCFUNC:
    begin
    CompileExpression(ValType);

    // Try to convert integer to real
    if ConversionIsPossible(ValType, REALTYPEINDEX) then
      begin
      GenerateFloat(0);
      ValType := REALTYPEINDEX;
      end;

    DummyType := GetCompatibleType(ValType, REALTYPEINDEX);
    GenerateRound(func = TRUNCFUNC);
    ValType := INTEGERTYPEINDEX;
    end;

  ORDFUNC:
    begin
    CompileExpression(ValType);
    if not IsOrdinalType(Types[ValType]) then
      Error('Ordinal type expected', '', -1);
    ValType := INTEGERTYPEINDEX;
    end;

  CHRFUNC:
    begin
    CompileExpression(ValType);
    DummyType := GetCompatibleType(ValType, INTEGERTYPEINDEX);
    ValType := CHARTYPEINDEX;
    end;

  PREDFUNC, SUCCFUNC:
    begin
    CompileExpression(ValType);
    if not IsOrdinalType(Types[ValType]) then
      Error('Ordinal type expected', '', -1);
    if func = SUCCFUNC then
      PushConst(1)
    else
      PushConst(-1);
    GenerateBinaryOperator(PLUSTOK, INTEGERTYPEINDEX);
    end;

  ABSFUNC, SQRFUNC, SINFUNC, COSFUNC, ARCTANFUNC, EXPFUNC, LNFUNC, SQRTFUNC:
    begin
    CompileExpression(ValType);
    if (func = ABSFUNC) or (func = SQRFUNC) then                          // Abs and Sqr accept real or integer parameters
      begin
      if not (IsNumericType(Types[ValType]) or
             ((Types[ValType].TypeKind = SUBRANGETYPE) and IsNumericType(Types[Types[ValType].HostType]))) then
        Error('Numeric type expected', '', -1)
      end
    else
      begin
      
      // Try to convert integer to real
      if ConversionIsPossible(ValType, REALTYPEINDEX) then
        begin
        GenerateFloat(0);
        ValType := REALTYPEINDEX;
        end;

      DummyType := GetCompatibleType(ValType, REALTYPEINDEX);
      end;

    GenerateMathFunction(func, ValType);
    end;
end;// case

EatTok(CPARTOK);
end;// CompilePredefinedFunc





procedure CompileDesignator{(var ValType: Byte)};
var
  IdentIndex, FieldIndex: Integer;
  ArrayIndexType, DummyType: Byte;
  IsRefParam: Boolean;
begin
AssertIdent;

IdentIndex := GetIdent(Tok.Name);

if Ident[IdentIndex].Kind <> VARIABLE then
  Error('Expected variable but found ', '', Tok.Kind);

PushVarPtr(Ident[IdentIndex].Value, Ident[IdentIndex].Scope, BlockStackTop - Ident[IdentIndex].NestingLevel);
ValType := Ident[IdentIndex].DataType;


if IsStructuredType(Types[Ident[IdentIndex].DataType]) then
  IsRefParam := (Ident[IdentIndex].PassMethod = CONSTPASSING) or 
                (Ident[IdentIndex].PassMethod = VARPASSING)                   // For structured parameters, CONST is equivalent to VAR
else
  IsRefParam := Ident[IdentIndex].PassMethod = VARPASSING;                    // For scalar parameters, CONST is equivalent to passing by value

if IsRefParam then DerefPtr(POINTERTYPEINDEX);                                // Parameter is passed by reference


NextTok;

while (Tok.Kind = DEREFERENCETOK) or (Tok.Kind = OBRACKETTOK) or (Tok.Kind = PERIODTOK) do
  if Tok.Kind = DEREFERENCETOK then                           // Pointer dereferencing
    begin
    if (Types[ValType].TypeKind <> POINTERTYPE) or (Types[ValType].BaseType = ANYTYPEINDEX) then
      Error('Typed pointer expected', '', -1);
    DerefPtr(ValType);
    ValType := Types[ValType].BaseType;
    NextTok;
    end
  else if Tok.Kind = OBRACKETTOK then                         // Array element access
    begin
    repeat
      if Types[ValType].TypeKind <> ARRAYTYPE then
        Error('Array expected', '', -1);
      NextTok;
      CompileExpression(ArrayIndexType);                        // Array index
      DummyType := GetCompatibleType(ArrayIndexType, Types[ValType].IndexType);
      GetArrayElementPtr(ValType);
      ValType := Types[ValType].BaseType;
    until Tok.Kind <> COMMATOK;
    EatTok(CBRACKETTOK);
    end
  else if Tok.Kind = PERIODTOK then                           // Record field access
    begin
    if Types[ValType].TypeKind <> RECORDTYPE then
      Error('Record expected', '', -1);
    NextTok;
    AssertIdent;
    FieldIndex := GetField(ValType, Tok.Name);
    GetFieldPtr(ValType, FieldIndex);
    ValType := Types[ValType].Field[FieldIndex]^.DataType;
    NextTok;   
    end;
end; // CompileDesignator




procedure CompileActualParameters(IdentIndex: Integer);
var
  NumActualParams: Integer;
  ActualParamType, DummyType: Byte;
  IsRefParam, TreatCharAsString: Boolean;
  CurParam: PParam;
begin
NumActualParams := 0;

if Tok.Kind = OPARTOK then                            // Actual parameter list found
  begin
  repeat
    NextTok;

    if NumActualParams + 1 > Ident[IdentIndex].NumParams then
      Error('Too many actual parameters', '', -1);

    CurParam := Ident[IdentIndex].Param[NumActualParams + 1];

    // Evaluate actual parameters and push them onto the stack

    TreatCharAsString := (Tok.Kind = CHARLITERALTOK) and (CurParam^.DataType = STRINGTYPEINDEX);

    if (Tok.Kind = STRINGLITERALTOK) or TreatCharAsString then
      begin
      if CurParam^.PassMethod <> CONSTPASSING then
        Error('String literals can be passed as CONST only', '', -1);
      IsRefParam := FALSE;
      end
    else
      if IsStructuredType(Types[CurParam^.DataType]) then
        IsRefParam := (CurParam^.PassMethod = CONSTPASSING) or
                      (CurParam^.PassMethod = VARPASSING)                   // For structured parameters, CONST is equivalent to VAR
      else
        IsRefParam := CurParam^.PassMethod = VARPASSING;                    // For scalar parameters, CONST is equivalent to passing by value

    if TreatCharAsString then
      begin                                     // Special case
      PushVarPtr(Tok.StrAddress, GLOBAL, 0);
      ActualParamType := STRINGTYPEINDEX;
      NextTok;
      end
    else
      if IsRefParam then                        // General rule
        CompileDesignator(ActualParamType)
      else
        CompileExpression(ActualParamType);

    Inc(NumActualParams);

    // Try to convert integer to real
    if ConversionIsPossible(ActualParamType, CurParam^.DataType) and not IsRefParam then
      begin
      GenerateFloat(0);
      ActualParamType := REALTYPEINDEX;
      end;

    // Strict type checking for parameters passed by reference
    if IsRefParam and (CurParam^.DataType <> ActualParamType) then
      Error('Incompatible types', '', -1);
    
    // Relaxed type checking for parameters passed by value
    DummyType := GetCompatibleType(CurParam^.DataType, ActualParamType);
  until Tok.Kind <> COMMATOK;

  EatTok(CPARTOK);
  end;// if Tok.Kind = OPARTOR

if NumActualParams < Ident[IdentIndex].NumParams then
  Error('Too few actual parameters', '', -1);
  
end;// CompileActualParameters  

        



procedure CompileFactor(var ValType: Byte);
var
  IdentIndex: Integer;
  NotOpTok: TToken;
  FictitiousIntegerPtr: ^Integer;
begin
case Tok.Kind of
  IDENTTOK:
    begin
    IdentIndex := GetIdent(Tok.Name);
    if Ident[IdentIndex].Kind = PROC then
      Error('Expected expression but found procedure ', Ident[IdentIndex].Name, -1)
    else if Ident[IdentIndex].Kind = FUNC then                                      // Function call
      if Ident[IdentIndex].PredefIndex <> 0 then                                    // Predefined function call
        CompilePredefinedFunc(Ident[IdentIndex].PredefIndex, ValType)
      else                                                                          // User-defined function call
        begin
        NextTok;
        CompileActualParameters(IdentIndex);
        
        if Ident[IdentIndex].IsExternal then
          begin
          GenerateExternalCall(Ident[IdentIndex].Value, TRUE);
          end
        else
          begin  
          if Pass = CALLDETERMPASS then AddCallGraphChild(BlockStack[BlockStackTop], Ident[IdentIndex].ProcAsBlock);
          GenerateCall(Ident[IdentIndex].Value, BlockStackTop - Ident[IdentIndex].NestingLevel);
          RestoreStackTop;
          end;
          
        ValType := Ident[IdentIndex].DataType;
        end // FUNC
    else if Ident[IdentIndex].Kind = VARIABLE then            // Designator
      begin
      CompileDesignator(ValType);
      if not IsStructuredType(Types[ValType]) then  // Factors of type 'array' or 'record' should contain a pointer to them
        DerefPtr(ValType);
      end
    else if Ident[IdentIndex].Kind = CONSTANT then            // Constant
      begin
      ValType := Ident[IdentIndex].DataType;
      if ValType = REALTYPE then
        begin
        FictitiousIntegerPtr := Pointer(@Ident[IdentIndex].FracValue);
        PushConst(Integer(FictitiousIntegerPtr^));
        end
      else
        PushConst(Ident[IdentIndex].Value);
      NextTok;
      end
    else                                                      // Type cast
      begin
      NextTok;
      EatTok(OPARTOK);
      CompileExpression(ValType);
      EatTok(CPARTOK);

      if not (IsCastableType(Types[Ident[IdentIndex].DataType]) and 
              IsCastableType(Types[ValType])) then
        Error('Invalid typecast', '', -1);

      ValType := Ident[IdentIndex].DataType;
      end;
    end;


  ADDRESSTOK:
    begin
    NextTok;
    CompileDesignator(ValType);
    ValType := POINTERTYPEINDEX;
    end;


  INTNUMBERTOK:
    begin
    PushConst(Tok.Value);
    ValType := INTEGERTYPEINDEX;
    NextTok;
    end;


  FRACNUMBERTOK:
    begin
    FictitiousIntegerPtr := Pointer(@Tok.FracValue);
    PushConst(Integer(FictitiousIntegerPtr^));
    ValType := REALTYPEINDEX;
    NextTok;
    end;


  CHARLITERALTOK:
    begin
    PushConst(Tok.Value);
    ValType := CHARTYPEINDEX;
    NextTok;
    end;


  STRINGLITERALTOK:
    begin
    PushVarPtr(Tok.StrAddress, GLOBAL, 0);
    ValType := STRINGTYPEINDEX;
    NextTok;
    end;


  OPARTOK:       // Expression in parentheses expected
    begin
    NextTok;
    CompileExpression(ValType);
    EatTok(CPARTOK);
    end;


  NOTTOK:
    begin
    NotOpTok := Tok;
    NextTok;
    CompileFactor(ValType);
    CheckOperator(NotOpTok, ValType);
    GenerateUnaryOperator(NOTTOK, ValType);
    end;


  NILTOK:
    begin
    PushConst(0);
    ValType := POINTERTYPEINDEX;
    NextTok;
    end

else
  Error('Expected expression but found ', '', Tok.Kind);
end;// case

end;// CompileFactor




procedure CompileTerm(var ValType: Byte);
var
  OpTok: TToken;
  RightValType: Byte;
begin
CompileFactor(ValType);

while IsMultiplicativeOperator(Tok) do
  begin
  OpTok := Tok;
  NextTok;
  CompileFactor(RightValType);

  // Try to convert integer to real
  if ConversionIsPossible(ValType, RightValType) then
    begin
    GenerateFloat(SizeOf(Single));
    ValType := REALTYPEINDEX;
    end;
  if ConversionIsPossible(RightValType, ValType) then
    begin
    GenerateFloat(0);
    RightValType := REALTYPEINDEX;
    end;

  // Special case: real division of two integers
  if (OpTok.Kind = DIVTOK) and ConversionIsPossible(ValType, REALTYPEINDEX) and ConversionIsPossible(RightValType, REALTYPEINDEX) then
    begin
    GenerateFloat(SizeOf(Single));
    GenerateFloat(0);
    ValType := REALTYPEINDEX;
    RightValType := REALTYPEINDEX;
    end;


  ValType := GetCompatibleType(ValType, RightValType);
  CheckOperator(OpTok, ValType);
  GenerateBinaryOperator(OpTok.Kind, ValType);
  end;// while

end;// CompileTerm




procedure CompileSimpleExpression(var ValType: Byte);
var
  UnaryOpTok, OpTok: TToken;
  RightValType: Byte;
begin
UnaryOpTok := Tok;
if IsUnaryOperator(UnaryOpTok) then
  NextTok;

CompileTerm(ValType);

if IsUnaryOperator(UnaryOpTok) then
  CheckOperator(UnaryOpTok, ValType);

if UnaryOpTok.Kind = MINUSTOK then GenerateUnaryOperator(MINUSTOK, ValType);     // Unary minus

while IsAdditiveOperator(Tok) do
  begin
  OpTok := Tok;
  NextTok;
  CompileTerm(RightValType);

  // Try to convert integer to real
  if ConversionIsPossible(ValType, RightValType) then
    begin
    GenerateFloat(SizeOf(Single));
    ValType := REALTYPEINDEX;
    end;
  if ConversionIsPossible(RightValType, ValType) then
    begin
    GenerateFloat(0);
    RightValType := REALTYPEINDEX;
    end;

  ValType := GetCompatibleType(ValType, RightValType);
  CheckOperator(OpTok, ValType);
  GenerateBinaryOperator(OpTok.Kind, ValType);
  end;// while

end;// CompileSimpleExpression




procedure CompileExpression{(var ValType: Byte)};
var
  OpTok: TToken;
  RightValType, DummyType: Byte;
begin
CompileSimpleExpression(ValType);

if IsRelationOperator(Tok) then
  begin
  OpTok := Tok;
  NextTok;
  CompileSimpleExpression(RightValType);

  // Try to convert integer to real
  if ConversionIsPossible(ValType, RightValType) then
    begin
    GenerateFloat(SizeOf(Single));
    ValType := REALTYPEINDEX;
    end;
  if ConversionIsPossible(RightValType, ValType) then
    begin
    GenerateFloat(0);
    RightValType := REALTYPEINDEX;
    end;

  DummyType := GetCompatibleType(ValType, RightValType);
  CheckOperator(OpTok, ValType);
  ValType := BOOLEANTYPEINDEX;
  GenerateRelation(OpTok.Kind, RightValType);
  end;// while

end;// CompileExpression




procedure CompileStatementList;
begin
CompileStatement;
while Tok.Kind = SEMICOLONTOK do
  begin
  NextTok;
  CompileStatement;
  end;
end; // CompileStatementList 




procedure CompileCompoundStatement;
begin
EatTok(BEGINTOK);
CompileStatementList;
EatTok(ENDTOK);
end; // CompileCompoundStatement




procedure CompileStatement;
var
  IdentIndex, ResultIdentIndex, NumCaseStatements: Integer;
  ConstVal, ConstVal2: TConst;
  ExpressionType, DesignatorType, ConstValType, SelectorType, DummyType: Byte;
  Down, ExitLoop, TreatCharAsString: Boolean;
begin

case Tok.Kind of
  IDENTTOK:
    begin
    IdentIndex := GetIdent(Tok.Name);
    case Ident[IdentIndex].Kind of

      VARIABLE, FUNC:                                                 // Variable or function result assignment
        begin
        if Ident[IdentIndex].Kind = VARIABLE then
          CompileDesignator(DesignatorType)
        else
          begin
          if Ident[IdentIndex].ProcAsBlock <> BlockStack[BlockStackTop] then
            Error('Expected current function name but found ', Ident[IdentIndex].Name, -1);

          ResultIdentIndex := GetIdent('RESULT');
          PushVarPtr(Ident[ResultIdentIndex].Value, LOCAL, 0);
          DesignatorType := Ident[ResultIdentIndex].DataType;

          NextTok;
          end;    

        EatTok(ASSIGNTOK);

        TreatCharAsString := (Tok.Kind = CHARLITERALTOK) and (DesignatorType = STRINGTYPEINDEX);
        if TreatCharAsString then
          begin                                                       // Special case
          PushVarPtr(Tok.StrAddress, GLOBAL, 0);
          ExpressionType := STRINGTYPEINDEX;
          NextTok;
          end
        else                                                          
          CompileExpression(ExpressionType);                          // General rule - right-hand side expression

        // Try to convert integer to real
        if ConversionIsPossible(ExpressionType, DesignatorType) then
          begin
          GenerateFloat(0);
          ExpressionType := REALTYPEINDEX;
          end;

        DummyType := GetCompatibleType(DesignatorType, ExpressionType);

        if IsStructuredType(Types[DesignatorType]) then
          GenerateStructuredAssignment(DesignatorType)
        else
          GenerateAssignment(DesignatorType);
        end;// VARIABLE

      PROC:                                                           // Procedure call
        if Ident[IdentIndex].PredefIndex <> 0 then                    // Predefined procedure call
          CompilePredefinedProc(Ident[IdentIndex].PredefIndex)
        else                                                          // User-defined procedure call
          begin
          NextTok;
          CompileActualParameters(IdentIndex);
          
          if Ident[IdentIndex].IsExternal then
            begin
            GenerateExternalCall(Ident[IdentIndex].Value, FALSE);
            end
          else  
            begin
            if Pass = CALLDETERMPASS then AddCallGraphChild(BlockStack[BlockStackTop], Ident[IdentIndex].ProcAsBlock);
            GenerateCall(Ident[IdentIndex].Value, BlockStackTop - Ident[IdentIndex].NestingLevel);
            end;
          end// PROC
    else
      Error('Expected statement but found ', Ident[IdentIndex].Name, -1);
    end// case Ident[IdentIndex].Kind
    end;

  BEGINTOK:
    CompileCompoundStatement;

  IFTOK:
    begin
    NextTok;
    CompileExpression(ExpressionType);
    DummyType := GetCompatibleType(ExpressionType, BOOLEANTYPEINDEX);
    EatTok(THENTOK);

    GenerateIfCondition;              // Satisfied if expression is not zero
    GenerateIfProlog;
    CompileStatement;

    if Tok.Kind = ELSETOK then
      begin
      NextTok;
      GenerateElseProlog;                 
      CompileStatement;
      end;

    GenerateIfElseEpilog;
    end;

  CASETOK:
    begin
    NextTok;
    CompileExpression(SelectorType);
    if not IsOrdinalType(Types[SelectorType]) then
      Error('Ordinal variable expected as CASE selector', '', -1);
    EatTok(OFTOK);

    GenerateCaseProlog;  

    NumCaseStatements := 0;

    repeat       // Loop over all cases

      repeat     // Loop over all constants for the current case
        CompileConstExpression(ConstVal, ConstValType);
        DummyType := GetCompatibleType(ConstValType, SelectorType);

        if Tok.Kind = RANGETOK then                                      // Range check
          begin
          NextTok;
          CompileConstExpression(ConstVal2, ConstValType);
          DummyType := GetCompatibleType(ConstValType, SelectorType);
          GenerateCaseRangeCheck(ConstVal.Value, ConstVal2.Value);
          end
        else
          GenerateCaseEqualityCheck(ConstVal.Value);                     // Equality check

        ExitLoop := FALSE;
        if Tok.Kind = COMMATOK then
          NextTok
        else
          ExitLoop := TRUE;
      until ExitLoop;

      EatTok(COLONTOK);

      GenerateCaseStatementProlog;
      CompileStatement;
      GenerateCaseStatementEpilog;

      Inc(NumCaseStatements);

      ExitLoop := FALSE;
      if Tok.Kind <> SEMICOLONTOK then
        begin
        if Tok.Kind = ELSETOK then              // Default statements
          begin
          NextTok;
          CompileStatementList;
          end;
        ExitLoop := TRUE;
        end
      else
        begin
        NextTok;
        if Tok.Kind = ENDTOK then ExitLoop := TRUE;
        end
    until ExitLoop;

    EatTok(ENDTOK);

    GenerateCaseEpilog(NumCaseStatements);
    end;  

  WHILETOK:
    begin
    Inc(CodePosStackTop);
    CodePosStack[CodePosStackTop] := CodeSize;      // Save return address used by GenerateWhileEpilog

    NextTok;
    CompileExpression(ExpressionType);
    DummyType := GetCompatibleType(ExpressionType, BOOLEANTYPEINDEX);
    EatTok(DOTOK);

    GenerateWhileCondition;                         // Satisfied if expression is not zero
    GenerateIfProlog;
    CompileStatement;
    GenerateWhileEpilog;
    end;

  REPEATTOK:
    begin
    GenerateRepeatProlog;

    NextTok;
    CompileStatementList;

    EatTok(UNTILTOK);

    CompileExpression(ExpressionType);
    DummyType := GetCompatibleType(ExpressionType, BOOLEANTYPEINDEX);
    GenerateRepeatCondition;
    GenerateRepeatEpilog;
    end;

  FORTOK:
    begin
    NextTok;
    AssertIdent;
    IdentIndex := GetIdent(Tok.Name);

    if (Ident[IdentIndex].Kind <> VARIABLE) or
      ((Ident[IdentIndex].NestingLevel <> 1) and (Ident[IdentIndex].NestingLevel <> BlockStackTop)) or
       (Ident[IdentIndex].PassMethod <> VALPASSING) then
      Error('Simple local variable expected as FOR loop counter', '', -1);

    if not IsOrdinalType(Types[Ident[IdentIndex].DataType]) then
      Error('Ordinal variable expected as FOR loop counter', '', -1);

    PushVarPtr(Ident[IdentIndex].Value, Ident[IdentIndex].Scope, 0);

    NextTok;
    EatTok(ASSIGNTOK);
    CompileExpression(ExpressionType);
    DummyType := GetCompatibleType(ExpressionType, Ident[IdentIndex].DataType);

    if not ((Tok.Kind = TOTOK) or (Tok.Kind = DOWNTOTOK)) then
      Error('Expected TO or DOWNTO but found ', '', Tok.Kind);

    Down := Tok.Kind = DOWNTOTOK;

    NextTok;
    CompileExpression(ExpressionType);
    DummyType := GetCompatibleType(ExpressionType, Ident[IdentIndex].DataType);

    SaveStackTop;                                             // Save final value
    GenerateAssignment(Ident[IdentIndex].DataType);           // Assign initial value to the counter
    RestoreStackTop;                                          // Restore final value

    Inc(CodePosStackTop);
    CodePosStack[CodePosStackTop] := CodeSize;                // Save return address used by GenerateForEpilog

    GenerateForCondition(Ident[IdentIndex].Value, Ident[IdentIndex].Scope, TypeSize(Ident[IdentIndex].DataType), Down);  // Satisfied if counter does not reach the second expression value

    EatTok(DOTOK);

    GenerateIfProlog;
    CompileStatement;
    GenerateForEpilog(Ident[IdentIndex].Value, Ident[IdentIndex].Scope, TypeSize(Ident[IdentIndex].DataType), Down);
    end;

end;// case

end;// CompileStatement




procedure CompileType{(var DataType: Byte)};
var
  FieldInListName: array [1..MAXFIELDS] of TString;
  IdentIndex, NumFieldsInList, FieldInListIndex: LongInt;
  NestedDataType, LowBoundType, HighBoundType, ArrType, IndexType, FieldType, DummyType: Byte;
  ConstVal: TConst;
  TypeNameGiven, ExitLoop: Boolean;

  procedure DeclareField(const Name: TString; RecType, FieldType: Byte);
  var
    i: Integer;
  begin
  for i := 1 to Types[RecType].NumFields do
    if CustomStrComp(Types[RecType].Field[i]^.Name, Name) = 0 then
      Error('Duplicate field', '', -1);

  // Add new field
  Inc(Types[RecType].NumFields);
  New(Types[RecType].Field[Types[RecType].NumFields]);
  
  Types[RecType].Field[Types[RecType].NumFields]^.Name     := Name;
  Types[RecType].Field[Types[RecType].NumFields]^.DataType := FieldType;
  Types[RecType].Field[Types[RecType].NumFields]^.Offset   := TypeSize(RecType) - TypeSize(FieldType);
  end;


begin
if Tok.Kind = DEREFERENCETOK then                                 // Typed pointer
  begin
  // Add new anonymous type
  Inc(NumTypes);
  if NumTypes > MAXTYPES then
    Error('Maximum number of types exceeded', '', -1);
    
  Types[NumTypes].TypeKind := POINTERTYPE;
  DataType := NumTypes;

  // Compile pointer base type
  NextTok;
  AssertIdent;
  IdentIndex := GetIdentUnsafe(Tok.Name);                         
  
  if IdentIndex = 0 then                                          // Check for a forward-referenced base type
    begin
    // Add new forward-referenced type
    Inc(NumTypes);
    if NumTypes > MAXTYPES then
      Error('Maximum number of types exceeded', '', -1);    
    
    Types[NumTypes].TypeKind := FORWARDTYPE;
    Types[NumTypes].TypeIdentName := Tok.Name;
    Types[NumTypes].Block := BlockStack[BlockStackTop];
    NestedDataType := NumTypes;
    end
  else
    begin
    if Ident[IdentIndex].Kind <> USERTYPE then
      Error('Type name expected', '', -1);
    NestedDataType := Ident[IdentIndex].DataType;                 // Usual base type
    end;

  Types[DataType].BaseType := NestedDataType;
  Types[DataType].Block := BlockStack[BlockStackTop];

  NextTok;
  end// if DEREFERENCETOK
else if Tok.Kind = ARRAYTOK then                                  // Array
  begin
  NextTok;
  EatTok(OBRACKETTOK);

  DataType := NumTypes + 1;

  repeat
    // Add new anonymous type
    Inc(NumTypes);
    if NumTypes > MAXTYPES then
      Error('Maximum number of types exceeded', '', -1);
    
    Types[NumTypes].TypeKind := ARRAYTYPE;
    Types[NumTypes].Block := BlockStack[BlockStackTop];
    ArrType := NumTypes;

    CompileType(IndexType);
    if not IsOrdinalType(Types[IndexType]) then
      Error('Ordinal type expected', '', -1);
    Types[ArrType].IndexType := IndexType;

    ExitLoop := FALSE;
    if Tok.Kind = COMMATOK then
      begin
      Types[ArrType].BaseType := NumTypes + 1;
      NextTok;
      end
    else
      ExitLoop := TRUE;
  until ExitLoop;

  EatTok(CBRACKETTOK);
  EatTok(OFTOK);

  CompileType(NestedDataType);
  Types[ArrType].BaseType := NestedDataType;
  end // if ARRAYTOK
else if Tok.Kind = RECORDTOK then                                 // Record
  begin
  // Add new anonymous type
  Inc(NumTypes);
  if NumTypes > MAXTYPES then
    Error('Maximum number of types exceeded', '', -1);  
  
  Types[NumTypes].TypeKind := RECORDTYPE;
  DataType := NumTypes;

  NextTok;

  Types[DataType].NumFields := 0;
  repeat
    NumFieldsInList := 0;
    repeat
      AssertIdent;

      Inc(NumFieldsInList);
      FieldInListName[NumFieldsInList] := Tok.Name;

      NextTok;

      ExitLoop := FALSE;
      if Tok.Kind = COMMATOK then
        NextTok
      else
        ExitLoop := TRUE;
    until ExitLoop;

    EatTok(COLONTOK);

    CompileType(FieldType);

    for FieldInListIndex := 1 to NumFieldsInList do
      DeclareField(FieldInListName[FieldInListIndex], DataType, FieldType);

    ExitLoop := FALSE;
    if Tok.Kind <> SEMICOLONTOK then
      ExitLoop := TRUE
    else
      begin
      NextTok;
      if Tok.Kind = ENDTOK then ExitLoop := TRUE;
      end
  until ExitLoop;

  EatTok(ENDTOK);

  Types[DataType].Block := BlockStack[BlockStackTop];
  end// if RECORDTOK
else                                                                              // Subrange or type name
  begin
  TypeNameGiven := FALSE;
  IdentIndex := 0;
  if Tok.Kind = IDENTTOK then      
    begin
    IdentIndex := GetIdent(Tok.Name);
    if Ident[IdentIndex].Kind = USERTYPE then TypeNameGiven := TRUE;
    end;

  if TypeNameGiven then                                                           // Type identifier
    begin
    DataType := Ident[IdentIndex].DataType;
    NextTok;
    end
  else                                                                            // Subrange
    begin
    // Add new anonymous type
    Inc(NumTypes);
    if NumTypes > MAXTYPES then
      Error('Maximum number of types exceeded', '', -1);    
    
    Types[NumTypes].TypeKind := SUBRANGETYPE;
    DataType := NumTypes;

    CompileConstExpression(ConstVal, LowBoundType);                               // Subrange lower bound
    if not IsOrdinalType(Types[LowBoundType]) or (Types[LowBoundType].TypeKind = SUBRANGETYPE) then
      Error('Ordinal type expected', '', -1);
    Types[DataType].Low := ConstVal.Value;

    EatTok(RANGETOK);

    CompileConstExpression(ConstVal, HighBoundType);                              // Subrange upper bound
    if not IsOrdinalType(Types[HighBoundType]) or (Types[HighBoundType].TypeKind = SUBRANGETYPE) then
      Error('Ordinal type expected', '', -1);
    Types[DataType].High := ConstVal.Value;

    DummyType := GetCompatibleType(LowBoundType, HighBoundType);

    if Types[DataType].High < Types[DataType].Low then
      Error('Illegal subrange bounds', '', -1);

    Types[DataType].HostType := LowBoundType;
    Types[DataType].Block := BlockStack[BlockStackTop];
    end;// else
  end;// else  

end;// CompileType




procedure CompileBlock(BlockIdentIndex: Integer);
var
  NameTok, ProcFuncTok: TToken;
  IdentInListName: array [1..MAXPARAMS] of TString;
  LocalDataSize, ParamDataSize: Integer;
  NumIdentInList, IdentInListIndex, ForwardIdentIndex, LibProcIdentIndex, IdentIndex, ParamIndex, FieldIndex, TypeIndex: Integer;
  ConstVal: TConst;
  ExitLoop: Boolean;
  ListPassMethod: Byte;
  VarType, ConstValType: Byte;


  procedure DeclareId(const Name: TString; Kind: Byte; TotalNumParams: Integer; DataType: Byte; PassMethod: Byte; ConstValue: LongInt; FracConstValue: Single; PredefIndex: Byte);
  var
    i: Integer;
    Scope: Byte;
  begin
  if BlockStack[BlockStackTop] = 1 then Scope := GLOBAL else Scope := LOCAL;

  i := GetIdentUnsafe(Name);

  if (i > 0) and (Ident[i].Block = BlockStack[BlockStackTop]) then
    Error('Duplicate identifier ', Name, -1);

  Inc(NumIdent);
  if NumIdent > MAXIDENTS then
    Error('Maximum number of identifiers exceeded', '', -1);
    
  Ident[NumIdent].Name := Name;
  Ident[NumIdent].Kind := Kind;
  Ident[NumIdent].Scope := Scope;
  Ident[NumIdent].DataType := DataType;
  Ident[NumIdent].Block := BlockStack[BlockStackTop];
  Ident[NumIdent].NestingLevel := BlockStackTop;
  Ident[NumIdent].NumParams := 0;
  Ident[NumIdent].PassMethod := PassMethod;
  Ident[NumIdent].IsUnresolvedForward := FALSE;
  Ident[NumIdent].IsExternal := FALSE;

  case Kind of
    PROC, FUNC:
      if PredefIndex = 0 then
        Ident[NumIdent].Value := CodeSize                                 // Routine entry point address
      else
        Ident[NumIdent].PredefIndex := PredefIndex;                       // Predefined routine index

    VARIABLE:
      if (Pass = CALLDETERMPASS) or BlockIsNotDead[BlockStack[BlockStackTop]] then
        case Scope of
          GLOBAL:
            begin
            Ident[NumIdent].Value := DataSectionOrigin + VarDataOrigin + GlobalDataSize;     // Variable address
            GlobalDataSize := GlobalDataSize + TypeSize(DataType);
            end;// else

          LOCAL:
            if TotalNumParams > 0 then
              begin
              ParamDataSize := ParamDataSize + SizeOf(LongInt);                                 // Parameters always occupy 4 bytes each
              Ident[NumIdent].Value := (3 + TotalNumParams) * SizeOf(LongInt) - ParamDataSize;  // Parameter offset from bp (>0); the last (hidden) parameter is the static link
              end
            else
              begin
              Ident[NumIdent].Value := -LocalDataSize - TypeSize(DataType);                     // Local variable offset from bp (<0)
              LocalDataSize := LocalDataSize + TypeSize(DataType);
              end;
        end// case
      else
        Ident[NumIdent].Value := 0;

    CONSTANT:
      if Types[DataType].TypeKind = REALTYPE then
        Ident[NumIdent].FracValue := FracConstValue                     // Real constant value
      else
        Ident[NumIdent].Value := ConstValue;                            // Ordinal constant value

  end;// case


  if VarDataOrigin + GlobalDataSize >= MAXDATASIZE then
    Error('Maximum global data size exceeded', '', -1);

  if LocalDataSize >= MAXDATASIZE then
    Error('Maximum local data size exceeded', '', -1);

  if ParamDataSize >= MAXDATASIZE then
    Error('Maximum parameter data size exceeded', '', -1);

  end;// DeclareId



  procedure DeclarePredefinedIdents;
  begin
  // Constants
  DeclareId('TRUE',  CONSTANT, 0, BOOLEANTYPEINDEX, VALPASSING, -1, 0.0, 0);
  DeclareId('FALSE', CONSTANT, 0, BOOLEANTYPEINDEX, VALPASSING,  0, 0.0, 0);

  // Types
  DeclareId('INTEGER',  USERTYPE, 0, INTEGERTYPEINDEX,  VALPASSING, 0, 0.0, 0);
  DeclareId('SMALLINT', USERTYPE, 0, SMALLINTTYPEINDEX, VALPASSING, 0, 0.0, 0);
  DeclareId('SHORTINT', USERTYPE, 0, SHORTINTTYPEINDEX, VALPASSING, 0, 0.0, 0);
  DeclareId('CHAR',     USERTYPE, 0, CHARTYPEINDEX,     VALPASSING, 0, 0.0, 0);
  DeclareId('BOOLEAN',  USERTYPE, 0, BOOLEANTYPEINDEX,  VALPASSING, 0, 0.0, 0);
  DeclareId('REAL',     USERTYPE, 0, REALTYPEINDEX,     VALPASSING, 0, 0.0, 0);
  DeclareId('POINTER',  USERTYPE, 0, POINTERTYPEINDEX,  VALPASSING, 0, 0.0, 0);
  DeclareId('TEXT',     USERTYPE, 0, TEXTTYPEINDEX,     VALPASSING, 0, 0.0, 0);
  DeclareId('STRING',   USERTYPE, 0, STRINGTYPEINDEX,   VALPASSING, 0, 0.0, 0);

  // Procedures
  DeclareId('INC',      PROC, 0, 0, VALPASSING, 0, 0.0, INCPROC);
  DeclareId('DEC',      PROC, 0, 0, VALPASSING, 0, 0.0, DECPROC);
  DeclareId('READ',     PROC, 0, 0, VALPASSING, 0, 0.0, READPROC);
  DeclareId('WRITE',    PROC, 0, 0, VALPASSING, 0, 0.0, WRITEPROC);
  DeclareId('READLN',   PROC, 0, 0, VALPASSING, 0, 0.0, READLNPROC);
  DeclareId('WRITELN',  PROC, 0, 0, VALPASSING, 0, 0.0, WRITELNPROC);
  DeclareId('NEW',      PROC, 0, 0, VALPASSING, 0, 0.0, NEWPROC);
  DeclareId('DISPOSE',  PROC, 0, 0, VALPASSING, 0, 0.0, DISPOSEPROC);
  DeclareId('HALT',     PROC, 0, 0, VALPASSING, 0, 0.0, HALTPROC);

  // Functions
  DeclareId('SIZEOF', FUNC, 0, 0, VALPASSING, 0, 0.0, SIZEOFFUNC);
  DeclareId('ORD',    FUNC, 0, 0, VALPASSING, 0, 0.0, ORDFUNC);
  DeclareId('CHR',    FUNC, 0, 0, VALPASSING, 0, 0.0, CHRFUNC);
  DeclareId('PRED',   FUNC, 0, 0, VALPASSING, 0, 0.0, PREDFUNC);
  DeclareId('SUCC',   FUNC, 0, 0, VALPASSING, 0, 0.0, SUCCFUNC);
  DeclareId('ROUND',  FUNC, 0, 0, VALPASSING, 0, 0.0, ROUNDFUNC);
  DeclareId('TRUNC',  FUNC, 0, 0, VALPASSING, 0, 0.0, TRUNCFUNC);
  DeclareId('ABS',    FUNC, 0, 0, VALPASSING, 0, 0.0, ABSFUNC);
  DeclareId('SQR',    FUNC, 0, 0, VALPASSING, 0, 0.0, SQRFUNC);
  DeclareId('SIN',    FUNC, 0, 0, VALPASSING, 0, 0.0, SINFUNC);
  DeclareId('COS',    FUNC, 0, 0, VALPASSING, 0, 0.0, COSFUNC);
  DeclareId('ARCTAN', FUNC, 0, 0, VALPASSING, 0, 0.0, ARCTANFUNC);
  DeclareId('EXP',    FUNC, 0, 0, VALPASSING, 0, 0.0, EXPFUNC);
  DeclareId('LN',     FUNC, 0, 0, VALPASSING, 0, 0.0, LNFUNC);
  DeclareId('SQRT',   FUNC, 0, 0, VALPASSING, 0, 0.0, SQRTFUNC);
  end;// DeclarePredefinedIdents



  procedure DeclarePredefinedTypes;
  begin
  NumTypes := STRINGTYPEINDEX;

  Types[ANYTYPEINDEX].TypeKind      := ANYTYPE;
  Types[INTEGERTYPEINDEX].TypeKind  := INTEGERTYPE;
  Types[SMALLINTTYPEINDEX].TypeKind := SMALLINTTYPE;
  Types[SHORTINTTYPEINDEX].TypeKind := SHORTINTTYPE;
  Types[CHARTYPEINDEX].TypeKind     := CHARTYPE;
  Types[BOOLEANTYPEINDEX].TypeKind  := BOOLEANTYPE;
  Types[REALTYPEINDEX].TypeKind     := REALTYPE;
  Types[POINTERTYPEINDEX].TypeKind  := POINTERTYPE;
  Types[TEXTTYPEINDEX].TypeKind     := TEXTTYPE;
  Types[STRINGTYPEINDEX].TypeKind   := ARRAYTYPE;

  Types[POINTERTYPEINDEX].BaseType  := ANYTYPEINDEX;

  // Add new anonymous type: 0..MAXSTRLENGTH
  Inc(NumTypes);
  if NumTypes > MAXTYPES then
    Error('Maximum number of types exceeded', '', -1);
  
  Types[NumTypes].TypeKind := SUBRANGETYPE;
  Types[NumTypes].HostType := INTEGERTYPEINDEX;
  Types[NumTypes].Low      := 0;
  Types[NumTypes].High     := MAXSTRLENGTH;
  Types[NumTypes].Block    := BlockStack[BlockStackTop];

  Types[STRINGTYPEINDEX].BaseType  := CHARTYPEINDEX;
  Types[STRINGTYPEINDEX].IndexType := NumTypes;
  end;// DeclarePredefinedTypes



  procedure CheckForwardResolutions;
  var
    TypeIndex: Integer;
  begin
  // Search for unresolved forward references
  for TypeIndex := 1 to NumTypes do
    if (Types[TypeIndex].TypeKind = FORWARDTYPE) and
       (Types[TypeIndex].Block = BlockStack[BlockStackTop]) then
      Error('Unresolved forward reference to type ', Types[TypeIndex].TypeIdentName, -1);
  end; // CheckForwardResolutions


// DeclareId
begin
Inc(BlockStackTop);

if BlockIdentIndex = 0 then
  BlockStack[BlockStackTop] := 1
else
  BlockStack[BlockStackTop] := Ident[BlockIdentIndex].ProcAsBlock;

ParamDataSize := 0;
LocalDataSize := 0;

if BlockStack[BlockStackTop] = 1 then             // Main program
  begin
  DeclarePredefinedTypes;
  DeclarePredefinedIdents;
  GenerateProgramProlog;
  end
else
  begin
  // DeclareId parameters like local variables
  for ParamIndex := 1 to Ident[BlockIdentIndex].NumParams do
    DeclareId(Ident[BlockIdentIndex].Param[ParamIndex]^.Name,
            VARIABLE,
            Ident[BlockIdentIndex].NumParams,
            Ident[BlockIdentIndex].Param[ParamIndex]^.DataType,
            Ident[BlockIdentIndex].Param[ParamIndex]^.PassMethod,
            0,
            0.0,
            0);

  // Allocate Result variable if the current block is a function
  if Ident[BlockIdentIndex].Kind = FUNC then DeclareId('RESULT', VARIABLE, 0, Ident[BlockIdentIndex].DataType, VALPASSING, 0, 0.0, 0);
  end;// else

GenerateDeclarationProlog;


while (Tok.Kind = CONSTTOK) or (Tok.Kind = TYPETOK) or (Tok.Kind = VARTOK) or (Tok.Kind = PROCEDURETOK) or (Tok.Kind = FUNCTIONTOK) do
  begin
  if Tok.Kind = CONSTTOK then
    begin
    NextTok;
    repeat
      AssertIdent;

      NameTok := Tok;
      NextTok;
      EatTok(EQTOK);

      CompileConstExpression(ConstVal, ConstValType);
      DeclareId(NameTok.Name, CONSTANT, 0, ConstValType, VALPASSING, ConstVal.Value, ConstVal.FracValue, 0);

      EatTok(SEMICOLONTOK);
    until Tok.Kind <> IDENTTOK;

    end;// if CONSTTOK


  if Tok.Kind = TYPETOK then
    begin
    NextTok;
    repeat
      AssertIdent;

      NameTok := Tok;
      NextTok;
      EatTok(EQTOK);

      CompileType(VarType);
      DeclareId(NameTok.Name, USERTYPE, 0, VarType, VALPASSING, 0, 0.0, 0);

      // Check if this type was forward-referenced
      for TypeIndex := 1 to NumTypes do
        if (Types[TypeIndex].TypeKind = FORWARDTYPE) and
           (CustomStrComp(Types[TypeIndex].TypeIdentName, NameTok.Name) = 0) and
           (Types[TypeIndex].Block = BlockStack[BlockStackTop]) then
          begin
          // Forward type reference resolution
          Types[TypeIndex] := Types[VarType];
          if Types[VarType].TypeKind = RECORDTYPE then
            for FieldIndex := 1 to Types[VarType].NumFields do
              begin
              New(Types[TypeIndex].Field[FieldIndex]);
              Types[TypeIndex].Field[FieldIndex]^ := Types[VarType].Field[FieldIndex]^;
              end;
          end;// if    

      EatTok(SEMICOLONTOK);
    until Tok.Kind <> IDENTTOK;

    CheckForwardResolutions;

    end;// if TYPETOK


  if Tok.Kind = VARTOK then
    begin
    NextTok;
    repeat
      NumIdentInList := 0;
      repeat
        AssertIdent;

        Inc(NumIdentInList);
        IdentInListName[NumIdentInList] := Tok.Name;

        NextTok;

        ExitLoop := FALSE;
        if Tok.Kind = COMMATOK then
          NextTok
        else
          ExitLoop := TRUE;
      until ExitLoop;

      EatTok(COLONTOK);

      CompileType(VarType);

      for IdentInListIndex := 1 to NumIdentInList do
        DeclareId(IdentInListName[IdentInListIndex], VARIABLE, 0, VarType, VALPASSING, 0, 0.0, 0);

      EatTok(SEMICOLONTOK);
    until Tok.Kind <> IDENTTOK;

    CheckForwardResolutions;

    end;// if VARTOK


  if (Tok.Kind = PROCEDURETOK) or (Tok.Kind = FUNCTIONTOK) then
    begin
    ProcFuncTok := Tok;
    NextTok;

    AssertIdent;

    // Check for forward declaration resolution
    ForwardIdentIndex := GetIdentUnsafe(Tok.Name);
    if ForwardIdentIndex <> 0 then
      if not Ident[ForwardIdentIndex].IsUnresolvedForward or
         (Ident[ForwardIdentIndex].Block <> BlockStack[BlockStackTop]) or
         ((ProcFuncTok.Kind = PROCEDURETOK) and (Ident[ForwardIdentIndex].Kind <> PROC)) or
         ((ProcFuncTok.Kind = FUNCTIONTOK) and (Ident[ForwardIdentIndex].Kind <> FUNC)) then
       ForwardIdentIndex := 0;                                      // Found an identifier of another kind or scope, or it is already resolved

    if ForwardIdentIndex = 0 then
      begin

      if ProcFuncTok.Kind = PROCEDURETOK then
        DeclareId(Tok.Name, PROC, 0, 0, VALPASSING, 0, 0.0, 0)
      else
        DeclareId(Tok.Name, FUNC, 0, 0, VALPASSING, 0, 0.0, 0);

      NextTok;

      if Tok.Kind = OPARTOK then                                    // Formal parameter list found
        begin
        NextTok;
        repeat
          NumIdentInList := 0;
          ListPassMethod := VALPASSING;

          if Tok.Kind = CONSTTOK then
            begin
            ListPassMethod := CONSTPASSING;
            NextTok;
            end
          else if Tok.Kind = VARTOK then
            begin
            ListPassMethod := VARPASSING;
            NextTok;
            end;

          repeat
            AssertIdent;

            Inc(NumIdentInList);
            IdentInListName[NumIdentInList] := Tok.Name;

            NextTok;

            ExitLoop := FALSE;
            if Tok.Kind = COMMATOK then
              NextTok
            else
              ExitLoop := TRUE;
          until ExitLoop;

          EatTok(COLONTOK);

          // Only type names are allowed for formal parameters
          AssertIdent;
          IdentIndex := GetIdent(Tok.Name);
          if Ident[IdentIndex].Kind = USERTYPE then
            VarType := Ident[IdentIndex].DataType
          else
            Error('Type name expected', '', -1);
          NextTok;


          if (ListPassMethod = VALPASSING) and IsStructuredType(Types[VarType]) then
            Error('Structured parameters cannot be passed by value', '', -1);

          for IdentInListIndex := 1 to NumIdentInList do
            begin
            Inc(Ident[NumIdent].NumParams);

            if Ident[NumIdent].NumParams > MAXPARAMS then
              Error('Too many formal parameters in ', Ident[NumIdent].Name, -1);

            New(Ident[NumIdent].Param[Ident[NumIdent].NumParams]);

            Ident[NumIdent].Param[Ident[NumIdent].NumParams]^.DataType   := VarType;
            Ident[NumIdent].Param[Ident[NumIdent].NumParams]^.PassMethod := ListPassMethod;
            Ident[NumIdent].Param[Ident[NumIdent].NumParams]^.Name       := IdentInListName[IdentInListIndex];
            end;// for

          ExitLoop := FALSE;
          if Tok.Kind = SEMICOLONTOK then
            NextTok
          else
            ExitLoop := TRUE;
        until ExitLoop;

        EatTok(CPARTOK);
        end;// if Tok.Kind = OPARTOR

      Ident[NumIdent].DataType := 0;

      if ProcFuncTok.Kind = FUNCTIONTOK then
        begin
        EatTok(COLONTOK);

        // Only type names are allowed for function results
        AssertIdent;
        IdentIndex := GetIdent(Tok.Name);
        if Ident[IdentIndex].Kind = USERTYPE then
          VarType := Ident[IdentIndex].DataType
        else
          Error('Type name expected', '', -1);
        NextTok;

        if IsStructuredType(Types[VarType]) then
          Error('Structured result is not allowed', '', -1);

        Ident[NumIdent].DataType := VarType;
        end;// if IsNestedFunction

      end// if ForwardIdentIndex = 0
    else
      NextTok;

    EatTok(SEMICOLONTOK);

    // Check for an EXTERNAL or FORWARD directive (they are not reserved words)
    if (ForwardIdentIndex = 0) and (Tok.Kind = IDENTTOK) and (CustomStrComp(Tok.Name, 'EXTERNAL') = 0) then  // External declaration
      begin
      NextTok;
      EatTok(STRINGLITERALTOK);
      Ident[NumIdent].IsExternal := TRUE;
      Ident[NumIdent].Value := AddImportFunc(Tok.Name);
      end    
    else if (ForwardIdentIndex = 0) and (Tok.Kind = IDENTTOK) and (CustomStrComp(Tok.Name, 'FORWARD') = 0) then  // Forward declaration
      begin
      Inc(NumBlocks);
      if NumTypes > MAXBLOCKS then
        Error('Maximum number of blocks exceeded', '', -1);
        
      Ident[NumIdent].ProcAsBlock := NumBlocks;
      Ident[NumIdent].IsUnresolvedForward := TRUE;
      GenerateForwardReference;
      NextTok;
      end
    else
      begin

      if ForwardIdentIndex = 0 then                                                       // New declaration
        begin
        Inc(NumBlocks);
        if NumTypes > MAXBLOCKS then
          Error('Maximum number of blocks exceeded', '', -1);
        
        Ident[NumIdent].ProcAsBlock := NumBlocks;
        CompileBlock(NumIdent);
        end
      else                                                                                // Forward declaration resolution
        begin
        GenerateForwardResolution(ForwardIdentIndex);
        CompileBlock(ForwardIdentIndex);
        Ident[ForwardIdentIndex].IsUnresolvedForward := FALSE;
        end;

      end;

    EatTok(SEMICOLONTOK);
    end;// PROCEDURETOK, FUNCTIONTOK

  end;// while

GenerateDeclarationEpilog;  // Make jump to block entry point

if BlockStack[BlockStackTop] = 1 then
  begin  
  // Initialize heap and console I/O
  LibProcIdentIndex := GetIdent('INITSYSTEM');
  if Pass = CALLDETERMPASS then AddCallGraphChild(BlockStack[BlockStackTop], Ident[LibProcIdentIndex].ProcAsBlock);
  GenerateCall(Ident[LibProcIdentIndex].Value, BlockStackTop - Ident[LibProcIdentIndex].NestingLevel);  
  end
else
  GenerateStackFrameProlog(LocalDataSize);  

CompileCompoundStatement;

// If function, return Result value via the EDX register
if (BlockStack[BlockStackTop] <> 1) and (Ident[BlockIdentIndex].Kind = FUNC) then
  begin
  PushVarPtr(Ident[GetIdent('RESULT')].Value, LOCAL, 0);
  DerefPtr(Ident[BlockIdentIndex].DataType);
  SaveStackTop;
  end;

if BlockStack[BlockStackTop] = 1 then          // Main program
  begin
  GenerateProgramEpilog;
  LibProcIdentIndex := GetIdent('EXITPROCESS');
  GenerateExternalCall(Ident[LibProcIdentIndex].Value, FALSE);
  end
else
  begin
  GenerateStackFrameEpilog;
  GenerateReturn(Ident[BlockIdentIndex].NumParams * SizeOf(LongInt));
  end;
   
// Delete local identifiers and types from the tables to save space
while (NumIdent > 0) and (Ident[NumIdent].Block = BlockStack[BlockStackTop]) do
  begin
  // If procedure or function, delete parameters first
  if (Ident[NumIdent].Kind = PROC) or (Ident[NumIdent].Kind = FUNC) then
    begin   
    if Ident[NumIdent].IsUnresolvedForward then
      Error('Unresolved forward declaration of ', Ident[NumIdent].Name, -1);

    for ParamIndex := 1 to Ident[NumIdent].NumParams do
      Dispose(Ident[NumIdent].Param[ParamIndex]);
    end; 

  // Delete identifier itself
  Dec(NumIdent);
  end; 
  
  
while (NumTypes > 0) and (Types[NumTypes].Block = BlockStack[BlockStackTop]) do
  begin
  // If record, delete fields first
  if Types[NumTypes].TypeKind = RECORDTYPE then
    for FieldIndex := 1 to Types[NumTypes].NumFields do
      Dispose(Types[NumTypes].Field[FieldIndex]);

  // Delete type itself
  Dec(NumTypes);
  end;    

Dec(BlockStackTop);
end;// CompileBlock




procedure CompileProgram;
begin
NextTok;
EatTok(PROGRAMTOK);
AssertIdent;
NextTok;
CheckTok(SEMICOLONTOK);

EnterIncludedFile('system.pas');
NextTok;

Inc(NumBlocks);
if NumTypes > MAXBLOCKS then
  Error('Maximum number of blocks exceeded', '', -1);
  
CompileBlock(0);

CheckTok(PERIODTOK);
end;// CompileProgram




