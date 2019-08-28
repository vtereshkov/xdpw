// XD Pascal - a 32-bit compiler for Windows
// Developed by Vasiliy Tereshkov, 2009-2010, 2019

// Parser



procedure CompileConstExpression(var ConstVal: TConst; var ConstValType: Integer); forward;
procedure CompileDesignator(var ValType: Integer); forward;
procedure CompileExpression(var ValType: Integer); forward;
procedure CompileStatement(LoopNesting: Integer); forward;
procedure CompileType(var DataType: Integer); forward;




procedure CompileConstFactor(var ConstVal: TConst; var ConstValType: Integer);
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




procedure CompileConstTerm(var ConstVal: TConst; var ConstValType: Integer);
var
  OpTok: TToken;
  RightConstVal: TConst;
  RightConstValType: Integer;

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



procedure CompileSimpleConstExpression(var ConstVal: TConst; var ConstValType: Integer);
var
  UnaryOpTok, OpTok: TToken;
  RightConstVal: TConst;
  RightConstValType: Integer;

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



procedure CompileConstExpression{(var ConstVal: TConst; var ConstValType: Integer)};
var
  OpTok: TToken;
  RightConstVal: TConst;
  RightConstValType, DummyType: Integer;
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




procedure CompilePredefinedProc(proc: Byte; LoopNesting: Integer);
var
  DesignatorType, ExpressionType, DummyType: Integer;
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

        if Types[DesignatorType].TypeKind = FILETYPE then           // Text file handle
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

          if (Types[DesignatorType].TypeKind = INTEGERTYPE) or
            ((Types[DesignatorType].TypeKind = SUBRANGETYPE) and (Types[Types[DesignatorType].HostType].TypeKind = INTEGERTYPE)) then
                LibProcIdentIndex := GetIdent('READINT')                 // Integer argument
          else if (Types[DesignatorType].TypeKind = SMALLINTTYPE) or
            ((Types[DesignatorType].TypeKind = SUBRANGETYPE) and (Types[Types[DesignatorType].HostType].TypeKind = SMALLINTTYPE)) then
                LibProcIdentIndex := GetIdent('READSMALLINT')            // Small integer argument
          else if (Types[DesignatorType].TypeKind = SHORTINTTYPE) or
            ((Types[DesignatorType].TypeKind = SUBRANGETYPE) and (Types[Types[DesignatorType].HostType].TypeKind = SHORTINTTYPE)) then
                LibProcIdentIndex := GetIdent('READSHORTINT')            // Short integer argument
          else if (Types[DesignatorType].TypeKind = BOOLEANTYPE) or
            ((Types[DesignatorType].TypeKind = SUBRANGETYPE) and (Types[Types[DesignatorType].HostType].TypeKind = BOOLEANTYPE)) then
                LibProcIdentIndex := GetIdent('READBOOLEAN')             // Boolean argument          
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
          GenerateCall(Ident[LibProcIdentIndex].Value, BlockStackTop - 1, Ident[LibProcIdentIndex].NestingLevel);
          end; // else

        IsFirstParam := FALSE;

        if Tok.Kind <> COMMATOK then Break;
        NextTok;
      until FALSE;
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

      GenerateCall(Ident[LibProcIdentIndex].Value, BlockStackTop - 1, Ident[LibProcIdentIndex].NestingLevel);
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

        if Types[ExpressionType].TypeKind = FILETYPE then           // Text file handle
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
          GenerateCall(Ident[LibProcIdentIndex].Value, BlockStackTop - 1, Ident[LibProcIdentIndex].NestingLevel);
          end; // else

        IsFirstParam := FALSE;

        if Tok.Kind <> COMMATOK then Break;
        NextTok;
      until FALSE;
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

      GenerateCall(Ident[LibProcIdentIndex].Value, BlockStackTop - 1, Ident[LibProcIdentIndex].NestingLevel);
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
    
    GenerateCall(Ident[LibProcIdentIndex].Value, BlockStackTop - 1, Ident[LibProcIdentIndex].NestingLevel);
    
    EatTok(CPARTOK);
    end;
    
  BREAKPROC:
    begin
    if LoopNesting < 1 then
      Error('BREAK outside of loop is not allowed', '', -1);
    GenerateBreakCall(LoopNesting);
    end;  

  CONTINUEPROC:
    begin
    if LoopNesting < 1 then
      Error('CONTINUE outside of loop is not allowed', '', -1);
    GenerateContinueCall(LoopNesting);
    end;    
    
  EXITPROC:
    GenerateExitCall;  

  HALTPROC:
    begin
    if Tok.Kind = OPARTOK then
      begin
      NextTok;
      CompileExpression(ExpressionType);
      DummyType := GetCompatibleType(ExpressionType, INTEGERTYPEINDEX);
      EatTok(CPARTOK);
      end
    else
      PushConst(0);
      
    LibProcIdentIndex := GetIdent('EXITPROCESS');
    GenerateExternalCall(Ident[LibProcIdentIndex].Value, Ident[LibProcIdentIndex].NumParams, FALSE);
    end;

end;// case

end;// CompilePredefinedProc




procedure CompilePredefinedFunc(func: Byte; var ValType: Integer);
var
  IdentIndex: Integer;
  DummyType: Integer;
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
      SaveStackTopToEDX;                        // Save result to EDX just to discard it
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




procedure CompileTypeIdent(var DataType: Integer; AllowForwardReference: Boolean);
var
  IdentIndex: Integer;
begin
// STRING, FILE or type name allowed
case Tok.Kind of
  STRINGTOK:
    DataType := STRINGTYPEINDEX;
  FILETOK:
    DataType := FILETYPEINDEX
else
  AssertIdent;
  
  if AllowForwardReference then
    IdentIndex := GetIdentUnsafe(Tok.Name)
  else
    IdentIndex := GetIdent(Tok.Name);                         
  
  if IdentIndex = 0 then                                    // Forward-referenced type
    begin
    // Add new forward-referenced type
    Inc(NumTypes);
    if NumTypes > MAXTYPES then
      Error('Maximum number of types exceeded', '', -1);    
    
    Types[NumTypes].TypeKind := FORWARDTYPE;
    Types[NumTypes].TypeIdentName := Tok.Name;
    Types[NumTypes].Block := BlockStack[BlockStackTop];
    DataType := NumTypes;
    end
  else
    begin
    if Ident[IdentIndex].Kind <> USERTYPE then
      Error('Type name expected', '', -1);
    DataType := Ident[IdentIndex].DataType;                 // Usual type
    end;
end; // case

NextTok;
end;
  



procedure CompileFormalParametersAndResult(IsFunction: Boolean; var NumFormalParams: Integer; var FormalParam: PParams; var ResultType: Integer);
var
  IdentInListName: array [1..MAXPARAMS] of TString;
  NumIdentInList, IdentInListIndex: Integer;  
  ParamType: Integer;    
  ListPassMethod: Byte;
  IsOpenArrayList: Boolean;
  
begin
NumFormalParams := 0;
  
if Tok.Kind = OPARTOK then
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

      if Tok.Kind <> COMMATOK then Break;
      NextTok;
    until FALSE;

    
    // Formal parameter list type
    if Tok.Kind = COLONTOK then                       // Typed parameters 
      begin
      NextTok;
    
      // Special case: open array parameters
      if Tok.Kind = ARRAYTOK then
        begin
        NextTok;
        EatTok(OFTOK);
        IsOpenArrayList := TRUE;
        end
      else
        IsOpenArrayList := FALSE;
 
      // Type itself
      CompileTypeIdent(ParamType, FALSE);          
                 
      // Special case: open array parameters
      if IsOpenArrayList then
        begin
        // Add new anonymous type
        Inc(NumTypes);
        if NumTypes > MAXTYPES then
          Error('Maximum number of types exceeded', '', -1);
        
        Types[NumTypes].TypeKind    := ARRAYTYPE;
        Types[NumTypes].Block       := BlockStack[BlockStackTop];
        Types[NumTypes].BaseType    := ParamType;
        Types[NumTypes].IndexType   := Types[STRINGTYPEINDEX].IndexType;   // Any subrange with zero lower bound
        Types[NumTypes].IsOpenArray := TRUE;
        
        ParamType := NumTypes;
        end;  
      end
    else                                              // Untyped parameters (CONST or VAR only) 
      ParamType := ANYTYPEINDEX;

    
    if (ListPassMethod = VALPASSING) and IsStructuredType(Types[ParamType]) then
      Error('Structured parameters cannot be passed by value', '', -1);
      
    if (ListPassMethod = VALPASSING) and (ParamType = ANYTYPEINDEX) then
      Error('Untyped parameters cannot be passed by value', '', -1);          

    for IdentInListIndex := 1 to NumIdentInList do
      begin
      Inc(NumFormalParams);

      if NumFormalParams > MAXPARAMS then
        Error('Too many formal parameters', '', -1);

      New(FormalParam[NumFormalParams]);

      FormalParam[NumFormalParams]^.DataType   := ParamType;
      FormalParam[NumFormalParams]^.PassMethod := ListPassMethod;
      FormalParam[NumFormalParams]^.Name       := IdentInListName[IdentInListIndex];
      end;// for

    if Tok.Kind <> SEMICOLONTOK then Break;
    NextTok;
  until FALSE;

  EatTok(CPARTOK);
  end;// if Tok.Kind = OPARTOR


// Function result type
ResultType := 0;

if IsFunction then
  begin
  EatTok(COLONTOK);
  
  CompileTypeIdent(ResultType, FALSE);

  if IsStructuredType(Types[ResultType]) then
    Error('Structured result is not allowed', '', -1);
  end;
end; // CompileFormalParametersAndResult




procedure CompileActualParameters(NumFormalParams: Integer; var FormalParam: PParams);
var
  NumActualParams: Integer;
  ActualParamType, DummyType: Integer;
  IsRefParam, TreatCharAsString: Boolean;
  CurParam: PParam;
begin
NumActualParams := 0;

if Tok.Kind = OPARTOK then                            // Actual parameter list found
  begin
  NextTok;
  
  if Tok.Kind <> CPARTOK then
    repeat
      if NumActualParams + 1 > NumFormalParams then
        Error('Too many actual parameters', '', -1);

      CurParam := FormalParam[NumActualParams + 1];

      // Evaluate actual parameters and push them onto the stack

      TreatCharAsString := (Tok.Kind = CHARLITERALTOK) and (CurParam^.DataType = STRINGTYPEINDEX);

      if (Tok.Kind = STRINGLITERALTOK) or TreatCharAsString then
        begin
        if CurParam^.PassMethod <> CONSTPASSING then
          Error('String literals can be passed as CONST only', '', -1);
        IsRefParam := FALSE;
        end
      else
        if IsStructuredType(Types[CurParam^.DataType]) or (Types[CurParam^.DataType].TypeKind = ANYTYPE) then
          IsRefParam := (CurParam^.PassMethod = CONSTPASSING) or
                        (CurParam^.PassMethod = VARPASSING)                   // For structured parameters, CONST is equivalent to VAR
        else
          IsRefParam := CurParam^.PassMethod = VARPASSING;                    // For scalar parameters, CONST is equivalent to passing by value

      if TreatCharAsString then
        begin                                     // Special case
        PushVarPtr(Tok.StrAddress, GLOBAL, 0, RELOCNONE);
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
      
      if IsRefParam then  // Strict type checking for parameters passed by reference, except for open array parameters and untyped parameters
        begin    
        if (CurParam^.DataType <> ActualParamType) then
          if (Types[CurParam^.DataType].TypeKind = ARRAYTYPE) and Types[CurParam^.DataType].IsOpenArray then
            begin
            if (Types[ActualParamType].TypeKind <> ARRAYTYPE) or (Types[ActualParamType].BaseType <> Types[CurParam^.DataType].BaseType) then
              Error('Incompatible types', '', -1);
            end  
          else if Types[CurParam^.DataType].TypeKind <> ANYTYPE then
            Error('Incompatible types', '', -1);
        end
      else                // Relaxed type checking for parameters passed by value      
        DummyType := GetCompatibleType(CurParam^.DataType, ActualParamType);
        
    if Tok.Kind <> COMMATOK then Break;
    NextTok;
    until FALSE;

  EatTok(CPARTOK);
  end;// if Tok.Kind = OPARTOK

if NumActualParams < NumFormalParams then
  Error('Too few actual parameters', '', -1);
  
end;// CompileActualParameters  




procedure CompileDesignator{(var ValType: Integer)};
var
  IdentIndex, FieldIndex: Integer;
  ArrayIndexType, DummyType: Integer;
  IsRefParam: Boolean;
begin
AssertIdent;

IdentIndex := GetIdent(Tok.Name);

if Ident[IdentIndex].Kind <> VARIABLE then
  Error('Expected variable but found ', '', Tok.Kind);

PushVarPtr(Ident[IdentIndex].Value, Ident[IdentIndex].Scope, BlockStackTop - Ident[IdentIndex].NestingLevel, RELOCDATA);
ValType := Ident[IdentIndex].DataType;


if IsStructuredType(Types[Ident[IdentIndex].DataType]) or (Types[Ident[IdentIndex].DataType].TypeKind = ANYTYPE) then
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




procedure CompileFactor(var ValType: Integer);
var
  IdentIndex: Integer;
  NotOpTok: TToken;
  FictitiousIntegerPtr: ^Integer;
begin
case Tok.Kind of
  IDENTTOK:
    begin
    IdentIndex := GetIdent(Tok.Name);
    
    case Ident[IdentIndex].Kind of
      PROC:
        Error('Expected expression but found procedure ', Ident[IdentIndex].Name, -1);
        
      FUNC:                                                                           // Function call
        if Ident[IdentIndex].PredefIndex <> 0 then                                    // Predefined function call
          CompilePredefinedFunc(Ident[IdentIndex].PredefIndex, ValType)
        else                                                                          // User-defined function call
          begin
          NextTok;
          CompileActualParameters(Ident[IdentIndex].NumParams, Ident[IdentIndex].Param);
          
          if Ident[IdentIndex].IsExternal then
            GenerateExternalCall(Ident[IdentIndex].Value, Ident[IdentIndex].NumParams, TRUE)
          else
            begin  
            GenerateCall(Ident[IdentIndex].Value, BlockStackTop - 1, Ident[IdentIndex].NestingLevel);
            RestoreStackTopFromEAX;
            end;
            
          ValType := Ident[IdentIndex].DataType;
          end;
          
      VARIABLE:                                                                       // Designator
        begin
        CompileDesignator(ValType);
        if not IsStructuredType(Types[ValType]) then  // Factors of type 'array' or 'record' should contain a pointer to them
          DerefPtr(ValType);
        
        if Tok.Kind = OPARTOK then                    // Procedural variable call - parentheses required even for empty parameter list
          begin
          if (Types[ValType].TypeKind <> PROCEDURALTYPE) or (Types[ValType].ResultType = 0) then 
            Error('Function variable expected', '', -1); 

          SaveStackTopToEDI;                          // Save call address
          CompileActualParameters(Types[ValType].NumParams, Types[ValType].Param);
          RestoreStackTopFromEDI;                     // Restore call address
          GenerateIndirectCall;
          RestoreStackTopFromEAX; 
          
          ValType := Types[ValType].ResultType;
          end;  
        end;
        
      CONSTANT:                                                                       // Constant
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
        end;
        
      USERTYPE:                                                                       // Type cast
        begin                                                                      
        NextTok;
        EatTok(OPARTOK);
        CompileExpression(ValType);
        EatTok(CPARTOK);

        if not (IsCastableType(Types[Ident[IdentIndex].DataType]) and 
                IsCastableType(Types[ValType])) then
          Error('Invalid typecast', '', -1);

        ValType := Ident[IdentIndex].DataType;
        end
        
      else
        Error('Internal fault: Illegal identifier', '', -1);  
      end; // case Ident[IdentIndex].Kind      
    end;  


  ADDRESSTOK:
    begin
    NextTok;
    IdentIndex := GetIdent(Tok.Name);
    
    if (Ident[IdentIndex].Kind = PROC) or (Ident[IdentIndex].Kind = FUNC) then
      begin
      if (Ident[IdentIndex].PredefIndex <> 0) or (Ident[IdentIndex].Block <> 1) then
        Error('Procedure or function cannot be predefined or nested', '', -1);
        
      PushRelocConst(Ident[IdentIndex].Value, RELOCCODE); // To be resolved later when code section origin is known
      NextTok;
      end
    else  
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
    PushVarPtr(Tok.StrAddress, GLOBAL, 0, RELOCNONE);
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




procedure CompileTerm(var ValType: Integer);
var
  OpTok: TToken;
  RightValType: Integer;
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




procedure CompileSimpleExpression(var ValType: Integer);
var
  UnaryOpTok, OpTok: TToken;
  RightValType: Integer;
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




procedure CompileExpression{(var ValType: Integer)};
var
  OpTok: TToken;
  RightValType, DummyType: Integer;
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




procedure CompileStatementList(LoopNesting: Integer);
begin
CompileStatement(LoopNesting);
while Tok.Kind = SEMICOLONTOK do
  begin
  NextTok;
  CompileStatement(LoopNesting);
  end;
end; // CompileStatementList 




procedure CompileCompoundStatement(LoopNesting: Integer);
begin
EatTok(BEGINTOK);
CompileStatementList(LoopNesting);
EatTok(ENDTOK);
end; // CompileCompoundStatement




procedure CompileStatement{(LoopNesting: Integer)};
var
  IdentIndex, ResultIdentIndex, NumCaseStatements: Integer;
  ConstVal, ConstVal2: TConst;
  ExpressionType, DesignatorType, ConstValType, SelectorType, DummyType: Integer;
  Down, TreatCharAsString: Boolean;
begin

case Tok.Kind of
  IDENTTOK:
    begin
    IdentIndex := GetIdent(Tok.Name);
    case Ident[IdentIndex].Kind of

      VARIABLE, FUNC:                                                 // Variable or function result assignment, or procedural variable call
        begin
        if Ident[IdentIndex].Kind = VARIABLE then
          CompileDesignator(DesignatorType)
        else
          begin
          if Ident[IdentIndex].ProcAsBlock <> BlockStack[BlockStackTop] then
            Error('Expected current function name but found ', Ident[IdentIndex].Name, -1);

          ResultIdentIndex := GetIdent('RESULT');
          PushVarPtr(Ident[ResultIdentIndex].Value, LOCAL, 0, RELOCDATA);
          DesignatorType := Ident[ResultIdentIndex].DataType;

          NextTok;
          end;    

        if Tok.Kind = ASSIGNTOK then                                    // Assignment
          begin
          NextTok;
        
          TreatCharAsString := (Tok.Kind = CHARLITERALTOK) and (DesignatorType = STRINGTYPEINDEX);
          if TreatCharAsString then
            begin                                                       // Special case
            PushVarPtr(Tok.StrAddress, GLOBAL, 0, RELOCNONE);
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
          end
          
        else                                                            // Procedural variable call                                                              
          begin
          CheckTok(OPARTOK);                                            // Parentheses required even for empty parameter list
         
          if (Types[DesignatorType].TypeKind <> PROCEDURALTYPE) or (Types[DesignatorType].ResultType <> 0) then 
            Error('Procedure variable expected', '', -1);  

          DerefPtr(DesignatorType);
          SaveStackTopToEDI;                                            // Save call address
          CompileActualParameters(Types[DesignatorType].NumParams, Types[DesignatorType].Param);
          RestoreStackTopFromEDI;                                       // Restore call address
          GenerateIndirectCall;
          end;  
  
        end; // VARIABLE, FUNC
        

      PROC:                                                           // Procedure call
        if Ident[IdentIndex].PredefIndex <> 0 then                    // Predefined procedure call
          CompilePredefinedProc(Ident[IdentIndex].PredefIndex, LoopNesting)
        else                                                          // User-defined procedure call
          begin
          NextTok;
          CompileActualParameters(Ident[IdentIndex].NumParams, Ident[IdentIndex].Param);
          
          if Ident[IdentIndex].IsExternal then
            GenerateExternalCall(Ident[IdentIndex].Value, Ident[IdentIndex].NumParams, FALSE)
          else  
            GenerateCall(Ident[IdentIndex].Value, BlockStackTop - 1, Ident[IdentIndex].NestingLevel);
          end// PROC
    else
      Error('Expected statement but found ', Ident[IdentIndex].Name, -1);
    end// case Ident[IdentIndex].Kind
    end;
    

  BEGINTOK:
    CompileCompoundStatement(LoopNesting);
    

  IFTOK:
    begin
    NextTok;
    CompileExpression(ExpressionType);
    DummyType := GetCompatibleType(ExpressionType, BOOLEANTYPEINDEX);
    EatTok(THENTOK);

    GenerateIfCondition;              // Satisfied if expression is not zero
    GenerateIfProlog;
    CompileStatement(LoopNesting);

    if Tok.Kind = ELSETOK then
      begin
      NextTok;
      GenerateElseProlog;                 
      CompileStatement(LoopNesting);
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

        if Tok.Kind <> COMMATOK then Break;
        NextTok;
      until FALSE;

      EatTok(COLONTOK);

      GenerateCaseStatementProlog;
      CompileStatement(LoopNesting);
      GenerateCaseStatementEpilog;

      Inc(NumCaseStatements);

      if Tok.Kind <> SEMICOLONTOK then
        begin
        if Tok.Kind = ELSETOK then              // Default statements
          begin
          NextTok;
          CompileStatementList(LoopNesting);
          end;          
        Break;
        end;

      NextTok;
    until Tok.Kind = ENDTOK;

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

    GenerateBreakProlog(LoopNesting + 1);
    GenerateContinueProlog(LoopNesting + 1);
    GenerateWhileCondition;                         // Satisfied if expression is not zero
    GenerateWhileProlog;
    
    CompileStatement(LoopNesting + 1);
    
    GenerateContinueEpilog(LoopNesting + 1);
    GenerateWhileEpilog;
    GenerateBreakEpilog(LoopNesting + 1);
    end;
    

  REPEATTOK:
    begin
    GenerateBreakProlog(LoopNesting + 1);
    GenerateContinueProlog(LoopNesting + 1);
    GenerateRepeatProlog;

    NextTok;
    CompileStatementList(LoopNesting + 1);

    EatTok(UNTILTOK);
    GenerateContinueEpilog(LoopNesting + 1);

    CompileExpression(ExpressionType);
    DummyType := GetCompatibleType(ExpressionType, BOOLEANTYPEINDEX);
    GenerateRepeatCondition;
    GenerateRepeatEpilog;
    GenerateBreakEpilog(LoopNesting + 1);
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

    PushVarPtr(Ident[IdentIndex].Value, Ident[IdentIndex].Scope, 0, RELOCDATA);

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

    SaveStackTopToEDX;                                        // Save final value of the counter
    GenerateAssignment(Ident[IdentIndex].DataType);           // Assign initial value to the counter
    RestoreStackTopFromEDX;                                   // Restore final value of the counter

    Inc(CodePosStackTop);
    CodePosStack[CodePosStackTop] := CodeSize;                // Save return address used by GenerateForEpilog

    GenerateForCondition(Ident[IdentIndex].Value, Ident[IdentIndex].Scope, TypeSize(Ident[IdentIndex].DataType), Down);  // Satisfied if counter does not reach the second expression value

    EatTok(DOTOK);
    
    GenerateBreakProlog(LoopNesting + 1);
    GenerateContinueProlog(LoopNesting + 1);
    GenerateForProlog;
    
    CompileStatement(LoopNesting + 1);    
    
    GenerateContinueEpilog(LoopNesting + 1);
    GenerateForEpilog(Ident[IdentIndex].Value, Ident[IdentIndex].Scope, TypeSize(Ident[IdentIndex].DataType), Down);
    GenerateBreakEpilog(LoopNesting + 1);
    SaveStackTopToEDX;                                        // Pop and discard final value of the counter
    end;

end;// case

end;// CompileStatement




procedure CompileType{(var DataType: Integer)};


  procedure CompileTypedPointerType(var DataType: Integer);
  var
    NestedDataType: Integer;
  begin
  // Add new anonymous type
  Inc(NumTypes);
  if NumTypes > MAXTYPES then
    Error('Maximum number of types exceeded', '', -1);
    
  Types[NumTypes].TypeKind := POINTERTYPE;
  DataType := NumTypes;

  // Compile pointer base type
  NextTok;
  CompileTypeIdent(NestedDataType, TRUE);
    
  Types[DataType].BaseType := NestedDataType;
  Types[DataType].Block := BlockStack[BlockStackTop];
  end; // CompileTypedPointerType
  
  
  
  
  procedure CompileArrayType(var DataType: Integer);
  var
    ArrType, IndexType, NestedDataType: Integer;
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
    Types[NumTypes].IsOpenArray := FALSE;
    ArrType := NumTypes;

    CompileType(IndexType);
    if not IsOrdinalType(Types[IndexType]) then
      Error('Ordinal type expected', '', -1);
    Types[ArrType].IndexType := IndexType;

    if Tok.Kind <> COMMATOK then Break;
    
    Types[ArrType].BaseType := NumTypes + 1;
    NextTok;
  until FALSE;

  EatTok(CBRACKETTOK);
  EatTok(OFTOK);

  CompileType(NestedDataType);
  Types[ArrType].BaseType := NestedDataType;  
  end; // CompileArrayType
  
  
  
  
  procedure CompileRecordType(var DataType: Integer);
  

    procedure DeclareField(const Name: TString; RecType, FieldType: Integer);
    var
      i: Integer;
    begin
    for i := 1 to Types[RecType].NumFields do
      if CompareStr(Types[RecType].Field[i]^.Name, Name) = 0 then
        Error('Duplicate field', '', -1);

    // Add new field
    Inc(Types[RecType].NumFields);
    New(Types[RecType].Field[Types[RecType].NumFields]);
    
    Types[RecType].Field[Types[RecType].NumFields]^.Name     := Name;
    Types[RecType].Field[Types[RecType].NumFields]^.DataType := FieldType;
    Types[RecType].Field[Types[RecType].NumFields]^.Offset   := TypeSize(RecType) - TypeSize(FieldType);
    end;
    
  
  var
    FieldInListName: array [1..MAXFIELDS] of TString;
    NumFieldsInList, FieldInListIndex: Integer;
    FieldType: Integer;
  
  
  begin // CompileRecordType
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

      if Tok.Kind <> COMMATOK then Break;
      NextTok;
    until FALSE;

    EatTok(COLONTOK);

    CompileType(FieldType);

    for FieldInListIndex := 1 to NumFieldsInList do
      DeclareField(FieldInListName[FieldInListIndex], DataType, FieldType);

    if Tok.Kind <> SEMICOLONTOK then Break; 
    NextTok;
  until Tok.Kind = ENDTOK;

  EatTok(ENDTOK);

  Types[DataType].Block := BlockStack[BlockStackTop];
  end; // CompileRecordType




  procedure CompileSubrangeType(var DataType: Integer);
  var
    ConstVal: TConst;
    LowBoundType, HighBoundType, DummyType: Integer;
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
  end; // CompileSubrangeType
  
  
  
  
  procedure CompileProceduralType(var DataType: Integer; IsFunction: Boolean);
  begin
  Inc(NumTypes);
  if NumTypes > MAXTYPES then
    Error('Maximum number of types exceeded', '', -1);    
  
  Types[NumTypes].TypeKind := PROCEDURALTYPE;
  Types[NumTypes].Block := BlockStack[BlockStackTop];
  DataType := NumTypes;
  
  NextTok;
  
  CompileFormalParametersAndResult(IsFunction, Types[DataType].NumParams, Types[DataType].Param, Types[DataType].ResultType);  
  end; // CompileProceduralType
  
  

var
  IdentIndex: LongInt;
  TypeNameGiven: Boolean;   


begin // CompileType
case Tok.Kind of
  DEREFERENCETOK: 
    CompileTypedPointerType(DataType);  
  ARRAYTOK:       
    CompileArrayType(DataType);  
  RECORDTOK:      
    CompileRecordType(DataType);
  STRINGTOK:
    begin 
    DataType := STRINGTYPEINDEX;
    NextTok;
    end;
  FILETOK:
    begin 
    DataType := FILETYPEINDEX;
    NextTok;
    end;
  PROCEDURETOK, FUNCTIONTOK:
    CompileProceduralType(DataType, Tok.Kind = FUNCTIONTOK)     
else                                                                              // Subrange or type name
  TypeNameGiven := FALSE;
  IdentIndex := 0;
  if Tok.Kind = IDENTTOK then      
    begin
    IdentIndex := GetIdent(Tok.Name);
    if Ident[IdentIndex].Kind = USERTYPE then TypeNameGiven := TRUE;
    end;

  if TypeNameGiven then                                                           // Type name
    begin
    DataType := Ident[IdentIndex].DataType;
    NextTok;
    end
  else                                                                            // Subrange
    CompileSubrangeType(DataType);
end; // case  

end;// CompileType





procedure CompileBlock(BlockIdentIndex: Integer);
var
  LocalDataSize, ParamDataSize: Integer;



  procedure DeclareId(const Name: TString; Kind: Byte; TotalNumParams: Integer; DataType: Integer; PassMethod: Byte; ConstValue: LongInt; FracConstValue: Single; PredefIndex: Byte);
  var
    i, AdditionalStackItems: Integer;
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
        begin
        Ident[NumIdent].Value := CodeSize;                                // Routine entry point address
        Ident[NumIdent].PredefIndex := 0;
        end
      else
        begin
        Ident[NumIdent].Value := 0;
        Ident[NumIdent].PredefIndex := PredefIndex;                       // Predefined routine index
        end;

    VARIABLE:
      case Scope of
       GLOBAL:
         begin
         Ident[NumIdent].Value := GlobalDataSize;                                              // Variable address (relocatable)
         GlobalDataSize := GlobalDataSize + TypeSize(DataType);
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
      if Types[DataType].TypeKind = REALTYPE then
        Ident[NumIdent].FracValue := FracConstValue                     // Real constant value
      else
        Ident[NumIdent].Value := ConstValue;                            // Ordinal constant value

  end;// case


  if GlobalDataSize + MAXSTATICSTRDATASIZE >= MAXDATASIZE then
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

  // Procedures
  DeclareId('INC',      PROC, 0, 0, VALPASSING, 0, 0.0, INCPROC);
  DeclareId('DEC',      PROC, 0, 0, VALPASSING, 0, 0.0, DECPROC);
  DeclareId('READ',     PROC, 0, 0, VALPASSING, 0, 0.0, READPROC);
  DeclareId('WRITE',    PROC, 0, 0, VALPASSING, 0, 0.0, WRITEPROC);
  DeclareId('READLN',   PROC, 0, 0, VALPASSING, 0, 0.0, READLNPROC);
  DeclareId('WRITELN',  PROC, 0, 0, VALPASSING, 0, 0.0, WRITELNPROC);
  DeclareId('NEW',      PROC, 0, 0, VALPASSING, 0, 0.0, NEWPROC);
  DeclareId('DISPOSE',  PROC, 0, 0, VALPASSING, 0, 0.0, DISPOSEPROC);
  DeclareId('BREAK',    PROC, 0, 0, VALPASSING, 0, 0.0, BREAKPROC);
  DeclareId('CONTINUE', PROC, 0, 0, VALPASSING, 0, 0.0, CONTINUEPROC);  
  DeclareId('EXIT',     PROC, 0, 0, VALPASSING, 0, 0.0, EXITPROC);
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
  Types[FILETYPEINDEX].TypeKind     := FILETYPE;
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

  Types[STRINGTYPEINDEX].BaseType    := CHARTYPEINDEX;
  Types[STRINGTYPEINDEX].IndexType   := NumTypes;
  Types[STRINGTYPEINDEX].IsOpenArray := FALSE;
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
  
  
  
  
  procedure CompileConstDeclarations;
  var
    NameTok: TToken;
    ConstVal: TConst;
    ConstValType: Integer;    
  begin
  repeat
    AssertIdent;

    NameTok := Tok;
    NextTok;
    EatTok(EQTOK);

    CompileConstExpression(ConstVal, ConstValType);
    DeclareId(NameTok.Name, CONSTANT, 0, ConstValType, VALPASSING, ConstVal.Value, ConstVal.FracValue, 0);

    EatTok(SEMICOLONTOK);
  until Tok.Kind <> IDENTTOK;
  end; // CompileConstDeclarations
  
  

  
  procedure CompileTypeDeclarations;
  var
    NameTok: TToken;
    VarType: Integer;
    TypeIndex, FieldIndex: Integer;
  begin
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
         (CompareStr(Types[TypeIndex].TypeIdentName, NameTok.Name) = 0) and
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
  end; // CompileTypeDeclarations
  
  
  
  
  procedure CompileVarDeclarations;
  var
    IdentInListName: array [1..MAXPARAMS] of TString;
    NumIdentInList, IdentInListIndex: Integer;
    VarType: Integer;
  begin
  repeat
    NumIdentInList := 0;
    repeat
      AssertIdent;

      Inc(NumIdentInList);
      IdentInListName[NumIdentInList] := Tok.Name;

      NextTok;

      if Tok.Kind <> COMMATOK then Break;
      NextTok;
    until FALSE;

    EatTok(COLONTOK);

    CompileType(VarType);

    for IdentInListIndex := 1 to NumIdentInList do
      DeclareId(IdentInListName[IdentInListIndex], VARIABLE, 0, VarType, VALPASSING, 0, 0.0, 0);

    EatTok(SEMICOLONTOK);
  until Tok.Kind <> IDENTTOK;

  CheckForwardResolutions;
  end; // CompileVarDeclarations




  procedure CompileProcFuncDeclarations(IsFunction: Boolean);

    
    procedure CompileDirective;
    var
      ImportLibName, ImportFuncName: TString;
      
    begin
    if (CompareStr(Tok.Name, 'EXTERNAL') = 0) then      // External (Windows API) declaration  
      begin
      if BlockStackTop <> 1 then
        Error('External declaration must be global', '', -1);
        
      // Read import library name
      NextTok;      
      ImportLibName := Tok.Name;
      EatTok(STRINGLITERALTOK);      
      
      // Read import function name
      if (Tok.Kind <> IDENTTOK) or (CompareStr(Tok.Name, 'NAME') <> 0) then
        Error('Expected NAME but found ', '', Tok.Kind);
      NextTok;
      ImportFuncName := Tok.Name;
      EatTok(STRINGLITERALTOK);

      // Register import function
      Ident[NumIdent].IsExternal := TRUE;
      Ident[NumIdent].Value := AddImportFunc(ImportLibName, ImportFuncName);
      end
    else if (CompareStr(Tok.Name, 'FORWARD') = 0) then  // Forward declaration
      begin
      Inc(NumBlocks);
      if NumBlocks > MAXBLOCKS then
        Error('Maximum number of blocks exceeded', '', -1);
        
      Ident[NumIdent].ProcAsBlock := NumBlocks;
      Ident[NumIdent].IsUnresolvedForward := TRUE;
      GenerateForwardReference;
      NextTok;
      end
    else
      Error('Unknown directive ', Tok.Name, -1);  
    end; // CompileDirective
    
    
    
  
  var
    ForwardIdentIndex: Integer;

    
  begin // CompileProcFuncDeclarations   
  AssertIdent;

  // Check for forward declaration resolution
  ForwardIdentIndex := GetIdentUnsafe(Tok.Name);
  if ForwardIdentIndex <> 0 then
    if not Ident[ForwardIdentIndex].IsUnresolvedForward or
       (Ident[ForwardIdentIndex].Block <> BlockStack[BlockStackTop]) or
       ((Ident[ForwardIdentIndex].Kind <> PROC) and not IsFunction) or
       ((Ident[ForwardIdentIndex].Kind <> FUNC) and IsFunction) then
     ForwardIdentIndex := 0;                                      // Found an identifier of another kind or scope, or it is already resolved

  if ForwardIdentIndex = 0 then
    begin

    if IsFunction then
      DeclareId(Tok.Name, FUNC, 0, 0, VALPASSING, 0, 0.0, 0)
    else
      DeclareId(Tok.Name, PROC, 0, 0, VALPASSING, 0, 0.0, 0);

    NextTok;

    CompileFormalParametersAndResult(IsFunction, Ident[NumIdent].NumParams, Ident[NumIdent].Param, Ident[NumIdent].DataType);
    end// if ForwardIdentIndex = 0
  else
    NextTok;

  EatTok(SEMICOLONTOK);

  
  if ForwardIdentIndex = 0 then
   if Tok.Kind = IDENTTOK then                                           // External or forward declaration
     CompileDirective 
    else                                                                 // Conventional declaration
      begin
      Inc(NumBlocks);
      if NumTypes > MAXBLOCKS then
        Error('Maximum number of blocks exceeded', '', -1);
      
      Ident[NumIdent].ProcAsBlock := NumBlocks;
      CompileBlock(NumIdent);
      end
  else                                                                   // Forward declaration resolution
    begin
    GenerateForwardResolution(Ident[ForwardIdentIndex].Value);
    CompileBlock(ForwardIdentIndex);
    Ident[ForwardIdentIndex].IsUnresolvedForward := FALSE;
    end;


  EatTok(SEMICOLONTOK);  
  end; // CompileProcFuncDeclarations
  



  procedure CompileDeclarations;
  var
    DeclTok: TToken;
    ParamIndex: Integer;
    NestedProcsFound: Boolean;
   
  begin  
  ParamDataSize := 0;
  LocalDataSize := 0;
  NestedProcsFound := FALSE;
  

  if BlockStack[BlockStackTop] = 1 then             // Main program
    begin
    DeclarePredefinedTypes;
    DeclarePredefinedIdents;
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
    

  // Local declarations
  while (Tok.Kind = CONSTTOK) or (Tok.Kind = TYPETOK) or (Tok.Kind = VARTOK) or (Tok.Kind = PROCEDURETOK) or (Tok.Kind = FUNCTIONTOK) do
    begin
    DeclTok := Tok;
    NextTok;
    
    case DeclTok.Kind of
      CONSTTOK:     
        CompileConstDeclarations;
        
      TYPETOK:      
        CompileTypeDeclarations;
        
      VARTOK:       
        CompileVarDeclarations;
        
      PROCEDURETOK, FUNCTIONTOK:
        begin
        if (BlockStack[BlockStackTop] <> 1) and not NestedProcsFound then
          begin
          NestedProcsFound := TRUE;
          GenerateNestedProcsProlog;
          end;
  
        CompileProcFuncDeclarations(DeclTok.Kind = FUNCTIONTOK);
        end;
    end; // case

    end;// while

    
  // Jump to entry point
  if NestedProcsFound then
    GenerateNestedProcsEpilog;
    
  end; // CompileDeclarations
  
  


  procedure DeleteDeclarations;
  var
    ParamIndex, FieldIndex: Integer;
    
  begin  
  // Delete local identifiers to save space
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
    
  // Delete local types
  while (NumTypes > 0) and (Types[NumTypes].Block = BlockStack[BlockStackTop]) do
    begin
    // If procedural type, delete parameters first
    if Types[NumTypes].TypeKind = PROCEDURALTYPE then
      for ParamIndex := 1 to Types[NumTypes].NumParams do
        Dispose(Types[NumTypes].Param[ParamIndex]); 
    
    // If record, delete fields first
    if Types[NumTypes].TypeKind = RECORDTYPE then
      for FieldIndex := 1 to Types[NumTypes].NumFields do
        Dispose(Types[NumTypes].Field[FieldIndex]);

    // Delete type itself
    Dec(NumTypes);
    end;
      
  end; // DeleteDeclarations




var
  LibProcIdentIndex: Integer;


begin // CompileBlock
Inc(BlockStackTop);

if BlockIdentIndex = 0 then
  BlockStack[BlockStackTop] := 1
else
  BlockStack[BlockStackTop] := Ident[BlockIdentIndex].ProcAsBlock;

CompileDeclarations;

if BlockStack[BlockStackTop] = 1 then          // Main program
  begin
  ProgramEntryPoint := CodeSize;
  GenerateProgramProlog;
  
  // Initialize heap and console I/O
  LibProcIdentIndex := GetIdent('INITSYSTEM');
  GenerateCall(Ident[LibProcIdentIndex].Value, BlockStackTop - 1, Ident[LibProcIdentIndex].NestingLevel);  
  end
else
  GenerateStackFrameProlog(LocalDataSize);     // Procedure or function 

// Block body
GenerateExitProlog;
CompileCompoundStatement(0);
GenerateExitEpilog;                            // Direct all Exit procedure calls here

if ForLoopNesting <> 0 then
  Error('Internal fault: Illegal FOR loop nesting', '', -1);

// If function, return Result value via the EAX register
if (BlockStack[BlockStackTop] <> 1) and (Ident[BlockIdentIndex].Kind = FUNC) then
  begin
  PushVarPtr(Ident[GetIdent('RESULT')].Value, LOCAL, 0, RELOCDATA);
  DerefPtr(Ident[BlockIdentIndex].DataType);
  SaveStackTopToEAX;
  end;

if BlockStack[BlockStackTop] = 1 then          // Main program
  begin
  GenerateProgramEpilog;
  LibProcIdentIndex := GetIdent('EXITPROCESS');  
  PushConst(0);
  GenerateExternalCall(Ident[LibProcIdentIndex].Value, Ident[LibProcIdentIndex].NumParams, FALSE);
  end
else
  begin
  GenerateStackFrameEpilog;
  GenerateReturn(Ident[BlockIdentIndex].NumParams * SizeOf(LongInt), Ident[BlockIdentIndex].NestingLevel);
  end;
  
DeleteDeclarations;  
Dec(BlockStackTop);
end;// CompileBlock




procedure CompileProgram;
begin
NextTok;
EatTok(PROGRAMTOK);
AssertIdent;
NextTok;
CheckTok(SEMICOLONTOK);

EnterIncludedFile('system.inc');
NextTok;

Inc(NumBlocks);
if NumBlocks > MAXBLOCKS then
  Error('Maximum number of blocks exceeded', '', -1);
  
CompileBlock(0);

CheckTok(PERIODTOK);
end;// CompileProgram




