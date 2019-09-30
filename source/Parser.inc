// XD Pascal - a 32-bit compiler for Windows
// Copyright (c) 2009-2010, 2019, Vasiliy Tereshkov

// Parser



procedure CompileConstExpression(var ConstVal: TConst; var ConstValType: Integer); forward;
procedure CompileDesignator(var ValType: Integer; ForceCharToString: Boolean); forward;
procedure CompileExpression(var ValType: Integer; ForceCharToString: Boolean); forward;
procedure CompileStatement(LoopNesting: Integer); forward;
procedure CompileType(var DataType: Integer); forward;




function AllocateTempStorage(Size: Integer): Integer;
begin
TempDataSize := TempDataSize + Size;    
Result := -LocalDataSize - TempDataSize;
end; // AllocateTempStorage




procedure CompileConstFactor(var ConstVal: TConst; var ConstValType: Integer);
var
  IdentIndex: Integer;
begin
case Tok.Kind of
  IDENTTOK:
    begin
    IdentIndex := GetIdent(Tok.Name);
    if Ident[IdentIndex].Kind <> CONSTANT then
      Error('Constant expected but ' + Ident[IdentIndex].Name + ' found')
    else
      begin
      ConstValType := Ident[IdentIndex].DataType;
      if Types[ConstValType].Kind = REALTYPE then
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
  Error('Expression expected but ' + GetTokSpelling(Tok.Kind) + ' found');
end;// case

end;// CompileConstFactor




procedure CompileConstTerm(var ConstVal: TConst; var ConstValType: Integer);
var
  OpTok: TToken;
  RightConstVal: TConst;
  RightConstValType: Integer;

begin
CompileConstFactor(ConstVal, ConstValType);

while Tok.Kind in MultiplicativeOperators do
  begin
  OpTok := Tok;
  NextTok;
  CompileConstFactor(RightConstVal, RightConstValType);

  // Try to convert integer to real
  if ConversionToRealIsPossible(ConstValType, RightConstValType) then
    begin
    ConstVal.FracValue := ConstVal.Value;
    ConstValType := REALTYPEINDEX;
    end;
  if ConversionToRealIsPossible(RightConstValType, ConstValType) then
    begin
    RightConstVal.FracValue := RightConstVal.Value;
    RightConstValType := REALTYPEINDEX;
    end;

  // Special case: real division of two integers
  if (OpTok.Kind = DIVTOK) and ConversionToRealIsPossible(ConstValType, REALTYPEINDEX) and ConversionToRealIsPossible(RightConstValType, REALTYPEINDEX) then
    begin
    ConstVal.FracValue := ConstVal.Value;
    RightConstVal.FracValue := RightConstVal.Value;
    ConstValType := REALTYPEINDEX;
    RightConstValType := REALTYPEINDEX;
    end;

  ConstValType := GetCompatibleType(ConstValType, RightConstValType);
  CheckOperator(OpTok, ConstValType);

  if Types[ConstValType].Kind = REALTYPE then        // Real constants
    case OpTok.Kind of
      MULTOK:  ConstVal.FracValue := ConstVal.FracValue * RightConstVal.FracValue;
      DIVTOK:  if RightConstVal.FracValue <> 0 then
                 ConstVal.FracValue := ConstVal.FracValue / RightConstVal.FracValue
               else
                 Error('Constant division by zero')
    end
  else                                                    // Integer constants
    case OpTok.Kind of             
      MULTOK:  ConstVal.Value := ConstVal.Value  *  RightConstVal.Value;
      IDIVTOK: if RightConstVal.Value <> 0 then
                 ConstVal.Value := ConstVal.Value div RightConstVal.Value
               else
                 Error('Constant division by zero');  
      MODTOK:  if RightConstVal.Value <> 0 then
                 ConstVal.Value := ConstVal.Value mod RightConstVal.Value
               else
                 Error('Constant division by zero');
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
if UnaryOpTok.Kind in UnaryOperators then
  NextTok;

CompileConstTerm(ConstVal, ConstValType);

if UnaryOpTok.Kind in UnaryOperators then
  CheckOperator(UnaryOpTok, ConstValType);

if UnaryOpTok.Kind = MINUSTOK then      // Unary minus
  if Types[ConstValType].Kind = REALTYPE then
    ConstVal.FracValue := -ConstVal.FracValue
  else
    ConstVal.Value := -ConstVal.Value;

while Tok.Kind in AdditiveOperators do
  begin
  OpTok := Tok;
  NextTok;
  CompileConstTerm(RightConstVal, RightConstValType);

  // Try to convert integer to real
  if ConversionToRealIsPossible(ConstValType, RightConstValType) then
    begin
    ConstVal.FracValue := ConstVal.Value;
    ConstValType := REALTYPEINDEX;
    end;
  if ConversionToRealIsPossible(RightConstValType, ConstValType) then
    begin
    RightConstVal.FracValue := RightConstVal.Value;
    RightConstValType := REALTYPEINDEX;
    end;  

  ConstValType := GetCompatibleType(ConstValType, RightConstValType);
  CheckOperator(OpTok, ConstValType);

  if Types[ConstValType].Kind = REALTYPE then       // Real constants
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
  RightConstValType: Integer;
  Yes: Boolean;

begin
Yes := FALSE;
CompileSimpleConstExpression(ConstVal, ConstValType);

if Tok.Kind in RelationOperators then
  begin
  OpTok := Tok;
  NextTok;
  CompileSimpleConstExpression(RightConstVal, RightConstValType);

  // Try to convert integer to real
  if ConversionToRealIsPossible(ConstValType, RightConstValType) then
    begin
    ConstVal.FracValue := ConstVal.Value;
    ConstValType := REALTYPEINDEX;
    end;
  if ConversionToRealIsPossible(RightConstValType, ConstValType) then
    begin
    RightConstVal.FracValue := RightConstVal.Value;
    RightConstValType := REALTYPEINDEX;
    end;

  GetCompatibleType(ConstValType, RightConstValType);
  CheckOperator(OpTok, ConstValType);

  if Types[ConstValType].Kind = REALTYPE then
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




procedure CompilePredefinedProc(proc: TPredefProc; LoopNesting: Integer);


  function GetReadProcIdent(DataType: Integer): Integer;
  begin
  Result := 0;

  if (Types[DataType].Kind = INTEGERTYPE) or
    ((Types[DataType].Kind = SUBRANGETYPE) and (Types[Types[DataType].HostType].Kind = INTEGERTYPE)) then
        Result := GetIdent('READINT')                 // Integer argument
        
  else if (Types[DataType].Kind = SMALLINTTYPE) or
    ((Types[DataType].Kind = SUBRANGETYPE) and (Types[Types[DataType].HostType].Kind = SMALLINTTYPE)) then
        Result := GetIdent('READSMALLINT')            // Small integer argument
        
  else if (Types[DataType].Kind = SHORTINTTYPE) or
    ((Types[DataType].Kind = SUBRANGETYPE) and (Types[Types[DataType].HostType].Kind = SHORTINTTYPE)) then
        Result := GetIdent('READSHORTINT')            // Short integer argument
        
  else if (Types[DataType].Kind = WORDTYPE) or
    ((Types[DataType].Kind = SUBRANGETYPE) and (Types[Types[DataType].HostType].Kind = WORDTYPE)) then
        Result := GetIdent('READWORD')                // Word argument

  else if (Types[DataType].Kind = BYTETYPE) or
    ((Types[DataType].Kind = SUBRANGETYPE) and (Types[Types[DataType].HostType].Kind = BYTETYPE)) then
        Result := GetIdent('READBYTE')                // Byte argument
       
  else if (Types[DataType].Kind = BOOLEANTYPE) or
    ((Types[DataType].Kind = SUBRANGETYPE) and (Types[Types[DataType].HostType].Kind = BOOLEANTYPE)) then
        Result := GetIdent('READBOOLEAN')             // Boolean argument
  
  else if (Types[DataType].Kind = CHARTYPE) or
    ((Types[DataType].Kind = SUBRANGETYPE) and (Types[Types[DataType].HostType].Kind = CHARTYPE)) then
        Result := GetIdent('READCH')                  // Character argument
        
  else if Types[DataType].Kind = REALTYPE then
        Result := GetIdent('READREAL')                // Real argument
        
  else if (Types[DataType].Kind = ARRAYTYPE) and (Types[DataType].BaseType = CHARTYPEINDEX) then
        Result := GetIdent('READSTRING')              // String argument
        
  else
    Error('Incompatible types');
 
  end; // GetReadProcIdent
  
  
  
  function GetWriteProcIdent(DataType: Integer): Integer;
  begin
  Result := 0;
  
  if (Types[DataType].Kind in IntegerTypes) or
    ((Types[DataType].Kind = SUBRANGETYPE) and (Types[Types[DataType].HostType].Kind in IntegerTypes)) then
        Result := GetIdent('WRITEINTF')                 // Integer argument
        
  else if (Types[DataType].Kind = BOOLEANTYPE) or
    ((Types[DataType].Kind = SUBRANGETYPE) and (Types[Types[DataType].HostType].Kind = BOOLEANTYPE)) then
        Result := GetIdent('WRITEBOOLEANF')             // Boolean argument
        
  else if Types[DataType].Kind = REALTYPE then
        Result := GetIdent('WRITEREALF')                // Real argument
        
  else if Types[DataType].Kind = POINTERTYPE then
        Result := GetIdent('WRITEPOINTERF')             // Pointer argument
        
  else if (Types[DataType].Kind = ARRAYTYPE) and (Types[DataType].BaseType = CHARTYPEINDEX) then
        Result := GetIdent('WRITESTRINGF')              // String argument
        
  else
    Error('Incompatible types');
  
  end; // GetWriteProcIdentIndex
  
 

var
  Formatter: TConst;
  DesignatorType, ExpressionType, FormatterType: Integer;
  LibProcIdentIndex, ConsoleIndex: Integer;
  IsFirstParam, FileSpecified: Boolean;
  
  
begin // CompilePredefinedProc
NextTok;

case proc of
  INCPROC, DECPROC:
    begin
    EatTok(OPARTOK);
    AssertIdent;
    CompileDesignator(DesignatorType, FALSE);
    GetCompatibleType(DesignatorType, INTEGERTYPEINDEX);
    GenerateIncDec(proc, TypeSize(DesignatorType));
    EatTok(CPARTOK);
    end;

  READPROC, READLNPROC:
    begin
    ConsoleIndex := GetIdent('STDINPUTFILE');
    FileSpecified := FALSE;                       // By default, use standard input device, i.e. console
    IsFirstParam := TRUE;

    if Tok.Kind = OPARTOK then
      begin
      NextTok;
      repeat
        // 1st argument - file handle
        if FileSpecified then
          DuplicateStackTop
        else
          PushVarPtr(Ident[ConsoleIndex].Value, 
                     Ident[ConsoleIndex].Scope, 
                     BlockStackTop - Ident[ConsoleIndex].NestingLevel, 
                     Ident[ConsoleIndex].RelocType);

        // 2nd argument - stream handle
        PushConst(0);
        
        // 3rd argument - designator
        CompileDesignator(DesignatorType, FALSE);

        if Types[DesignatorType].Kind = FILETYPE then           // Text file handle
          begin
          if not IsFirstParam then
            Error('Incompatible types');
          FileSpecified := TRUE;
          end
        else                                                        // Any output expression
          begin
          // Call the specific output subroutine. Interface: FileHandle; StreamHandle; var Designator
          LibProcIdentIndex := GetReadProcIdent(DesignatorType);          
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
        DuplicateStackTop
      else
        PushVarPtr(Ident[ConsoleIndex].Value, 
                   Ident[ConsoleIndex].Scope, 
                   BlockStackTop - Ident[ConsoleIndex].NestingLevel, 
                   Ident[ConsoleIndex].RelocType);
        
      // 2nd argument - stream handle
      PushConst(0);        

      GenerateCall(Ident[LibProcIdentIndex].Value, BlockStackTop - 1, Ident[LibProcIdentIndex].NestingLevel);
      end;
      
    // Remove first 3 arguments if they correspond to a file variable 
    if FileSpecified then
      DiscardStackTop(3);

    end;// READPROC, READLNPROC


  WRITEPROC, WRITELNPROC:
    begin
    ConsoleIndex := GetIdent('STDOUTPUTFILE');
    FileSpecified := FALSE;                       // By default, use standard output device, i.e. console
    IsFirstParam := TRUE;

    if Tok.Kind = OPARTOK then
      begin
      NextTok;
      repeat
        // 1st argument - file handle
        if FileSpecified then
          DuplicateStackTop
        else
          PushVarPtr(Ident[ConsoleIndex].Value, 
                     Ident[ConsoleIndex].Scope, 
                     BlockStackTop - Ident[ConsoleIndex].NestingLevel, 
                     Ident[ConsoleIndex].RelocType);

        // 2nd argument - stream handle
        PushConst(0);
        
        // 3rd argument - expression
        CompileExpression(ExpressionType, TRUE);
        
        if Types[ExpressionType].Kind = FILETYPE then           // Text file handle
          begin
          if not IsFirstParam then
            Error('Incompatible types');
          FileSpecified := TRUE;
          end
        else                                                    // Any output expression
          begin
          // 4th argument - minimum width
          if Tok.Kind = COLONTOK then
            begin
            NextTok;
            CompileConstExpression(Formatter, FormatterType);
            GetCompatibleType(FormatterType, INTEGERTYPEINDEX);
            
            PushConst(Formatter.Value);
            
            // 5th argument - number of decimal places
            if (Tok.Kind = COLONTOK) and (Types[ExpressionType].Kind = REALTYPE) then
              begin
              NextTok;
              CompileConstExpression(Formatter, FormatterType);
              GetCompatibleType(FormatterType, INTEGERTYPEINDEX);
              
              PushConst(Formatter.Value);
              end
            else
              PushConst(0);
 
            end            
          else
            begin
            PushConst(0);
            PushConst(0);
            end;
          
          // Call the specific output subroutine. Interface: FileHandle; StreamHandle; Expression; MinWidth; DecPlaces
          LibProcIdentIndex := GetWriteProcIdent(ExpressionType);
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
        DuplicateStackTop
      else
        PushVarPtr(Ident[ConsoleIndex].Value, 
                   Ident[ConsoleIndex].Scope, 
                   BlockStackTop - Ident[ConsoleIndex].NestingLevel, 
                   Ident[ConsoleIndex].RelocType);
        
      // 2nd argument - stream handle
      PushConst(0);         

      GenerateCall(Ident[LibProcIdentIndex].Value, BlockStackTop - 1, Ident[LibProcIdentIndex].NestingLevel);
      end;

    // Remove first 3 arguments if they correspond to a file variable 
    if FileSpecified then
      DiscardStackTop(3);
    
    end;// WRITEPROC, WRITELNPROC
    

  NEWPROC, DISPOSEPROC:
    begin
    EatTok(OPARTOK);
    AssertIdent;
    CompileDesignator(DesignatorType, FALSE);
    GetCompatibleType(DesignatorType, POINTERTYPEINDEX);
    
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
      Error('BREAK outside of loop is not allowed');
    GenerateBreakCall(LoopNesting);
    end;
  

  CONTINUEPROC:
    begin
    if LoopNesting < 1 then
      Error('CONTINUE outside of loop is not allowed');
    GenerateContinueCall(LoopNesting);
    end;
    
    
  EXITPROC:
    GenerateExitCall;
  

  HALTPROC:
    begin
    if Tok.Kind = OPARTOK then
      begin
      NextTok;
      CompileExpression(ExpressionType, FALSE);
      GetCompatibleType(ExpressionType, INTEGERTYPEINDEX);
      EatTok(CPARTOK);
      end
    else
      PushConst(0);
      
    LibProcIdentIndex := GetIdent('EXITPROCESS');
    GenerateExternalCall(Ident[LibProcIdentIndex].Value, Ident[LibProcIdentIndex].NumParams);
    end;

end;// case

end;// CompilePredefinedProc




procedure CompilePredefinedFunc(func: TPredefProc; var ValType: Integer);
var
  IdentIndex: Integer;

begin
NextTok;
EatTok(OPARTOK);

case func of

  SIZEOFFUNC:
    begin
    AssertIdent;
    if FieldInsideWithFound(Tok.Name) then                // Record field inside a WITH block
      begin
      CompileDesignator(ValType, FALSE);
      DiscardStackTop(1);
      PushConst(TypeSize(ValType));      
      end
    else                                                  // Ordinary identifier
      begin  
      IdentIndex := GetIdent(Tok.Name);
      if Ident[IdentIndex].Kind = USERTYPE then
        begin
        NextTok;
        PushConst(TypeSize(Ident[IdentIndex].DataType));
        end
      else
        begin
        CompileDesignator(ValType, FALSE);
        DiscardStackTop(1);
        PushConst(TypeSize(ValType));
        end;
      end;
    ValType := INTEGERTYPEINDEX;
    end;
    

  ROUNDFUNC, TRUNCFUNC:
    begin
    CompileExpression(ValType, FALSE);

    // Try to convert integer to real
    if ConversionToRealIsPossible(ValType, REALTYPEINDEX) then
      begin
      GenerateFloat(0);
      ValType := REALTYPEINDEX;
      end;

    GetCompatibleType(ValType, REALTYPEINDEX);
    GenerateRound(func = TRUNCFUNC);
    ValType := INTEGERTYPEINDEX;
    end;
    

  ORDFUNC:
    begin
    CompileExpression(ValType, FALSE);
    if not (Types[ValType].Kind in OrdinalTypes) then
      Error('Ordinal type expected');
    ValType := INTEGERTYPEINDEX;
    end;
    

  CHRFUNC:
    begin
    CompileExpression(ValType, FALSE);
    GetCompatibleType(ValType, INTEGERTYPEINDEX);
    ValType := CHARTYPEINDEX;
    end;
    

  PREDFUNC, SUCCFUNC:
    begin
    CompileExpression(ValType, FALSE);
    if not (Types[ValType].Kind in OrdinalTypes) then
      Error('Ordinal type expected');
    if func = SUCCFUNC then
      PushConst(1)
    else
      PushConst(-1);
    GenerateBinaryOperator(PLUSTOK, INTEGERTYPEINDEX);
    end;
    

  ABSFUNC, SQRFUNC, SINFUNC, COSFUNC, ARCTANFUNC, EXPFUNC, LNFUNC, SQRTFUNC:
    begin
    CompileExpression(ValType, FALSE);
    if (func = ABSFUNC) or (func = SQRFUNC) then                          // Abs and Sqr accept real or integer parameters
      begin
      if not ((Types[ValType].Kind in NumericTypes) or
             ((Types[ValType].Kind = SUBRANGETYPE) and (Types[Types[ValType].HostType].Kind in NumericTypes))) then
        Error('Numeric type expected')
      end
    else
      begin
      
      // Try to convert integer to real
      if ConversionToRealIsPossible(ValType, REALTYPEINDEX) then
        begin
        GenerateFloat(0);
        ValType := REALTYPEINDEX;
        end;

      GetCompatibleType(ValType, REALTYPEINDEX);
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
      Error('Maximum number of types exceeded');    
    
    Types[NumTypes].Kind := FORWARDTYPE;
    Types[NumTypes].TypeIdentName := Tok.Name;
    Types[NumTypes].Block := BlockStack[BlockStackTop];
    DataType := NumTypes;
    end
  else
    begin
    if Ident[IdentIndex].Kind <> USERTYPE then
      Error('Type name expected');
    DataType := Ident[IdentIndex].DataType;                 // Usual type
    end;
end; // case

NextTok;
end; // CompileTypeIdent
  



procedure CompileFormalParametersAndResult(IsFunction: Boolean; var NumFormalParams: Integer; var FormalParam: PParams; var ResultType: Integer);
var
  IdentInListName: array [1..MAXPARAMS] of TString;
  NumIdentInList, IdentInListIndex: Integer;  
  ParamType: Integer;    
  ListPassMethod: TPassMethod;
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
        // Add new anonymous type 0..0 for array index
        Inc(NumTypes);
        if NumTypes > MAXTYPES then
          Error('Maximum number of types exceeded');
        
        Types[NumTypes].Kind        := SUBRANGETYPE;
        Types[NumTypes].Block       := BlockStack[BlockStackTop];
        Types[NumTypes].HostType    := INTEGERTYPEINDEX;
        Types[NumTypes].Low         := 0;
        Types[NumTypes].High        := 0;
        
        // Add new anonymous type for array itself
        Inc(NumTypes);
        if NumTypes > MAXTYPES then
          Error('Maximum number of types exceeded');
        
        Types[NumTypes].Kind        := ARRAYTYPE;
        Types[NumTypes].Block       := BlockStack[BlockStackTop];
        Types[NumTypes].BaseType    := ParamType;
        Types[NumTypes].IndexType   := NumTypes - 1;
        Types[NumTypes].IsOpenArray := TRUE;
        
        ParamType := NumTypes;
        end;  
      end
    else                                              // Untyped parameters (CONST or VAR only) 
      ParamType := ANYTYPEINDEX;

    
    if (ListPassMethod = VALPASSING) and (Types[ParamType].Kind in StructuredTypes) then
      Error('Structured parameters cannot be passed by value');
      
    if (ListPassMethod = VALPASSING) and (ParamType = ANYTYPEINDEX) then
      Error('Untyped parameters cannot be passed by value');          

    for IdentInListIndex := 1 to NumIdentInList do
      begin
      Inc(NumFormalParams);

      if NumFormalParams > MAXPARAMS then
        Error('Too many formal parameters');

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
  end;
end; // CompileFormalParametersAndResult




procedure CompileActualParameters(NumFormalParams: Integer; var FormalParam: PParams; ResultType: Integer);
var
  NumActualParams: Integer;
  ActualParamType: Integer;
  TempStorageAddr: Integer;
  IsRefParam: Boolean;
  CurParam: PParam;
begin
NumActualParams := 0;

if Tok.Kind = OPARTOK then                            // Actual parameter list found
  begin
  NextTok;
  
  if Tok.Kind <> CPARTOK then
    repeat
      if NumActualParams + 1 > NumFormalParams then
        Error('Too many actual parameters');

      CurParam := FormalParam[NumActualParams + 1];

      // Evaluate actual parameters and push them onto the stack
      if Types[CurParam^.DataType].Kind in StructuredTypes + [ANYTYPE] then
        IsRefParam := (CurParam^.PassMethod = CONSTPASSING) or
                      (CurParam^.PassMethod = VARPASSING)                   // For structured parameters, CONST is equivalent to VAR
      else
        IsRefParam := CurParam^.PassMethod = VARPASSING;                    // For scalar parameters, CONST is equivalent to passing by value

      if IsRefParam and (CurParam^.DataType <> STRINGTYPEINDEX) then
        CompileDesignator(ActualParamType, FALSE)
      else
        CompileExpression(ActualParamType, CurParam^.DataType = STRINGTYPEINDEX);

      Inc(NumActualParams);

      // Try to convert integer to real
      if ConversionToRealIsPossible(ActualParamType, CurParam^.DataType) and not IsRefParam then
        begin
        GenerateFloat(0);
        ActualParamType := REALTYPEINDEX;
        end;
      
      if IsRefParam then  // Strict type checking for parameters passed by reference, except for open array parameters and untyped parameters
        GetCompatibleRefType(CurParam^.DataType, ActualParamType)
      else                // Relaxed type checking for parameters passed by value      
        GetCompatibleType(CurParam^.DataType, ActualParamType);
        
    if Tok.Kind <> COMMATOK then Break;
    NextTok;
    until FALSE;

  EatTok(CPARTOK);
  end;// if Tok.Kind = OPARTOK

if NumActualParams < NumFormalParams then
  Error('Too few actual parameters');
  
// Allocate space for structured Result as a hidden VAR parameter
if ResultType <> 0 then
  if Types[ResultType].Kind in StructuredTypes then
    begin
    TempStorageAddr := AllocateTempStorage(TypeSize(ResultType));
    PushVarPtr(TempStorageAddr, LOCAL, 0, UNINITDATARELOC);
    end;
end;// CompileActualParameters




procedure CompileCall(IdentIndex: Integer);
var
  ResultType: Integer;
begin
if Ident[IdentIndex].Kind = FUNC then
  ResultType := Ident[IdentIndex].DataType
else  
  ResultType := 0;
  
CompileActualParameters(Ident[IdentIndex].NumParams, Ident[IdentIndex].Param, ResultType);
  
if Ident[IdentIndex].IsExternal then
  GenerateExternalCall(Ident[IdentIndex].Value, Ident[IdentIndex].NumParams)
else 
  GenerateCall(Ident[IdentIndex].Value, BlockStackTop - 1, Ident[IdentIndex].NestingLevel);
end; // CompileCall




procedure CompileIndirectCall(ProcVarType: Integer; FunctionOnly: Boolean);
var
  TotalNumParams: Integer;
begin
if (Types[ProcVarType].Kind <> PROCEDURALTYPE) or (FunctionOnly and (Types[ProcVarType].ResultType = 0)) then 
  Error('Appropriate procedural variable expected'); 

DerefPtr(ProcVarType);

CompileActualParameters(Types[ProcVarType].NumParams, Types[ProcVarType].Param, Types[ProcVarType].ResultType);

TotalNumParams := Types[ProcVarType].NumParams;
if Types[Types[ProcVarType].ResultType].Kind in StructuredTypes then
  Inc(TotalNumParams);  //   Allocate space for structured Result as a hidden VAR parameter

GenerateIndirectCall(TotalNumParams);
end; // CompileIndirectCall




procedure CompileFieldInsideWith(var ValType: Integer);
var
  FieldIndex: Integer;
  RecType: Integer;
  TempStorageAddr: Integer;
begin 
ValType := 0;

FieldIndex := GetFieldInsideWith(TempStorageAddr, RecType, Tok.Name);
  
if FieldIndex <> 0 then
  begin
  PushVarPtr(TempStorageAddr, LOCAL, 0, UNINITDATARELOC);
  DerefPtr(POINTERTYPEINDEX);
  
  GetFieldPtr(RecType, FieldIndex);
  ValType := Types[RecType].Field[FieldIndex]^.DataType;    
  end;
end; // CompileFieldInsideWith  




procedure CompileDesignator{(var ValType: Integer; ForceCharToString: Boolean)};
var
  IdentIndex, FieldIndex: Integer;
  ArrayIndexType: Integer;
  TempStorageAddr: LongInt;
  IsRefParam: Boolean;
  
begin
AssertIdent;

// First search among records in WITH blocks
CompileFieldInsideWith(ValType);

// If unsuccessful, search among ordinary variables
if ValType = 0 then
  begin
  IdentIndex := GetIdent(Tok.Name);

  if Ident[IdentIndex].Kind <> VARIABLE then
    Error('Variable expected but ' + GetTokSpelling(Tok.Kind) + ' found');
    
  PushVarPtr(Ident[IdentIndex].Value, 
             Ident[IdentIndex].Scope, 
             BlockStackTop - Ident[IdentIndex].NestingLevel, 
             Ident[IdentIndex].RelocType);
  
             
  if ForceCharToString and (Types[Ident[IdentIndex].DataType].Kind = CHARTYPE) then    // Special case: character as string
    begin
    DerefPtr(Ident[IdentIndex].DataType);
    
    TempStorageAddr := AllocateTempStorage(2 * SizeOf(Char));    
    PushVarPtr(TempStorageAddr, LOCAL, 0, UNINITDATARELOC);
    
    GetCharAsTempString;    
    ValType := STRINGTYPEINDEX;
    end
  else                                                                              // General rule
    ValType := Ident[IdentIndex].DataType;


  if Types[Ident[IdentIndex].DataType].Kind in StructuredTypes + [ANYTYPE] then
    IsRefParam := Ident[IdentIndex].PassMethod in [CONSTPASSING, VARPASSING]    // For structured parameters, CONST is equivalent to VAR
  else
    IsRefParam := Ident[IdentIndex].PassMethod = VARPASSING;                    // For scalar parameters, CONST is equivalent to passing by value

  if IsRefParam then DerefPtr(POINTERTYPEINDEX);                                // Parameter is passed by reference
  end; // if

  
// Apply dereferencing, indexing, field accessing
NextTok;

while Tok.Kind in [DEREFERENCETOK, OBRACKETTOK, PERIODTOK] do
  case Tok.Kind of
  
    DEREFERENCETOK:                                   // Pointer dereferencing
      begin
      if (Types[ValType].Kind <> POINTERTYPE) or (Types[ValType].BaseType = ANYTYPEINDEX) then
        Error('Typed pointer expected');
      DerefPtr(ValType);
      ValType := Types[ValType].BaseType;
      NextTok;
      end;
    
    OBRACKETTOK:                                      // Array element access
      begin
      repeat
        if Types[ValType].Kind <> ARRAYTYPE then
          Error('Array expected');
        NextTok;
        CompileExpression(ArrayIndexType, FALSE);     // Array index
        GetCompatibleType(ArrayIndexType, Types[ValType].IndexType);
        GetArrayElementPtr(ValType);
        ValType := Types[ValType].BaseType;
      until Tok.Kind <> COMMATOK;
      EatTok(CBRACKETTOK);
      end;
      
    PERIODTOK:                                        // Record field access
      begin
      if Types[ValType].Kind <> RECORDTYPE then
        Error('Record expected');
      NextTok;
      AssertIdent;
      FieldIndex := GetField(ValType, Tok.Name);
      GetFieldPtr(ValType, FieldIndex);
      ValType := Types[ValType].Field[FieldIndex]^.DataType;
      NextTok;   
      end;
    
  end; // case
  
end; // CompileDesignator




procedure CompileSetConstructor(var ValType: Integer);
var
  ElementType: Integer;
  LibProcIdentIndex: Integer;
  TempStorageAddr: Integer;
  
begin
// Add new anonymous type
Inc(NumTypes);
if NumTypes > MAXTYPES then
  Error('Maximum number of types exceeded');

Types[NumTypes].Kind := SETTYPE;
Types[NumTypes].Block := BlockStack[BlockStackTop];
Types[NumTypes].BaseType := ANYTYPEINDEX;
ValType := NumTypes;

// Allocate temporary storage
TempStorageAddr := AllocateTempStorage(TypeSize(ValType));
PushVarPtr(TempStorageAddr, LOCAL, 0, UNINITDATARELOC);

// Initialize set
LibProcIdentIndex := GetIdent('INITSET');
GenerateCall(Ident[LibProcIdentIndex].Value, BlockStackTop - 1, Ident[LibProcIdentIndex].NestingLevel); 

// Compile constructor
LibProcIdentIndex := GetIdent('ADDTOSET');
NextTok;

if Tok.Kind <> CBRACKETTOK then
  repeat
    PushVarPtr(TempStorageAddr, LOCAL, 0, UNINITDATARELOC);
    
    CompileExpression(ElementType, FALSE);
    
    if Types[ValType].BaseType = ANYTYPEINDEX then
      begin
      if not (Types[ElementType].Kind in OrdinalTypes) then
        Error('Ordinal type expected');        
      Types[ValType].BaseType := ElementType;
      end  
    else  
      GetCompatibleType(ElementType, Types[ValType].BaseType);

    if Tok.Kind = RANGETOK then
      begin
      NextTok;
      CompileExpression(ElementType, FALSE);    
      GetCompatibleType(ElementType, Types[ValType].BaseType);
      end
    else
      PushConst(-1);
      
    GenerateCall(Ident[LibProcIdentIndex].Value, BlockStackTop - 1, Ident[LibProcIdentIndex].NestingLevel);        
      
    if Tok.Kind <> COMMATOK then Break;
    NextTok;
  until FALSE;
  
EatTok(CBRACKETTOK);

PushVarPtr(TempStorageAddr, LOCAL, 0, UNINITDATARELOC);
end; // CompileSetConstructor




procedure CompileFactor(var ValType: Integer; ForceCharToString: Boolean);


  procedure CompileDereferenceOrCall(var ValType: Integer);
  begin
  if Tok.Kind = OPARTOK then                     // Procedural variable call - parentheses required even for empty parameter list
    begin
    CompileIndirectCall(ValType, TRUE);
    RestoreStackTopFromEAX;
    ValType := Types[ValType].ResultType;
    end       
  else                                           // Usual variable
    if not (Types[ValType].Kind in StructuredTypes) then // Factors of type 'array', 'record', 'set' should contain a pointer to them
      DerefPtr(ValType);
  end; // CompileDereferenceOrCall
  
  
var
  IdentIndex: Integer;
  FictitiousIntegerPtr: ^Integer;
  TempStorageAddr: LongInt;
  NotOpTok: TToken;

  
begin // CompileFactor
case Tok.Kind of

  IDENTTOK:
    if FieldInsideWithFound(Tok.Name) then                                              // Record field inside a WITH block
      begin
      CompileDesignator(ValType, ForceCharToString);
      CompileDereferenceOrCall(ValType);
      end      
    else                                                                                // Ordinary identifier
      begin
      IdentIndex := GetIdent(Tok.Name);

      case Ident[IdentIndex].Kind of
      
        GOTOLABEL:
          Error('Expression expected but label ' + Ident[IdentIndex].Name + ' found');
      
        PROC:
          Error('Expression expected but procedure ' + Ident[IdentIndex].Name + ' found');
          
        FUNC:                                                                           // Function call
          if Ident[IdentIndex].PredefProc <> EMPTYPROC then                                    // Predefined function call
            CompilePredefinedFunc(Ident[IdentIndex].PredefProc, ValType)
          else                                                                          // User-defined function call
            begin
            NextTok;
            CompileCall(IdentIndex);
            RestoreStackTopFromEAX;
            ValType := Ident[IdentIndex].DataType;
            end;
            
        VARIABLE:                                                                       // Designator
          begin
          CompileDesignator(ValType, ForceCharToString);
          CompileDereferenceOrCall(ValType);   
          end;
          
        CONSTANT:                                                                       // Constant
          begin
          ValType := Ident[IdentIndex].DataType;
          if Types[ValType].Kind = REALTYPE then
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
          CompileExpression(ValType, FALSE);
          EatTok(CPARTOK);

          if not ((Types[Ident[IdentIndex].DataType].Kind in CastableTypes) and 
                  (Types[ValType].Kind in CastableTypes)) then
            Error('Invalid typecast');

          ValType := Ident[IdentIndex].DataType;
          end
          
      else
        Error('Internal fault: Illegal identifier');  
      end; // case Ident[IdentIndex].Kind
      end; // else  


  ADDRESSTOK:
    begin
    NextTok;
    
    if FieldInsideWithFound(Tok.Name) then                  // Record field inside a WITH block
      CompileDesignator(ValType, FALSE)
    else                                                    // Ordinary identifier
      begin  
      IdentIndex := GetIdent(Tok.Name);
      
      if Ident[IdentIndex].Kind in [PROC, FUNC] then
        begin
        if (Ident[IdentIndex].PredefProc <> EMPTYPROC) or (Ident[IdentIndex].Block <> 1) then
          Error('Procedure or function cannot be predefined or nested');
          
        PushRelocConst(Ident[IdentIndex].Value, CODERELOC); // To be resolved later when code section origin is known
        NextTok;
        end
      else  
        CompileDesignator(ValType, FALSE);
      end;  
      
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
    
    if ForceCharToString then
      begin
      TempStorageAddr := AllocateTempStorage(2 * SizeOf(Char));    
      PushVarPtr(TempStorageAddr, LOCAL, 0, UNINITDATARELOC);
    
      GetCharAsTempString;    
      ValType := STRINGTYPEINDEX;
      end
    else  
      ValType := CHARTYPEINDEX;
      
    NextTok;
    end;


  STRINGLITERALTOK:
    begin
    PushVarPtr(Tok.StrAddress, GLOBAL, 0, INITDATARELOC);
    ValType := STRINGTYPEINDEX;
    NextTok;
    end;


  OPARTOK:       // Expression in parentheses expected
    begin
    NextTok;
    CompileExpression(ValType, ForceCharToString);
    EatTok(CPARTOK);
    end;


  NOTTOK:
    begin
    NotOpTok := Tok;
    NextTok;
    CompileFactor(ValType, ForceCharToString);
    CheckOperator(NotOpTok, ValType);
    GenerateUnaryOperator(NOTTOK, ValType);
    end;
    
    
  OBRACKETTOK:  
    CompileSetConstructor(ValType);
    

  NILTOK:
    begin
    PushConst(0);
    ValType := POINTERTYPEINDEX;
    NextTok;
    end

else
  Error('Expression expected but ' + GetTokSpelling(Tok.Kind) + ' found');
end;// case

end;// CompileFactor




procedure CompileTerm(var ValType: Integer; ForceCharToString: Boolean);
var
  OpTok: TToken;
  RightValType: Integer;
  LibProcIdentIndex: Integer;
  TempStorageAddr: Integer; 
  
begin
CompileFactor(ValType, ForceCharToString);

while Tok.Kind in MultiplicativeOperators do
  begin
  OpTok := Tok;
  NextTok;
  CompileFactor(RightValType, ForceCharToString);

  // Try to convert integer to real
  if ConversionToRealIsPossible(ValType, RightValType) then
    begin
    GenerateFloat(SizeOf(Single));
    ValType := REALTYPEINDEX;
    end;
  if ConversionToRealIsPossible(RightValType, ValType) then
    begin
    GenerateFloat(0);
    RightValType := REALTYPEINDEX;
    end;

  // Special case: real division of two integers
  if (OpTok.Kind = DIVTOK) and ConversionToRealIsPossible(ValType, REALTYPEINDEX) and ConversionToRealIsPossible(RightValType, REALTYPEINDEX) then
    begin
    GenerateFloat(SizeOf(Single));
    GenerateFloat(0);
    ValType := REALTYPEINDEX;
    RightValType := REALTYPEINDEX;
    end;
    
  // Special case: set intersection  
  if (OpTok.Kind = MULTOK) and (Types[ValType].Kind = SETTYPE) then  
    begin
    ValType := GetCompatibleType(ValType, RightValType);
    
    LibProcIdentIndex := GetIdent('SETINTERSECTION');
      
    TempStorageAddr := AllocateTempStorage(TypeSize(ValType));    
    PushVarPtr(TempStorageAddr, LOCAL, 0, UNINITDATARELOC);

    GenerateCall(Ident[LibProcIdentIndex].Value, BlockStackTop - 1, Ident[LibProcIdentIndex].NestingLevel);    
    PushVarPtr(TempStorageAddr, LOCAL, 0, UNINITDATARELOC);
    end
  // General rule  
  else
    begin
    ValType := GetCompatibleType(ValType, RightValType);
    CheckOperator(OpTok, ValType);
    GenerateBinaryOperator(OpTok.Kind, ValType);
    end;
    
  end;// while

end;// CompileTerm




procedure CompileSimpleExpression(var ValType: Integer; ForceCharToString: Boolean);
var
  UnaryOpTok, OpTok: TToken;
  RightValType: Integer;
  LibProcIdentIndex: Integer;
  TempStorageAddr: Integer;
  
  
begin // CompileSimpleExpression
UnaryOpTok := Tok;
if UnaryOpTok.Kind in UnaryOperators then
  NextTok;

CompileTerm(ValType, ForceCharToString);

if UnaryOpTok.Kind in UnaryOperators then
  CheckOperator(UnaryOpTok, ValType);

if UnaryOpTok.Kind = MINUSTOK then GenerateUnaryOperator(MINUSTOK, ValType);     // Unary minus

while Tok.Kind in AdditiveOperators do
  begin
  OpTok := Tok;
  NextTok;
  CompileTerm(RightValType, ForceCharToString);

  // Try to convert integer to real
  if ConversionToRealIsPossible(ValType, RightValType) then
    begin
    GenerateFloat(SizeOf(Single));
    ValType := REALTYPEINDEX;
    end;
  if ConversionToRealIsPossible(RightValType, ValType) then
    begin
    GenerateFloat(0);
    RightValType := REALTYPEINDEX;
    end;
      
  // Special case: string concatenation
  if (ValType = STRINGTYPEINDEX) and (RightValType = STRINGTYPEINDEX) and (OpTok.Kind = PLUSTOK) then
    begin 
    LibProcIdentIndex := GetIdent('CONCATSTR');   

    TempStorageAddr := AllocateTempStorage(TypeSize(STRINGTYPEINDEX));    
    PushVarPtr(TempStorageAddr, LOCAL, 0, UNINITDATARELOC);
    
    GenerateCall(Ident[LibProcIdentIndex].Value, BlockStackTop - 1, Ident[LibProcIdentIndex].NestingLevel);    
    PushVarPtr(TempStorageAddr, LOCAL, 0, UNINITDATARELOC);
    ValType := STRINGTYPEINDEX;
    end
  // Special case: set union or difference  
  else if (OpTok.Kind in [PLUSTOK, MINUSTOK]) and (Types[ValType].Kind = SETTYPE) then  
    begin
    ValType := GetCompatibleType(ValType, RightValType);
    
    if OpTok.Kind = PLUSTOK then
      LibProcIdentIndex := GetIdent('SETUNION')
    else
      LibProcIdentIndex := GetIdent('SETDIFFERENCE');
      
    TempStorageAddr := AllocateTempStorage(TypeSize(ValType));    
    PushVarPtr(TempStorageAddr, LOCAL, 0, UNINITDATARELOC);

    GenerateCall(Ident[LibProcIdentIndex].Value, BlockStackTop - 1, Ident[LibProcIdentIndex].NestingLevel);    
    PushVarPtr(TempStorageAddr, LOCAL, 0, UNINITDATARELOC);
    end  
  // General rule
  else
    begin
    ValType := GetCompatibleType(ValType, RightValType);
    CheckOperator(OpTok, ValType);
    GenerateBinaryOperator(OpTok.Kind, ValType);
    end;
  
  end;// while

end;// CompileSimpleExpression




procedure CompileExpression{(var ValType: Integer; ForceCharToString: Boolean)};
var
  OpTok: TToken;
  RightValType: Integer;
  LibProcIdentIndex: Integer;

  
begin // CompileExpression
CompileSimpleExpression(ValType, ForceCharToString);

if Tok.Kind in RelationOperators then
  begin
  OpTok := Tok;
  NextTok;
  CompileSimpleExpression(RightValType, ForceCharToString);

  // Try to convert integer to real
  if ConversionToRealIsPossible(ValType, RightValType) then
    begin
    GenerateFloat(SizeOf(Single));
    ValType := REALTYPEINDEX;
    end;
  if ConversionToRealIsPossible(RightValType, ValType) then
    begin
    GenerateFloat(0);
    RightValType := REALTYPEINDEX;
    end;
    
  // Special case: string comparison
  if (ValType = STRINGTYPEINDEX) and (RightValType = STRINGTYPEINDEX) then
    begin 
    LibProcIdentIndex := GetIdent('COMPARESTR');
   
    GenerateCall(Ident[LibProcIdentIndex].Value, BlockStackTop - 1, Ident[LibProcIdentIndex].NestingLevel);
    RestoreStackTopFromEAX;
    ValType := Ident[LibProcIdentIndex].DataType;
    
    PushConst(0);
    RightValType := INTEGERTYPEINDEX;
    end;

  // Special case: set comparison
  if (OpTok.Kind in [EQTOK, NETOK]) and (Types[ValType].Kind = SETTYPE) then
    begin
    ValType := GetCompatibleType(ValType, RightValType); 
    
    LibProcIdentIndex := GetIdent('COMPARESETS');
    
    GenerateCall(Ident[LibProcIdentIndex].Value, BlockStackTop - 1, Ident[LibProcIdentIndex].NestingLevel);
    RestoreStackTopFromEAX;
    ValType := Ident[LibProcIdentIndex].DataType;
    
    PushConst(0);
    RightValType := INTEGERTYPEINDEX;
    end;  

  GetCompatibleType(ValType, RightValType);
  CheckOperator(OpTok, ValType);
  ValType := BOOLEANTYPEINDEX;
  GenerateRelation(OpTok.Kind, RightValType);
  end
else if Tok.Kind = INTOK then
  begin
  NextTok;
  CompileSimpleExpression(RightValType, ForceCharToString);
  
  if Types[RightValType].Kind <> SETTYPE then
    Error('Set expected');
  
  if Types[RightValType].BaseType <> ANYTYPEINDEX then
    GetCompatibleType(ValType, Types[RightValType].BaseType)
  else if not (Types[ValType].Kind in OrdinalTypes) then
    Error('Ordinal type expected');   

  LibProcIdentIndex := GetIdent('INSET');
  GenerateCall(Ident[LibProcIdentIndex].Value, BlockStackTop - 1, Ident[LibProcIdentIndex].NestingLevel);
  RestoreStackTopFromEAX;
  ValType := Ident[LibProcIdentIndex].DataType;  
  end;  

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



  procedure CompileLabel;
  var
    LabelIndex: Integer;
  begin
  if Tok.Kind = IDENTTOK then
    begin
    LabelIndex := GetIdentUnsafe(Tok.Name);
    
    if LabelIndex <> 0 then
      if Ident[LabelIndex].Kind = GOTOLABEL then
        begin
        if Ident[LabelIndex].Block <> BlockStack[BlockStackTop] then
          Error('Label is not declared in current procedure');
        
        Ident[LabelIndex].Value := CodeSize;        
        Ident[LabelIndex].IsUnresolvedForward := FALSE;
        Ident[LabelIndex].ForLoopNesting := ForLoopNesting;
        
        NextTok;
        EatTok(COLONTOK);
        end;      
    end;
  end; // CompileLabel
  
  
  
  
  procedure CompileAssignment(DesignatorType: Integer);
  var
    ExpressionType: Integer;
  begin
  NextTok;

  CompileExpression(ExpressionType, DesignatorType = STRINGTYPEINDEX);                          // General rule: right-hand side expression

  // Try to convert integer to real
  if ConversionToRealIsPossible(ExpressionType, DesignatorType) then
    begin
    GenerateFloat(0);
    ExpressionType := REALTYPEINDEX;
    end;

  GetCompatibleType(DesignatorType, ExpressionType);

  if Types[DesignatorType].Kind in StructuredTypes then
    GenerateStructuredAssignment(DesignatorType)
  else
    GenerateAssignment(DesignatorType);
  
  end; // CompileAssignment
  
  
  
  
  procedure CompileAssignmentOrCall(DesignatorType: Integer);
  begin 
  if Tok.Kind = ASSIGNTOK then                                    // Assignment
    CompileAssignment(DesignatorType)
  else                                                            // Procedural variable call                                                              
    begin
    CheckTok(OPARTOK);                                            // Parentheses required even for empty parameter list         
    CompileIndirectCall(DesignatorType, FALSE);
    end;  
  end; // CompileAssignmentOrCall
  

  
  
  procedure CompileIfStatement(LoopNesting: Integer);
  var
    ExpressionType: Integer;
    
  begin
  NextTok;
  
  CompileExpression(ExpressionType, FALSE);
  GetCompatibleType(ExpressionType, BOOLEANTYPEINDEX);
  
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
  end; // CompileIfStatement  



  
  procedure CompileCaseStatement(LoopNesting: Integer);
  var
    SelectorType, ConstValType: Integer;
    NumCaseStatements: Integer;
    ConstVal, ConstVal2: TConst;
    
  begin
  NextTok;
  
  CompileExpression(SelectorType, FALSE);
  if not (Types[SelectorType].Kind in OrdinalTypes) then
    Error('Ordinal variable expected as CASE selector');
  
  EatTok(OFTOK);

  GenerateCaseProlog;  

  NumCaseStatements := 0;

  repeat       // Loop over all cases

    repeat     // Loop over all constants for the current case
      CompileConstExpression(ConstVal, ConstValType);
      GetCompatibleType(ConstValType, SelectorType);

      if Tok.Kind = RANGETOK then                                      // Range check
        begin
        NextTok;
        CompileConstExpression(ConstVal2, ConstValType);
        GetCompatibleType(ConstValType, SelectorType);
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
  end; // CompileCaseStatement
  
  
  
  
  procedure CompileWhileStatement(LoopNesting: Integer);
  var
    ExpressionType: Integer;
    
  begin
  Inc(CodePosStackTop);
  CodePosStack[CodePosStackTop] := CodeSize;      // Save return address used by GenerateWhileEpilog

  NextTok;
  CompileExpression(ExpressionType, FALSE);
  GetCompatibleType(ExpressionType, BOOLEANTYPEINDEX);
  
  EatTok(DOTOK);

  GenerateBreakProlog(LoopNesting);
  GenerateContinueProlog(LoopNesting);
  GenerateWhileCondition;                         // Satisfied if expression is not zero
  GenerateWhileProlog;
  
  CompileStatement(LoopNesting);
  
  GenerateContinueEpilog(LoopNesting);
  GenerateWhileEpilog;
  GenerateBreakEpilog(LoopNesting);
  end; // CompileWhileStatement
  
  
  
  
  procedure CompileRepeatStatement(LoopNesting: Integer);
  var
    ExpressionType: Integer;
    
  begin
  GenerateBreakProlog(LoopNesting);
  GenerateContinueProlog(LoopNesting);
  GenerateRepeatProlog;

  NextTok;
  CompileStatementList(LoopNesting);

  EatTok(UNTILTOK);
  
  GenerateContinueEpilog(LoopNesting);

  CompileExpression(ExpressionType, FALSE);
  GetCompatibleType(ExpressionType, BOOLEANTYPEINDEX);
  
  GenerateRepeatCondition;
  GenerateRepeatEpilog;
  GenerateBreakEpilog(LoopNesting);
  end; // CompileRepeatStatement
  
  
  
  
  procedure CompileForStatement(LoopNesting: Integer);
  var
    CounterIndex: Integer;
    ExpressionType: Integer;
    Down: Boolean;
      
  begin
  NextTok;
  
  AssertIdent;
  CounterIndex := GetIdent(Tok.Name);

  if (Ident[CounterIndex].Kind <> VARIABLE) or
    ((Ident[CounterIndex].NestingLevel <> 1) and (Ident[CounterIndex].NestingLevel <> BlockStackTop)) or
     (Ident[CounterIndex].PassMethod <> VALPASSING) then
    Error('Simple local variable expected as FOR loop counter');

  if not (Types[Ident[CounterIndex].DataType].Kind in OrdinalTypes) then
    Error('Ordinal variable expected as FOR loop counter');

  // Assign initial value to the counter
  NextTok;
  CheckTok(ASSIGNTOK);
  
  PushVarPtr(Ident[CounterIndex].Value, Ident[CounterIndex].Scope, 0, Ident[CounterIndex].RelocType);
  
  CompileAssignment(Ident[CounterIndex].DataType);

  if not (Tok.Kind in [TOTOK, DOWNTOTOK]) then
    CheckTok(TOTOK);

  Down := Tok.Kind = DOWNTOTOK;
  NextTok;
  
  // Compute and save the total number of iterations
  PushVarPtr(Ident[CounterIndex].Value, Ident[CounterIndex].Scope, 0, Ident[CounterIndex].RelocType);
  DerefPtr(Ident[CounterIndex].DataType);
  
  CompileExpression(ExpressionType, FALSE);
  GetCompatibleType(ExpressionType, Ident[CounterIndex].DataType);
  GenerateForNumberOfIterations(Down);
  
  // Save return address used by GenerateForEpilog
  Inc(CodePosStackTop);
  CodePosStack[CodePosStackTop] := CodeSize;

  // Check the remaining number of iterations
  GenerateForCondition;

  EatTok(DOTOK);
  
  GenerateBreakProlog(LoopNesting);
  GenerateContinueProlog(LoopNesting);
  GenerateForProlog;
  
  CompileStatement(LoopNesting);    
  
  GenerateContinueEpilog(LoopNesting);
  
  PushVarPtr(Ident[CounterIndex].Value, 
             Ident[CounterIndex].Scope, 
             0, 
             Ident[CounterIndex].RelocType);
             
  GenerateForEpilog(Ident[CounterIndex].DataType, Down);
  GenerateBreakEpilog(LoopNesting);
  
  // Pop and discard the remaining number of iterations (i.e. zero)
  DiscardStackTop(1);                                          
  end; // CompileForStatement

  
  
  
  procedure CompileGotoStatement(LoopNesting: Integer);
  var
    LabelIndex: Integer;
    
  begin
  NextTok;
  
  AssertIdent;
  LabelIndex := GetIdent(Tok.Name);
  
  if Ident[LabelIndex].Kind <> GOTOLABEL then
    Error('Label expected');
    
  if Ident[LabelIndex].Block <> BlockStack[BlockStackTop] then
    Error('Label is not declared in current procedure');
    
  GenerateGoto(LabelIndex);

  NextTok;
  end; // CompileGotoStatement



  
  procedure CompileWithStatement(LoopNesting: Integer);
  var
    DesignatorType: Integer;
    DeltaWithNesting: Integer;
    TempStorageAddr: Integer;
    
  begin
  NextTok;  
  DeltaWithNesting := 0; 

  repeat   
    // Save designator pointer to temporary storage
    TempStorageAddr := AllocateTempStorage(TypeSize(POINTERTYPEINDEX));    
    PushVarPtr(TempStorageAddr, LOCAL, 0, UNINITDATARELOC);
    
    CompileDesignator(DesignatorType, FALSE);
    if Types[DesignatorType].Kind <> RECORDTYPE then
      Error('Record expected');
      
    GenerateAssignment(POINTERTYPEINDEX);

    // Save designator info
    Inc(DeltaWithNesting);
    Inc(WithNesting);
    
    if WithNesting > MAXWITHNESTING then
      Error('Maximum WITH block nesting exceeded');
    
    WithStack[WithNesting].TempPointer := TempStorageAddr;
    WithStack[WithNesting].DataType := DesignatorType;    
    
    if Tok.Kind <> COMMATOK then Break;
    NextTok;
  until FALSE;
  
  EatTok(DOTOK);
  
  CompileStatement(LoopNesting);
  
  WithNesting := WithNesting - DeltaWithNesting;
  end; // CompileWithStatement



  
var
  IdentIndex, ResultIdentIndex: Integer;
  DesignatorType: Integer;

  
begin // CompileStatement
CompileLabel;

case Tok.Kind of

  IDENTTOK:
    begin   
    if FieldInsideWithFound(Tok.Name) then                              // Record field inside a WITH block
      begin
      CompileDesignator(DesignatorType, FALSE);
      CompileAssignmentOrCall(DesignatorType);
      end 
    else                                                                // Ordinary identifier                                                                                
      begin  
      IdentIndex := GetIdent(Tok.Name);
      
      case Ident[IdentIndex].Kind of
      
        VARIABLE:                                                       // Assignment or procedural variable call
          begin
          CompileDesignator(DesignatorType, FALSE);
          CompileAssignmentOrCall(DesignatorType); 
          end; // VARIABLE        

        PROC, FUNC:                                                     // Procedure or function call (returned result discarded)
          if Ident[IdentIndex].PredefProc <> EMPTYPROC then             // Predefined procedure
            begin
            if Ident[IdentIndex].Kind <> PROC then
              Error('Procedure expected but predefined function ' + Ident[IdentIndex].Name + ' found');            
            CompilePredefinedProc(Ident[IdentIndex].PredefProc, LoopNesting)
            end
          else                                                          // User-defined procedure or function
            begin
            NextTok;
            
            if Tok.Kind = ASSIGNTOK then                                // Special case: assignment to a function name
              begin
              if (Ident[IdentIndex].Kind <> FUNC) or (Ident[IdentIndex].ProcAsBlock <> BlockStack[BlockStackTop]) then
                Error('Current function name expected but ' + Ident[IdentIndex].Name + ' found');

              ResultIdentIndex := GetIdent('RESULT');
              PushVarPtr(Ident[ResultIdentIndex].Value, LOCAL, 0, UNINITDATARELOC);
              
              DesignatorType := Ident[ResultIdentIndex].DataType;
              if Types[DesignatorType].Kind in StructuredTypes then 
                DerefPtr(POINTERTYPEINDEX);                        

              CompileAssignment(DesignatorType);
              end
            else                                                        // General rule: procedure or function call  
              CompileCall(IdentIndex);
              
            end  
              
      else
        Error('Statement expected but ' + Ident[IdentIndex].Name + ' found');
      end // case Ident[IdentIndex].Kind
      end; // else
    end;    

  BEGINTOK:
    CompileCompoundStatement(LoopNesting);    

  IFTOK:
    CompileIfStatement(LoopNesting);    

  CASETOK:
    CompileCaseStatement(LoopNesting);  

  WHILETOK:
    CompileWhileStatement(LoopNesting + 1);

  REPEATTOK:
    CompileRepeatStatement(LoopNesting + 1);    

  FORTOK:
    CompileForStatement(LoopNesting + 1);
    
  GOTOTOK:
    CompileGotoStatement(LoopNesting);

  WITHTOK:
    CompileWithStatement(LoopNesting);

end;// case

end;// CompileStatement




procedure CompileType{(var DataType: Integer)};


  procedure CompileEnumeratedType(var DataType: Integer);
  var
    ConstIndex: Integer;
  begin
  // Add new anonymous type
  Inc(NumTypes);
  if NumTypes > MAXTYPES then
    Error('Maximum number of types exceeded');
    
  Types[NumTypes].Kind := ENUMERATEDTYPE;
  DataType := NumTypes;

  // Compile enumeration constants
  ConstIndex := 0;
  NextTok;
  
  repeat
    AssertIdent;
    DeclareIdent(Tok.Name, CONSTANT, 0, DataType, VALPASSING, ConstIndex, 0.0, EMPTYPROC);
    
    Inc(ConstIndex);
    if ConstIndex > MAXENUMELEMENTS - 1 then
      Error('Too many enumeration elements');
      
    NextTok;
    
    if Tok.Kind <> COMMATOK then Break;
    NextTok;
  until FALSE;
  
  EatTok(CPARTOK);
  
  Types[DataType].Block := BlockStack[BlockStackTop];
  Types[DataType].Low := 0;
  Types[DataType].High := ConstIndex - 1;
  end; // CompileEnumeratedType




  procedure CompileTypedPointerType(var DataType: Integer);
  var
    NestedDataType: Integer;
  begin
  // Add new anonymous type
  Inc(NumTypes);
  if NumTypes > MAXTYPES then
    Error('Maximum number of types exceeded');
    
  Types[NumTypes].Kind := POINTERTYPE;
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
      Error('Maximum number of types exceeded');
    
    Types[NumTypes].Kind := ARRAYTYPE;
    Types[NumTypes].Block := BlockStack[BlockStackTop];
    Types[NumTypes].IsOpenArray := FALSE;
    ArrType := NumTypes;

    CompileType(IndexType);
    if not (Types[IndexType].Kind in OrdinalTypes) then
      Error('Ordinal type expected');
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
      if Types[RecType].Field[i]^.Name = Name then
        Error('Duplicate field ' + Name);

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
    Error('Maximum number of types exceeded');  
  
  Types[NumTypes].Kind := RECORDTYPE;
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




  procedure CompileSetType(var DataType: Integer);
  var
    NestedDataType: Integer;
  begin
  // Add new anonymous type
  Inc(NumTypes);
  if NumTypes > MAXTYPES then
    Error('Maximum number of types exceeded');
  
  Types[NumTypes].Kind := SETTYPE;
  Types[NumTypes].Block := BlockStack[BlockStackTop];
  DataType := NumTypes;
  
  NextTok;
  EatTok(OFTOK);

  CompileType(NestedDataType);
  
  if (LowBound(NestedDataType) < 0) or (HighBound(NestedDataType) > MAXSETELEMENTS - 1) then
    Error('Too many set elements');
  
  Types[DataType].BaseType := NestedDataType; 
  end; // CompileSetType




  procedure CompileSubrangeType(var DataType: Integer);
  var
    ConstVal: TConst;
    LowBoundType, HighBoundType: Integer;
  begin
  // Add new anonymous type
  Inc(NumTypes);
  if NumTypes > MAXTYPES then
    Error('Maximum number of types exceeded');    
  
  Types[NumTypes].Kind := SUBRANGETYPE;
  DataType := NumTypes;

  CompileConstExpression(ConstVal, LowBoundType);                               // Subrange lower bound
  if not (Types[LowBoundType].Kind in OrdinalTypes + [SUBRANGETYPE]) then
    Error('Ordinal type expected');
  Types[DataType].Low := ConstVal.Value;

  EatTok(RANGETOK);

  CompileConstExpression(ConstVal, HighBoundType);                              // Subrange upper bound
  if not (Types[HighBoundType].Kind in OrdinalTypes + [SUBRANGETYPE]) then
    Error('Ordinal type expected');
  Types[DataType].High := ConstVal.Value;

  GetCompatibleType(LowBoundType, HighBoundType);

  if Types[DataType].High < Types[DataType].Low then
    Error('Illegal subrange bounds');

  Types[DataType].HostType := LowBoundType;
  Types[DataType].Block := BlockStack[BlockStackTop];  
  end; // CompileSubrangeType
  
  
  
  
  procedure CompileProceduralType(var DataType: Integer; IsFunction: Boolean);
  begin
  Inc(NumTypes);
  if NumTypes > MAXTYPES then
    Error('Maximum number of types exceeded');    
  
  Types[NumTypes].Kind := PROCEDURALTYPE;
  Types[NumTypes].Block := BlockStack[BlockStackTop];
  DataType := NumTypes;
  
  NextTok;
  
  CompileFormalParametersAndResult(IsFunction, Types[DataType].NumParams, Types[DataType].Param, Types[DataType].ResultType);  
  end; // CompileProceduralType
  
  

var
  IdentIndex: LongInt;
  TypeNameGiven: Boolean;   


begin // CompileType

if Tok.Kind = PACKEDTOK then        // PACKED has no effect
  begin
  NextTok;
  if not (Tok.Kind in [ARRAYTOK, RECORDTOK, SETTOK, FILETOK]) then
    Error('PACKED is not allowed here');
  end;
 
case Tok.Kind of

  OPARTOK:
    CompileEnumeratedType(DataType);
    
  DEREFERENCETOK: 
    CompileTypedPointerType(DataType);
  
  ARRAYTOK:       
    CompileArrayType(DataType); 
 
  RECORDTOK:      
    CompileRecordType(DataType);
    
  SETTOK:      
    CompileSetType(DataType); 
   
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


  procedure DeclarePredefinedIdents;
  begin
  // Constants
  DeclareIdent('TRUE',  CONSTANT, 0, BOOLEANTYPEINDEX, VALPASSING, -1, 0.0, EMPTYPROC);
  DeclareIdent('FALSE', CONSTANT, 0, BOOLEANTYPEINDEX, VALPASSING,  0, 0.0, EMPTYPROC);

  // Types
  DeclareIdent('INTEGER',  USERTYPE, 0, INTEGERTYPEINDEX,  VALPASSING, 0, 0.0, EMPTYPROC);
  DeclareIdent('SMALLINT', USERTYPE, 0, SMALLINTTYPEINDEX, VALPASSING, 0, 0.0, EMPTYPROC);
  DeclareIdent('SHORTINT', USERTYPE, 0, SHORTINTTYPEINDEX, VALPASSING, 0, 0.0, EMPTYPROC);
  DeclareIdent('WORD',     USERTYPE, 0, WORDTYPEINDEX,     VALPASSING, 0, 0.0, EMPTYPROC);
  DeclareIdent('BYTE',     USERTYPE, 0, BYTETYPEINDEX,     VALPASSING, 0, 0.0, EMPTYPROC);  
  DeclareIdent('CHAR',     USERTYPE, 0, CHARTYPEINDEX,     VALPASSING, 0, 0.0, EMPTYPROC);
  DeclareIdent('BOOLEAN',  USERTYPE, 0, BOOLEANTYPEINDEX,  VALPASSING, 0, 0.0, EMPTYPROC);
  DeclareIdent('REAL',     USERTYPE, 0, REALTYPEINDEX,     VALPASSING, 0, 0.0, EMPTYPROC);
  DeclareIdent('POINTER',  USERTYPE, 0, POINTERTYPEINDEX,  VALPASSING, 0, 0.0, EMPTYPROC);

  // Procedures
  DeclareIdent('INC',      PROC, 0, 0, VALPASSING, 0, 0.0, INCPROC);
  DeclareIdent('DEC',      PROC, 0, 0, VALPASSING, 0, 0.0, DECPROC);
  DeclareIdent('READ',     PROC, 0, 0, VALPASSING, 0, 0.0, READPROC);
  DeclareIdent('WRITE',    PROC, 0, 0, VALPASSING, 0, 0.0, WRITEPROC);
  DeclareIdent('READLN',   PROC, 0, 0, VALPASSING, 0, 0.0, READLNPROC);
  DeclareIdent('WRITELN',  PROC, 0, 0, VALPASSING, 0, 0.0, WRITELNPROC);
  DeclareIdent('NEW',      PROC, 0, 0, VALPASSING, 0, 0.0, NEWPROC);
  DeclareIdent('DISPOSE',  PROC, 0, 0, VALPASSING, 0, 0.0, DISPOSEPROC);
  DeclareIdent('BREAK',    PROC, 0, 0, VALPASSING, 0, 0.0, BREAKPROC);
  DeclareIdent('CONTINUE', PROC, 0, 0, VALPASSING, 0, 0.0, CONTINUEPROC);  
  DeclareIdent('EXIT',     PROC, 0, 0, VALPASSING, 0, 0.0, EXITPROC);
  DeclareIdent('HALT',     PROC, 0, 0, VALPASSING, 0, 0.0, HALTPROC);

  // Functions
  DeclareIdent('SIZEOF', FUNC, 0, 0, VALPASSING, 0, 0.0, SIZEOFFUNC);
  DeclareIdent('ORD',    FUNC, 0, 0, VALPASSING, 0, 0.0, ORDFUNC);
  DeclareIdent('CHR',    FUNC, 0, 0, VALPASSING, 0, 0.0, CHRFUNC);
  DeclareIdent('PRED',   FUNC, 0, 0, VALPASSING, 0, 0.0, PREDFUNC);
  DeclareIdent('SUCC',   FUNC, 0, 0, VALPASSING, 0, 0.0, SUCCFUNC);
  DeclareIdent('ROUND',  FUNC, 0, 0, VALPASSING, 0, 0.0, ROUNDFUNC);
  DeclareIdent('TRUNC',  FUNC, 0, 0, VALPASSING, 0, 0.0, TRUNCFUNC);
  DeclareIdent('ABS',    FUNC, 0, 0, VALPASSING, 0, 0.0, ABSFUNC);
  DeclareIdent('SQR',    FUNC, 0, 0, VALPASSING, 0, 0.0, SQRFUNC);
  DeclareIdent('SIN',    FUNC, 0, 0, VALPASSING, 0, 0.0, SINFUNC);
  DeclareIdent('COS',    FUNC, 0, 0, VALPASSING, 0, 0.0, COSFUNC);
  DeclareIdent('ARCTAN', FUNC, 0, 0, VALPASSING, 0, 0.0, ARCTANFUNC);
  DeclareIdent('EXP',    FUNC, 0, 0, VALPASSING, 0, 0.0, EXPFUNC);
  DeclareIdent('LN',     FUNC, 0, 0, VALPASSING, 0, 0.0, LNFUNC);
  DeclareIdent('SQRT',   FUNC, 0, 0, VALPASSING, 0, 0.0, SQRTFUNC);
  end;// DeclarePredefinedIdents




  procedure DeclarePredefinedTypes;
  begin
  NumTypes := STRINGTYPEINDEX;

  Types[ANYTYPEINDEX].Kind      := ANYTYPE;
  Types[INTEGERTYPEINDEX].Kind  := INTEGERTYPE;
  Types[SMALLINTTYPEINDEX].Kind := SMALLINTTYPE;
  Types[SHORTINTTYPEINDEX].Kind := SHORTINTTYPE;
  Types[WORDTYPEINDEX].Kind     := WORDTYPE;  
  Types[BYTETYPEINDEX].Kind     := BYTETYPE;  
  Types[CHARTYPEINDEX].Kind     := CHARTYPE;
  Types[BOOLEANTYPEINDEX].Kind  := BOOLEANTYPE;
  Types[REALTYPEINDEX].Kind     := REALTYPE;
  Types[POINTERTYPEINDEX].Kind  := POINTERTYPE;
  Types[FILETYPEINDEX].Kind     := FILETYPE;
  Types[STRINGTYPEINDEX].Kind   := ARRAYTYPE;

  Types[POINTERTYPEINDEX].BaseType  := ANYTYPEINDEX;
  
  // Add new anonymous type: 1 .. MAXSTRLENGTH + 1
  Inc(NumTypes);
  if NumTypes > MAXTYPES then
    Error('Maximum number of types exceeded');
  
  Types[NumTypes].Kind     := SUBRANGETYPE;
  Types[NumTypes].HostType := INTEGERTYPEINDEX;
  Types[NumTypes].Low      := 1;
  Types[NumTypes].High     := MAXSTRLENGTH + 1;
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
    if (Types[TypeIndex].Kind = FORWARDTYPE) and
       (Types[TypeIndex].Block = BlockStack[BlockStackTop]) then
      Error('Unresolved forward reference to type ' + Types[TypeIndex].TypeIdentName);
  end; // CheckForwardResolutions
  
  
 

  procedure CompileLabelDeclarations;
  begin
  repeat
    AssertIdent;
    
    DeclareIdent(Tok.Name, GOTOLABEL, 0, 0, VALPASSING, 0, 0.0, EMPTYPROC);
    
    NextTok;
    if Tok.Kind <> COMMATOK then Break;
    NextTok;
  until FALSE;
  
  EatTok(SEMICOLONTOK);
  end; // CompileLabelDeclarations


 
  
  procedure CompileConstDeclarations;
  
  
  
    procedure CompileUntypedConstDeclaration(var NameTok: TToken);
    var
      ConstVal: TConst;
      ConstValType: Integer;    
    begin
    EatTok(EQTOK);
    
    CompileConstExpression(ConstVal, ConstValType);
    DeclareIdent(NameTok.Name, CONSTANT, 0, ConstValType, VALPASSING, ConstVal.Value, ConstVal.FracValue, EMPTYPROC);
    end; // CompileUntypedConstDeclaration;
    
    
    
    
    procedure CompileTypedConstDeclaration(var NameTok: TToken);
    
    
      procedure CompileTypedConstConstructor(InitializedDataOffset: LongInt; ConstType: Integer);
      const
        TrueVal: Integer = -1;
      
      var
        ConstVal, ElementVal, ElementVal2: TConst;
        ConstValType, ElementValType: Integer;
        NumElements, ElementIndex, FieldIndex: Integer;
        
      begin
      // Numbers
      if Types[ConstType].Kind in OrdinalTypes + [REALTYPE] then
        begin
        CompileConstExpression(ConstVal, ConstValType);          
        GetCompatibleType(ConstType, ConstValType); 
          
        if Types[ConstType].Kind = REALTYPE then
          Move(ConstVal.FracValue, InitializedGlobalData[InitializedDataOffset], TypeSize(ConstType))
        else
          Move(ConstVal.Value, InitializedGlobalData[InitializedDataOffset], TypeSize(ConstType));
        end
        
      // Arrays
      else if Types[ConstType].Kind = ARRAYTYPE then
        begin
        
        if ConstType = STRINGTYPEINDEX then         // Special case: strings
          begin
          if (Tok.Kind <> CHARLITERALTOK) and (Tok.Kind <> STRINGLITERALTOK) then
            CheckTok(STRINGLITERALTOK);
            
          Move(InitializedGlobalData[Tok.StrAddress], InitializedGlobalData[InitializedDataOffset], TypeSize(ConstType));
          NextTok;
          end
        else                                        // General rule
          begin
          EatTok(OPARTOK);
          
          NumElements := HighBound(Types[ConstType].IndexType) - LowBound(Types[ConstType].IndexType) + 1;
          for ElementIndex := 1 to NumElements do
            begin
            CompileTypedConstConstructor(InitializedDataOffset, Types[ConstType].BaseType);
            InitializedDataOffset := InitializedDataOffset + TypeSize(Types[ConstType].BaseType);
            
            if ElementIndex < NumElements then 
              EatTok(COMMATOK)
            else
              EatTok(CPARTOK);  
            end; // for
          end; // else
 
        end
        
      // Records
      else if Types[ConstType].Kind = RECORDTYPE then
        begin
        EatTok(OPARTOK);
        
        repeat
          AssertIdent;
          FieldIndex := GetField(ConstType, Tok.Name);
          
          NextTok;
          EatTok(COLONTOK);
          
          CompileTypedConstConstructor(InitializedDataOffset + Types[ConstType].Field[FieldIndex]^.Offset, Types[ConstType].Field[FieldIndex]^.DataType);          
          
          if Tok.Kind <> SEMICOLONTOK then Break;
          NextTok; 
        until FALSE;
        
        EatTok(CPARTOK);
        end
        
      // Sets
      else if Types[ConstType].Kind = SETTYPE then
        begin
        EatTok(OBRACKETTOK);
        
        if Tok.Kind <> CBRACKETTOK then
          repeat      
            CompileConstExpression(ElementVal, ElementValType);            
            GetCompatibleType(ElementValType, Types[ConstType].BaseType);

            if Tok.Kind = RANGETOK then
              begin
              NextTok;
              CompileConstExpression(ElementVal2, ElementValType);    
              GetCompatibleType(ElementValType, Types[ConstType].BaseType);
              end
            else
              ElementVal2 := ElementVal;
              
            for ElementIndex := ElementVal.Value to ElementVal2.Value do
              Move(TrueVal, InitializedGlobalData[InitializedDataOffset + ElementIndex], TypeSize(BOOLEANTYPEINDEX));
  
            if Tok.Kind <> COMMATOK then Break;
            NextTok;
          until FALSE;
        
        EatTok(CBRACKETTOK);
        end        
     
      else
        Error('Illegal type');         
 
      end; // CompileTypedConstConstructor  
    
    
    var
      ConstType: Integer;
      
      
    begin // CompileTypedConstDeclaration
    EatTok(COLONTOK);    
    CompileType(ConstType);
    
    DeclareIdent(NameTok.Name, CONSTANT, 0, ConstType, VARPASSING, 0, 0.0, EMPTYPROC);
    
    EatTok(EQTOK);    
    CompileTypedConstConstructor(Ident[NumIdent].Value, ConstType);   
    end; // CompileTypedConstDeclaration    



  
  var
    NameTok: TToken; 
   
  begin // CompileConstDeclarations
  repeat
    AssertIdent;

    NameTok := Tok;
    NextTok;
    
    if Tok.Kind = EQTOK then
      CompileUntypedConstDeclaration(NameTok)
    else
      CompileTypedConstDeclaration(NameTok);

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
    DeclareIdent(NameTok.Name, USERTYPE, 0, VarType, VALPASSING, 0, 0.0, EMPTYPROC);

    // Check if this type was forward-referenced
    for TypeIndex := 1 to NumTypes do
      if (Types[TypeIndex].Kind = FORWARDTYPE) and
         (Types[TypeIndex].TypeIdentName = NameTok.Name) and
         (Types[TypeIndex].Block = BlockStack[BlockStackTop]) then
        begin
        // Forward type reference resolution
        Types[TypeIndex] := Types[VarType];
        if Types[VarType].Kind = RECORDTYPE then
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
      DeclareIdent(IdentInListName[IdentInListIndex], VARIABLE, 0, VarType, VALPASSING, 0, 0.0, EMPTYPROC);

    EatTok(SEMICOLONTOK);
  until Tok.Kind <> IDENTTOK;

  CheckForwardResolutions;
  end; // CompileVarDeclarations




  procedure CompileProcFuncDeclarations(IsFunction: Boolean);

    
    procedure CompileDirective;
    var
      ImportLibName, ImportFuncName: TString;
      
    begin
    if Tok.Name = 'EXTERNAL' then      // External (Windows API) declaration  
      begin
      if BlockStackTop <> 1 then
        Error('External declaration must be global');
        
      // Read import library name
      NextTok;      
      ImportLibName := Tok.Name;
      EatTok(STRINGLITERALTOK);      
      
      // Read import function name
      if (Tok.Kind <> IDENTTOK) or (Tok.Name <> 'NAME') then
        Error('NAME expected but ' + GetTokSpelling(Tok.Kind) + ' found');
      NextTok;
      ImportFuncName := Tok.Name;
      EatTok(STRINGLITERALTOK);

      // Register import function
      Ident[NumIdent].IsExternal := TRUE;
      Ident[NumIdent].Value := AddImportFunc(ImportLibName, ImportFuncName);
      end
    else if Tok.Name = 'FORWARD' then  // Forward declaration
      begin
      Inc(NumBlocks);
      if NumBlocks > MAXBLOCKS then
        Error('Maximum number of blocks exceeded');
        
      Ident[NumIdent].ProcAsBlock := NumBlocks;
      Ident[NumIdent].IsUnresolvedForward := TRUE;
      GenerateForwardReference;
      NextTok;
      end
    else
      Error('Unknown directive ' + Tok.Name);  
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
      DeclareIdent(Tok.Name, FUNC, 0, 0, VALPASSING, 0, 0.0, EMPTYPROC)
    else
      DeclareIdent(Tok.Name, PROC, 0, 0, VALPASSING, 0, 0.0, EMPTYPROC);

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
        Error('Maximum number of blocks exceeded');
      
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
    TotalNumParams: Integer;
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
    // Declare parameters like local variables
    TotalNumParams := Ident[BlockIdentIndex].NumParams;
    if (Ident[BlockIdentIndex].Kind = FUNC) and (Types[Ident[BlockIdentIndex].DataType].Kind in StructuredTypes) then
      Inc(TotalNumParams);                          // Allocate space for structured Result as a hidden VAR parameter
    
    for ParamIndex := 1 to Ident[BlockIdentIndex].NumParams do
      DeclareIdent(Ident[BlockIdentIndex].Param[ParamIndex]^.Name,
                   VARIABLE,
                   TotalNumParams,
                   Ident[BlockIdentIndex].Param[ParamIndex]^.DataType,
                   Ident[BlockIdentIndex].Param[ParamIndex]^.PassMethod,
                   0,
                   0.0,
                   EMPTYPROC);

    // Allocate Result variable if the current block is a function
    if Ident[BlockIdentIndex].Kind = FUNC then
      if Types[Ident[BlockIdentIndex].DataType].Kind in StructuredTypes then    // For functions returning structured variables, Result is a hidden VAR parameter 
        DeclareIdent('RESULT', VARIABLE, TotalNumParams, Ident[BlockIdentIndex].DataType, VARPASSING, 0, 0.0, EMPTYPROC)
      else                                                                      // Otherwise, Result is a hidden local variable
        DeclareIdent('RESULT', VARIABLE, 0, Ident[BlockIdentIndex].DataType, VALPASSING, 0, 0.0, EMPTYPROC);
    end;// else
    

  // Local declarations
  while Tok.Kind in [LABELTOK, CONSTTOK, TYPETOK, VARTOK, PROCEDURETOK, FUNCTIONTOK] do
    begin
    DeclTok := Tok;
    NextTok;
    
    case DeclTok.Kind of
      LABELTOK:
        CompileLabelDeclarations;
        
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
  
  
  
  
  procedure CheckUnresolvedDeclarations;
  var
    IdentIndex: Integer;
  begin
  IdentIndex := NumIdent;
  
  while (IdentIndex > 0) and (Ident[IdentIndex].Block = BlockStack[BlockStackTop]) do
    begin
    if (Ident[IdentIndex].Kind in [GOTOLABEL, PROC, FUNC]) and Ident[IdentIndex].IsUnresolvedForward then
      Error('Unresolved declaration of ' + Ident[IdentIndex].Name);
    Dec(IdentIndex);
    end;
  end; // CheckUnresolvedDeclarations  
  
  


  procedure DeleteDeclarations;
  var
    ParamIndex, FieldIndex: Integer;
    
  begin  
  // Delete local identifiers to save space
  while (NumIdent > 0) and (Ident[NumIdent].Block = BlockStack[BlockStackTop]) do
    begin 
    // If procedure or function, delete parameters first
    if Ident[NumIdent].Kind in [PROC, FUNC] then
      for ParamIndex := 1 to Ident[NumIdent].NumParams do
        Dispose(Ident[NumIdent].Param[ParamIndex]);

    // Delete identifier itself
    Dec(NumIdent);
    end;     
    
  // Delete local types
  while (NumTypes > 0) and (Types[NumTypes].Block = BlockStack[BlockStackTop]) do
    begin
    // If procedural type, delete parameters first
    if Types[NumTypes].Kind = PROCEDURALTYPE then
      for ParamIndex := 1 to Types[NumTypes].NumParams do
        Dispose(Types[NumTypes].Param[ParamIndex]); 
    
    // If record, delete fields first
    if Types[NumTypes].Kind = RECORDTYPE then
      for FieldIndex := 1 to Types[NumTypes].NumFields do
        Dispose(Types[NumTypes].Field[FieldIndex]);

    // Delete type itself
    Dec(NumTypes);
    end;
      
  end; // DeleteDeclarations




var
  LibProcIdentIndex: Integer;
  TotalNumParams: Integer;


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
  GenerateTempStorageProlog;
  
  // Initialize heap and console I/O
  LibProcIdentIndex := GetIdent('INITSYSTEM');
  GenerateCall(Ident[LibProcIdentIndex].Value, BlockStackTop - 1, Ident[LibProcIdentIndex].NestingLevel);  
  end
else
  begin
  GenerateStackFrameProlog(LocalDataSize);     // Procedure or function
  GenerateTempStorageProlog;
  end;

// Initialize variables needed to access temporary strings, sets and WITH designators
TempDataSize := 0; 

// Block body
GenerateGotoProlog;
GenerateExitProlog;

CompileCompoundStatement(0);

CheckUnresolvedDeclarations;

GenerateExitEpilog;                            // Direct all Exit procedure calls here
GenerateGotoEpilog;

if ForLoopNesting <> 0 then
  Error('Internal fault: Illegal FOR loop nesting');

// If function, return Result value via the EAX register
if (BlockStack[BlockStackTop] <> 1) and (Ident[BlockIdentIndex].Kind = FUNC) then
  begin
  PushVarPtr(Ident[GetIdent('RESULT')].Value, LOCAL, 0, UNINITDATARELOC);
  if Types[Ident[BlockIdentIndex].DataType].Kind in StructuredTypes then
    DerefPtr(POINTERTYPEINDEX)
  else  
    DerefPtr(Ident[BlockIdentIndex].DataType);
  SaveStackTopToEAX;
  end;
  
GenerateTempStorageEpilog(TempDataSize);

if BlockStack[BlockStackTop] = 1 then          // Main program
  begin
  GenerateProgramEpilog;    
  LibProcIdentIndex := GetIdent('EXITPROCESS');  
  PushConst(0);
  GenerateExternalCall(Ident[LibProcIdentIndex].Value, Ident[LibProcIdentIndex].NumParams);
  end
else
  begin
  TotalNumParams := Ident[BlockIdentIndex].NumParams;
    if (Ident[BlockIdentIndex].Kind = FUNC) and (Types[Ident[BlockIdentIndex].DataType].Kind in StructuredTypes) then
      Inc(TotalNumParams);                     // Deallocate space allocated for structured Result as a hidden VAR parameter
      
  GenerateStackFrameEpilog;
  GenerateReturn(TotalNumParams * SizeOf(LongInt), Ident[BlockIdentIndex].NestingLevel);
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
  Error('Maximum number of blocks exceeded');
  
CompileBlock(0);

CheckTok(PERIODTOK);
end;// CompileProgram




