// XD Pascal - a 32-bit compiler for Windows
// Copyright (c) 2009-2010, 2019, Vasiliy Tereshkov

{$APPTYPE CONSOLE}
{$I-}
{$H-}
{$J+}

program XDPW;


uses Common, Scanner, Parser, CodeGen, Linker;



procedure ChangeExt(const InStr, Ext: TString; var OutStr: TString);
var
  i, DotPos: Integer;
begin
DotPos := 0;

for i := Length(InStr) downto 1 do
  if InStr[i] = '.' then
    begin
    DotPos := i;
    Break;
    end;

OutStr := InStr;
if DotPos > 0 then SetLength(OutStr, DotPos);  
OutStr := OutStr + Ext;
end;




var
  UnitName, ExeName: TString;
  ParamIndex: Integer;



begin
WriteLn;
WriteLn('XD Pascal for Windows ', VERSIONMAJOR, '.', VERSIONMINOR);
WriteLn('Copyright (c) 2009-2010, 2019, Vasiliy Tereshkov');

if ParamCount < 1 then
  begin
  WriteLn('Usage: xdpw <unit1.pas> <unit2.pas> ... <prog.pas>');
  Halt(1);
  end;

InitializeCommon;
InitializeLinker;
InitializeCodeGen;
   
for ParamIndex := 0 to ParamCount do
  begin 
  if ParamIndex = 0 then
    UnitName := 'System.pas'
  else
    begin
    UnitName := ParamStr(ParamIndex); 
    WriteLn('Compiling ', UnitName);
    end;

  // Compile
  InitializeScanner(UnitName);
  CompileProgramOrUnit;
  FinalizeScanner;
  end;

ChangeExt(UnitName, 'exe', ExeName);
LinkAndWriteProgram(ExeName);

WriteLn('Compilation complete. Code size: ', GetCodeSize, ' bytes. Data size: ', InitializedGlobalDataSize + UninitializedGlobalDataSize, ' bytes.');
FinalizeCommon;
end.

