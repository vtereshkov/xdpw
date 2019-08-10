// XD Pascal - a 32-bit compiler for Windows
// Developed by Vasiliy Tereshkov, 2009-2010, 2019

// Main program



program XDPW;


// uses SysUtils;       // For debug purposes only


{$I XDPWCustom.inc}
{$I XDPWCommon.inc}
{$I XDPWScanner.inc}
{$I XDPWCodeGen.inc}
{$I XDPWLinker.inc}
{$I XDPWOptimizer.inc}
{$I XDPWParser.inc}




procedure ZeroAll;
begin
NumIdent := 0; NumTypes := 0; NumImports := 0; NumBlocks := 0; BlockStackTop := 0; CodeSize := 0; CodePosStackTop := 0;
NumStaticStrChars := 0;
GlobalDataSize := 0;
ProgramEntryPoint := 0;
Clear(@CallGraph, SizeOf(CallGraph));
end;




var
  ExeName: TString;
  OutFile: TOutFile;
  BlockIndex: Integer;




begin
WriteLn;
WriteLn('XD Pascal for Windows ', VERSIONMAJOR, '.', VERSIONMINOR);
WriteLn('Developed by Vasiliy Tereshkov, 2009-2010, 2019');
WriteLn;

if ParamCount < 1 then
  begin
  WriteLn('Usage: xdpw <file.pas>');
  WriteLn;
  Halt(1);
  end;  
  
CustomParamStr(1, ProgramName);  

FillKeywords;

for BlockIndex := 1 to MAXBLOCKS do
  BlockIsNotDead[BlockIndex] := FALSE;
  
IsConsoleProgram := 1;  // Console program by default  


// First pass: compile the program and build the call graph
CodeSectionOrigin := 0;
DataSectionOrigin := 0;
VarDataOrigin := 0;

ZeroAll;
Pass := CALLDETERMPASS;
InitScanner;
CompileProgram;
Close(InFile);


// Visit the call graph nodes and mark all procedures that are called as not dead
MarkBlockNotDead(1);


// Second pass: compile the program and calculate code and data sizes (BlockIsNotDead array is preserved)
CodeSectionOrigin := 0;
DataSectionOrigin := 0;
VarDataOrigin := NumStaticStrChars;

ZeroAll;
Pass := SIZEDETERMPASS;
InitScanner;
CompileProgram;
Close(InFile);

FillHeaders(CodeSize, NumStaticStrChars, GlobalDataSize);
Clear(@ImportSection, SizeOf(ImportSection));


// Third pass: compile the program and generate output (BlockIsNotDead array is preserved)
CodeSectionOrigin := IMAGEBASE + Headers.CodeSectionHeader.VirtualAddress;
DataSectionOrigin := IMAGEBASE + Headers.DataSectionHeader.VirtualAddress;
VarDataOrigin := NumStaticStrChars;

ZeroAll;
Pass := CODEGENERATIONPASS;
InitScanner;
CompileProgram;
Close(InFile);

// Write output file
CustomChangeExt(ProgramName, 'exe', ExeName);
CustomRewrite(OutFile, ExeName);

if IOResult <> 0 then
  Error('Unable to open output file ', ExeName, -1);
  
CustomBlockWrite(OutFile, @Headers, SizeOf(Headers));
Pad(OutFile, SizeOf(Headers), FILEALIGNMENT);

CustomBlockWrite(OutFile, @Code, CodeSize);
Pad(OutFile, CodeSize, FILEALIGNMENT);

CustomBlockWrite(OutFile, @StaticStringData, NumStaticStrChars);
Pad(OutFile, NumStaticStrChars, FILEALIGNMENT);

CustomBlockWrite(OutFile, @ImportSection, SizeOf(ImportSection));
Pad(OutFile, SizeOf(ImportSection), FILEALIGNMENT);  
  
Close(OutFile);


WriteLn('Compilation complete. Code size: ', CodeSize, ' bytes. Data size: ', NumStaticStrChars + GlobalDataSize, ' bytes.');
WriteLn;
end.

