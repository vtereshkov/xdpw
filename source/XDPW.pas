// XD Pascal - a 32-bit compiler for Windows
// Developed by Vasiliy Tereshkov, 2009-2010, 2019

// Main program



program XDPW;



{$I XDPWCustom.inc}
{$I XDPWCommon.inc}
{$I XDPWScanner.inc}
{$I XDPWCodeGen.inc}
{$I XDPWLinker.inc}
{$I XDPWParser.inc}




procedure ZeroAll;
begin
NumIdent          := 0; 
NumTypes          := 0; 
NumImports        := 0; 
NumBlocks         := 0; 
BlockStackTop     := 0; 
NumRelocs         := 0;
ForLoopNesting    := 0;
NumExitCalls      := 0;
CodeSize          := 0; 
CodePosStackTop   := 0;
NumStaticStrChars := 0;
GlobalDataSize    := 0;
ProgramEntryPoint := 0;

Clear(@ImportSection, SizeOf(ImportSection));
end;




var
  ExeName: TString;
  OutFile: TOutFile;



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
IsConsoleProgram := 1;  // Console program by default  
DataSectionOrigin := IMAGEBASE + Align(SizeOf(Headers), SECTIONALIGNMENT) + Align(SizeOf(TImportSection), SECTIONALIGNMENT);

ZeroAll;
InitializeScanner;
CompileProgram;
FinalizeScanner;

FillHeaders(CodeSize, NumStaticStrChars, GlobalDataSize);
Relocate(IMAGEBASE + Headers.CodeSectionHeader.VirtualAddress, 
         IMAGEBASE + Headers.DataSectionHeader.VirtualAddress + NumStaticStrChars);


// Write output file
CustomChangeExt(ProgramName, 'exe', ExeName);
CustomRewrite(OutFile, ExeName);

if IOResult <> 0 then
  Error('Unable to open output file ', ExeName, -1);
  
CustomBlockWrite(OutFile, @Headers, SizeOf(Headers));
Pad(OutFile, SizeOf(Headers), FILEALIGNMENT);

CustomBlockWrite(OutFile, @ImportSection, SizeOf(ImportSection));
Pad(OutFile, SizeOf(ImportSection), FILEALIGNMENT);

CustomBlockWrite(OutFile, @StaticStringData, NumStaticStrChars);
Pad(OutFile, NumStaticStrChars, FILEALIGNMENT);

CustomBlockWrite(OutFile, @Code, CodeSize);
Pad(OutFile, CodeSize, FILEALIGNMENT);
  
Close(OutFile);


WriteLn('Compilation complete. Code size: ', CodeSize, ' bytes. Data size: ', NumStaticStrChars + GlobalDataSize, ' bytes.');
WriteLn;
end.

