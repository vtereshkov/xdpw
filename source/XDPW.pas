// XD Pascal - a 32-bit compiler for Windows
// Copyright (c) 2009-2010, 2019, Vasiliy Tereshkov

// Main program



program XDPW;


{$I Custom.inc}
{$I Common.inc}
{$I Scanner.inc}
{$I CodeGen.inc}
{$I Linker.inc}
{$I Parser.inc}




procedure ZeroAll;
begin
NumIdent          := 0; 
NumTypes          := 0; 
NumImports        := 0; 
NumBlocks         := 0; 
BlockStackTop     := 0; 
NumRelocs         := 0;
ForLoopNesting    := 0;
CodeSize          := 0; 
CodePosStackTop   := 0;
NumStaticStrChars := 0;
GlobalDataSize    := 0;
ProgramEntryPoint := 0;

FillChar(ImportSection, SizeOf(ImportSection), #0);
end;




procedure ChangeExt(const InStr, Ext: TString; var OutStr: TString);
var
  i, DotPos: Integer;
begin
DotPos := -1;

for i := STRINGFIRSTINDEX + Length(InStr) - 1 downto STRINGFIRSTINDEX do
  if InStr[i] = '.' then
    begin
    DotPos := i;
    Break;
    end;

OutStr := InStr;
if DotPos >= 0 then SetLength(OutStr, DotPos - STRINGFIRSTINDEX + 1);  
CustomAppendStr(OutStr, Ext);
end;




var
  ExeName: TString;
  OutFile: TOutFile;



begin
WriteLn;
WriteLn('XD Pascal for Windows ', VERSIONMAJOR, '.', VERSIONMINOR);
WriteLn('Copyright (c) 2009-2010, 2019, Vasiliy Tereshkov');

if ParamCount < 1 then
  begin
  WriteLn('Usage: xdpw <file.pas>');
  Halt(1);
  end;  
  
CustomParamStr(1, ProgramName);


// Compile
WriteLn('Compiling ', ProgramName);

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
ChangeExt(ProgramName, 'exe', ExeName);
CustomRewrite(OutFile, ExeName);

if IOResult <> 0 then
  Error('Unable to open output file ', ExeName, -1);
  
BlockWrite(OutFile, Headers, SizeOf(Headers));
Pad(OutFile, SizeOf(Headers), FILEALIGNMENT);

BlockWrite(OutFile, ImportSection, SizeOf(ImportSection));
Pad(OutFile, SizeOf(ImportSection), FILEALIGNMENT);

BlockWrite(OutFile, StaticStringData, NumStaticStrChars);
Pad(OutFile, NumStaticStrChars, FILEALIGNMENT);

BlockWrite(OutFile, Code, CodeSize);
Pad(OutFile, CodeSize, FILEALIGNMENT);
  
Close(OutFile);


WriteLn('Compilation complete. Code size: ', CodeSize, ' bytes. Data size: ', NumStaticStrChars + GlobalDataSize, ' bytes.');
end.

