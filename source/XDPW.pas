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
  ProgramName, ExeName: TString;
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
FillOperatorSets;
FillTypeSets;
 
IsConsoleProgram := 1;  // Console program by default  
DataSectionOrigin := IMGBASE + Align(SizeOf(Headers), SECTALIGN) + Align(SizeOf(TImportSection), SECTALIGN);

ZeroAll;
FillChar(ImportSection, SizeOf(ImportSection), #0);

InitializeScanner(ProgramName);
CompileProgram;
FinalizeScanner;

FillHeaders(CodeSize, NumStaticStrChars, GlobalDataSize);
Relocate(IMGBASE + Headers.CodeSectionHeader.VirtualAddress, 
         IMGBASE + Headers.DataSectionHeader.VirtualAddress + NumStaticStrChars);


// Write output file
ChangeExt(ProgramName, 'exe', ExeName);
Assign(OutFile, ExeName);
Rewrite(OutFile, 1);

if IOResult <> 0 then
  Error('Unable to open output file ', ExeName, EMPTYTOK);
  
BlockWrite(OutFile, Headers, SizeOf(Headers));
Pad(OutFile, SizeOf(Headers), FILEALIGN);

BlockWrite(OutFile, ImportSection, SizeOf(ImportSection));
Pad(OutFile, SizeOf(ImportSection), FILEALIGN);

BlockWrite(OutFile, StaticStringData, NumStaticStrChars);
Pad(OutFile, NumStaticStrChars, FILEALIGN);

BlockWrite(OutFile, Code, CodeSize);
Pad(OutFile, CodeSize, FILEALIGN);
  
Close(OutFile);


WriteLn('Compilation complete. Code size: ', CodeSize, ' bytes. Data size: ', NumStaticStrChars + GlobalDataSize, ' bytes.');
end.

