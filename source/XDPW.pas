// XD Pascal - a 32-bit compiler for Windows
// Copyright (c) 2009-2010, 2019-2020, Vasiliy Tereshkov

{$APPTYPE CONSOLE}
{$I-}
{$H-}
{$J+}

program XDPW;


uses Common, Scanner, Parser, CodeGen, Linker;




procedure SplitPath(const Path: TString; var Folder, Name, Ext: TString);
var
  DotPos, SlashPos, i: Integer;
begin
Folder := '';  
Name := Path;  
Ext := '';

DotPos := 0;  
SlashPos := 0;

for i := Length(Path) downto 1 do
  if (Path[i] = '.') and (DotPos = 0) then 
    DotPos := i
  else if (Path[i] = '\') and (SlashPos = 0) then
    SlashPos := i; 

if DotPos > 0 then
  begin
  Name := Copy(Path, 1, DotPos - 1);
  Ext  := Copy(Path, DotPos, Length(Path) - DotPos + 1);
  end;
  
if SlashPos > 0 then
  begin
  Folder := Copy(Path, 1, SlashPos);
  Name   := Copy(Path, SlashPos + 1, Length(Name) - SlashPos);
  end;  

end;




procedure ErrorProc(const Msg: TString);
begin
if NumUnits >= 1 then
  WriteLn('Error ', ScannerFileName, ' ', ScannerLine, ': ', Msg)
else
  WriteLn('Error: ', Msg);  

repeat FinalizeScanner until not RestoreScanner;
FinalizeCommon;
Halt(1);
end;




var
  PasPath, PasFolder, PasName, PasExt, ExePath: TString;
  


begin
WriteLn;
WriteLn('XD Pascal for Windows ', VERSIONMAJOR, '.', VERSIONMINOR);
WriteLn('Copyright (c) 2009-2010, 2019-2020, Vasiliy Tereshkov');

if ParamCount < 1 then
  begin
  WriteLn('Usage: xdpw <file.pas>');
  Halt(1);
  end;

PasPath := TString(ParamStr(1));
SplitPath(PasPath, PasFolder, PasName, PasExt);

SetErrorProc(@ErrorProc);

InitializeCommon;
InitializeLinker;
InitializeCodeGen;

SourceFolder := PasFolder;
UnitsFolder  := 'units\';
   
CompileProgramOrUnit('system.pas');
CompileProgramOrUnit(PasName + PasExt);

ExePath := PasFolder + PasName + '.exe';
LinkAndWriteProgram(ExePath);

WriteLn('Compilation complete. Code size: ', GetCodeSize, ' bytes. Data size: ', InitializedGlobalDataSize + UninitializedGlobalDataSize, ' bytes.');

repeat FinalizeScanner until not RestoreScanner;
FinalizeCommon;
end.

