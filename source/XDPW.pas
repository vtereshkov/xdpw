// XD Pascal - a 32-bit compiler for Windows
// Copyright (c) 2009-2010, 2019-2020, Vasiliy Tereshkov

{$APPTYPE CONSOLE}
{$I-}
{$H-}
{$J+}

program XDPW;


uses SysUtils, Common, Scanner, Parser, CodeGen, Linker;




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




procedure NoticeProc(const Msg: TString);
begin
WriteLn(Msg);  
end;




procedure ErrorProc(const Msg: TString);
begin
if NumUnits >= 1 then
  Notice('Error ' + ScannerFileName + ' ' + IntToStr(ScannerLine) + ': ' + Msg)
else
  Notice('Error: ' + Msg);  

repeat FinalizeScanner until not RestoreScanner;
FinalizeCommon;
Halt(1);
end;




var
  PasPath, PasFolder, PasName, PasExt, ExePath: TString;
  


begin
SetWriteProcs(@NoticeProc, @ErrorProc);

Notice('XD Pascal for Windows ' + VERSION);
Notice('Copyright (c) 2009-2010, 2019-2020, Vasiliy Tereshkov');

if ParamCount < 1 then
  begin
  Notice('Usage: xdpw <file.pas>');
  Halt(1);
  end;

PasPath := TString(ParamStr(1));
SplitPath(PasPath, PasFolder, PasName, PasExt);

InitializeCommon;
InitializeLinker;
InitializeCodeGen;

SourceFolder := PasFolder;
UnitsFolder  := 'units\';
   
CompileProgramOrUnit('system.pas');
CompileProgramOrUnit(PasName + PasExt);

ExePath := PasFolder + PasName + '.exe';
LinkAndWriteProgram(ExePath);

Notice('Complete. Code size: ' + IntToStr(GetCodeSize) + ' bytes. Data size: ' + IntToStr(InitializedGlobalDataSize + UninitializedGlobalDataSize) + ' bytes');

repeat FinalizeScanner until not RestoreScanner;
FinalizeCommon;
end.

