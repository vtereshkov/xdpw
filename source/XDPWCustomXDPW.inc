// XD Pascal - a 32-bit compiler for Windows
// Developed by Vasiliy Tereshkov, 2009-2010, 2019

// Wrapper for XD Pascal build



const
  STRINGFIRSTINDEX = 0;
  
  


type
  TString  = string;
  TKeyName = string;
  
  TInFile = file;
  TOutFile = file;
  


  
procedure CustomAppendStrChar(var s: TString; ch: Char);
var
  Len: Integer;
begin
Len := Length(s);
s[Len] := ch;
s[Len + 1] := #0;
end;




procedure CustomChangeExt(const InStr, Ext: TString; var OutStr: TString);
var
  i, DotPos: Integer;
begin
i := Length(InStr) - 1;
DotPos := -1;

while (i >= 0) and (DotPos = -1) do
  begin
  if InStr[i] = '.' then DotPos := i;
  Dec(i);
  end;

OutStr := InStr;

if DotPos >= 0 then
  OutStr[DotPos + 1] := #0;
  
AppendStr(OutStr, Ext);
end;




procedure CustomParamStr(Index: Integer; var Str: TString);
var
  Count: Integer;
begin  
Count := ParseCmdLine(Index, Str);
end;




procedure CustomReset(var F: TInFile; const Name: TString);
begin
Reset(F, Name);
end;




procedure CustomRewrite(var F: TOutFile; const Name: TString);
begin
Rewrite(F, Name);
end;

