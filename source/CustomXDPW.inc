// XD Pascal - a 32-bit compiler for Windows
// Copyright (c) 2009-2010, 2019, Vasiliy Tereshkov

// Wrappers for XD Pascal build



const
  STRINGFIRSTINDEX = 0;
  
  

type
  TString  = string;
  TKeyName = string;
  
  TInFile = file;
  TOutFile = file;
  



procedure CustomAppendStr(var s1: TString; const s2: TString);
begin
AppendStr(s1, s2);
end;



  
procedure CustomAppendStrChar(var s: TString; ch: Char);
var
  Len: Integer;
begin
Len := Length(s);
s[Len] := ch;
s[Len + 1] := #0;
end;




procedure CustomParamStr(Index: Integer; var Str: TString);
begin  
ParseCmdLine(Index, Str);
end;




procedure CustomReset(var F: TInFile; const Name: TString);
begin
Reset(F, Name);
end;




procedure CustomRewrite(var F: TOutFile; const Name: TString);
begin
Rewrite(F, Name);
end;

