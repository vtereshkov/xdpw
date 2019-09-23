// XD Pascal - a 32-bit compiler for Windows
// Copyright (c) 2009-2010, 2019, Vasiliy Tereshkov

// Wrappers for XD Pascal build
  

type
  TString  = string;
  TKeyName = string;
  
  TInFile = file;
  TOutFile = file;
  



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

