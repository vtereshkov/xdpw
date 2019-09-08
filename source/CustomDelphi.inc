// XD Pascal - a 32-bit compiler for Windows
// Copyright (c) 2009-2010, 2019, Vasiliy Tereshkov

// Wrappers for Delphi/Free Pascal build


{$APPTYPE CONSOLE}
{$ALIGN 1}
{$I-}



const
  STRINGFIRSTINDEX = 1;



type
  TString  = string [80];
  TKeyName = string [80];
  
  TInFile = file;
  TOutFile = file;




procedure CustomParamStr(Index: Integer; var Str: TString);
begin  
Str := ParamStr(Index);
end;




procedure CustomReset(var F: TInFile; const Name: TString);
begin
Assign(F, Name);
Reset(F, 1);
end;




procedure CustomRewrite(var F: TOutFile; const Name: TString);
begin
Assign(F, Name);
Rewrite(F, 1);
end;

