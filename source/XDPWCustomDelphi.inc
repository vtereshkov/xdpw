// XD Pascal - a 32-bit compiler for Windows
// Developed by Vasiliy Tereshkov, 2009-2010, 2019

// Wrapper for Delphi/Free Pascal build


{$APPTYPE CONSOLE}
{$ALIGN 1}
{$I-}



uses SysUtils;



const
  STRINGFIRSTINDEX = 1;



type
  TString  = string [80];
  TKeyName = string [80];
  
  TInFile = file;
  TOutFile = file;
  



procedure CustomAppendStr(var s1: TString; const s2: TString);
begin
s1 := s1 + s2;
end;


  

procedure CustomAppendStrChar(var s: TString; ch: Char);
begin
s := s + ch;
end;




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

