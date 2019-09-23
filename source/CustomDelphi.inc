// XD Pascal - a 32-bit compiler for Windows
// Copyright (c) 2009-2010, 2019, Vasiliy Tereshkov

// Wrappers for Delphi/Free Pascal build


{$APPTYPE CONSOLE}
{$I-}


type
  TString  = string [80];
  TKeyName = string [80];
  
  TInFile = file;
  TOutFile = file;




procedure CustomParamStr(Index: Integer; var Str: TString);
begin  
Str := ParamStr(Index);
end;
