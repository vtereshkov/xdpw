// XD Pascal - a 32-bit compiler for Windows
// Copyright (c) 2009-2010, 2019, Vasiliy Tereshkov

// Linker


 
const
  IMGBASE    = $400000;
  SECTALIGN  = $1000;
  FILEALIGN  = $200;
  
  MAXIMPORTS = 64;
  


type
  TMZHeader = record
    MZ: array [0..1] of Char;
    DOSStub: array [0..28] of Word;
    PEHeaderOffset: LongInt;
  end;


  TPEHeader = record
    PE: array [0..3] of Char;
    Machine: Word;
    NumberOfSections: Word;
    TimeDateStamp: LongInt;
    PointerToSymbolTable: LongInt;
    NumberOfSymbols: LongInt;
    SizeOfOptionalHeader: Word;
    Characteristics: Word;
  end;


  TPEOptionalHeader = record
    Magic: Word;
    MajorLinkerVersion: Byte;
    MinorLinkerVersion: Byte;
    SizeOfCode: LongInt;
    SizeOfInitializedData: LongInt;
    SizeOfUninitializedData: LongInt;
    AddressOfEntryPoint: LongInt;
    BaseOfCode: LongInt;
    BaseOfData: LongInt;
    ImageBase: LongInt;
    SectionAlignment: LongInt;
    FileAlignment: LongInt;
    MajorOperatingSystemVersion: Word;
    MinorOperatingSystemVersion: Word;
    MajorImageVersion: Word;
    MinorImageVersion: Word;
    MajorSubsystemVersion: Word;
    MinorSubsystemVersion: Word;
    Win32VersionValue: LongInt;
    SizeOfImage: LongInt;
    SizeOfHeaders: LongInt;
    CheckSum: LongInt;
    Subsystem: Word;
    DllCharacteristics: Word;
    SizeOfStackReserve: LongInt;
    SizeOfStackCommit: LongInt;
    SizeOfHeapReserve: LongInt;
    SizeOfHeapCommit: LongInt;
    LoaderFlags: LongInt;
    NumberOfRvaAndSizes: LongInt;
  end;
  
  
  TDataDirectory = record
    VirtualAddress: LongInt;
    Size: LongInt;
  end;  


  TPESectionHeader = record
    Name: array [0..7] of Char;
    VirtualSize: LongInt;
    VirtualAddress: LongInt;
    SizeOfRawData: LongInt;
    PointerToRawData: LongInt;
    PointerToRelocations: LongInt;
    PointerToLinenumbers: LongInt;
    NumberOfRelocations: Word;
    NumberOfLinenumbers: Word;
    Characteristics: LongInt;
  end;
  
  
  THeaders = record
    MZHeader: TMZHeader;
    PEHeader: TPEHeader;
    PEOptionalHeader: TPEOptionalHeader;
    DataDirectories: array [0..15] of TDataDirectory;
    ImportSectionHeader, DataSectionHeader, CodeSectionHeader: TPESectionHeader;	
  end;
  
  
  TImportFuncName = array [0..31] of Char;
  TImportFuncNames = array [0..255] of TImportFuncName;


  TImportDirectoryTableEntry = record
    Characteristics: LongInt;
    TimeDateStamp: LongInt;
    ForwarderChain: LongInt;
    Name: LongInt;
    FirstThunk: LongInt;
  end; 


  TImportNameTableEntry = record
    Hint: Word;
    Name: TImportFuncName;
  end;


  
  TImportSection = record
    DirectoryTable: array [0..MAXIMPORTS] of TImportDirectoryTableEntry;
    LibraryNames: array [0..MAXIMPORTS - 1, 0..15] of Char;
    LookupTable: array [0..MAXIMPORTS - 1, 0..1] of LongInt;
    NameTable: array [0..MAXIMPORTS - 1] of TImportNameTableEntry;
  end;
  
  
  

var
  Headers: THeaders;  
  ImportSection: TImportSection; 
  
    

 
function Align(size, alignment: Integer): Integer;
begin
Result := ((size + (alignment - 1)) div alignment) * alignment;
end;




procedure Pad(var f: file; size, alignment: Integer);
var
  i: Integer;
  b: Byte;
begin
b := 0;
for i := 0 to Align(size, alignment) - size - 1 do
  BlockWrite(f, b, 1);
end;



  
procedure FillHeaders(CodeSize, InitializedDataSize, UninitializedDataSize: Integer);
var
  DataSize: Integer;
begin
DataSize := InitializedDataSize + UninitializedDataSize;

FillChar(Headers, SizeOf(Headers), #0);

with Headers do
  begin
  
  with MZHeader do
    begin
    MZ[0]                         := 'M';  
    MZ[1]                         := 'Z';
    PEHeaderOffset                := SizeOf(MZHeader);
    end;  
      
  with PEHeader do
    begin  
    PE[0]                         := 'P';  
    PE[1]                         := 'E';
    Machine                       := $14C;                                                // Intel 386
    NumberOfSections              := 3;
    SizeOfOptionalHeader          := SizeOf(PEOptionalHeader) + SizeOf(DataDirectories);
    Characteristics               := $103;                                                // No relocations, executable, 32 bit
    end;

  with PEOptionalHeader do
    begin 
    Magic                         := $10B;                                                // PE32
    AddressOfEntryPoint           := Align(SizeOf(Headers), SECTALIGN) + Align(SizeOf(TImportSection), SECTALIGN) + Align(DataSize, SECTALIGN) + ProgramEntryPoint;
    ImageBase                     := IMGBASE;
    SectionAlignment              := SECTALIGN;
    FileAlignment                 := FILEALIGN;
    MajorOperatingSystemVersion   := 4;
    MajorSubsystemVersion         := 4;
    SizeOfImage                   := Align(SizeOf(Headers), SECTALIGN) + Align(SizeOf(TImportSection), SECTALIGN) + Align(DataSize, SECTALIGN) + Align(CodeSize, SECTALIGN);
    SizeOfHeaders                 := Align(SizeOf(Headers), FILEALIGN);
    Subsystem                     := 2 + IsConsoleProgram;                                // Win32 GUI/console
    SizeOfStackReserve            := $1000000;
    SizeOfStackCommit             := $1000000;
    SizeOfHeapReserve             := $1000000;
    SizeOfHeapCommit              := $1000000;
    NumberOfRvaAndSizes           := 16;
    end;

  with DataDirectories[1] do                                                              // Import directory
    begin
    VirtualAddress                := Align(SizeOf(Headers), SECTALIGN);
    Size                          := SizeOf(TImportSection);
    end;

  with ImportSectionHeader do
    begin
    Name[0]                       := '.';
    Name[1]                       := 'i';
    Name[2]                       := 'd';
    Name[3]                       := 'a';
    Name[4]                       := 't';
    Name[5]                       := 'a';
    VirtualSize                   := SizeOf(TImportSection);
    VirtualAddress                := Align(SizeOf(Headers), SECTALIGN);
    SizeOfRawData                 := Align(SizeOf(TImportSection), FILEALIGN);
    PointerToRawData              := Align(SizeOf(Headers), FILEALIGN);
    Characteristics               := LongInt($C0000040);                                  // Data, readable, writable
    end;

  with DataSectionHeader do
    begin
    Name[0]                       := '.';
    Name[1]                       := 'd';
    Name[2]                       := 'a';
    Name[3]                       := 't';
    Name[4]                       := 'a';
    VirtualSize                   := DataSize;
    VirtualAddress                := Align(SizeOf(Headers), SECTALIGN) + Align(SizeOf(TImportSection), SECTALIGN);
    SizeOfRawData                 := Align(InitializedDataSize, FILEALIGN);
    PointerToRawData              := Align(SizeOf(Headers), FILEALIGN) + Align(SizeOf(TImportSection), FILEALIGN);
    Characteristics               := LongInt($C00000C0);                                  // Data, readable, writable
    end;

  with CodeSectionHeader do
    begin
    Name[0]                       := '.';
    Name[1]                       := 't';
    Name[2]                       := 'e';
    Name[3]                       := 'x';
    Name[4]                       := 't';
    VirtualSize                   := CodeSize;
    VirtualAddress                := Align(SizeOf(Headers), SECTALIGN) + Align(SizeOf(TImportSection), SECTALIGN) + Align(DataSize, SECTALIGN);
    SizeOfRawData                 := Align(CodeSize, FILEALIGN);
    PointerToRawData              := Align(SizeOf(Headers), FILEALIGN) + Align(SizeOf(TImportSection), FILEALIGN) + Align(InitializedDataSize, FILEALIGN);
    Characteristics               := LongInt($60000020);                                  // Code, executable, readable
    end;
    
  end;
  
end;





function AddImportFunc(const ImportLibName, ImportFuncName: TString): LongInt;
var
  VirtualAddress: LongInt;
begin
VirtualAddress := Align(SizeOf(Headers), SECTALIGN);

ImportSection.DirectoryTable[NumImports].Name        := VirtualAddress + SizeOf(ImportSection.DirectoryTable)
                                                                       + SizeOf(ImportSection.LibraryNames[0]) * NumImports;
                                                                       
ImportSection.DirectoryTable[NumImports].FirstThunk  := VirtualAddress + SizeOf(ImportSection.DirectoryTable)
                                                                       + SizeOf(ImportSection.LibraryNames)
                                                                       + SizeOf(ImportSection.LookupTable[0]) * NumImports;

Move(ImportLibName[STRINGFIRSTINDEX], ImportSection.LibraryNames[NumImports], Length(ImportLibName));

ImportSection.LookupTable[NumImports, 0] := VirtualAddress + SizeOf(ImportSection.DirectoryTable)
                                                           + SizeOf(ImportSection.LibraryNames)
                                                           + SizeOf(ImportSection.LookupTable)
                                                           + SizeOf(ImportSection.NameTable[0]) * NumImports;                                              

Move(ImportFuncName[STRINGFIRSTINDEX], ImportSection.NameTable[NumImports].Name, Length(ImportFuncName));

Result := IMGBASE + VirtualAddress + LongInt(@ImportSection.LookupTable[NumImports, 0]) - LongInt(@ImportSection);
Inc(NumImports);
end;
  
    

