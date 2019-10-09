// XD Pascal - a 32-bit compiler for Windows
// Copyright (c) 2009-2010, 2019, Vasiliy Tereshkov

// Linker


 
const
  IMGBASE           = $400000;
  SECTALIGN         = $1000;
  FILEALIGN         = $200;
  
  MAXIMPORTLIBS     = 16;
  MAXIMPORTS        = 64;
  

    
type
  TDOSStub = array [0..127] of Byte;
 

  TPEHeader = packed record
    PE: array [0..3] of Char;
    Machine: Word;
    NumberOfSections: Word;
    TimeDateStamp: LongInt;
    PointerToSymbolTable: LongInt;
    NumberOfSymbols: LongInt;
    SizeOfOptionalHeader: Word;
    Characteristics: Word;
  end;


  TPEOptionalHeader = packed record
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
  
  
  TDataDirectory = packed record
    VirtualAddress: LongInt;
    Size: LongInt;
  end;  


  TPESectionHeader = packed record
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
  
  
  THeaders = packed record
    Stub: TDOSStub;
    PEHeader: TPEHeader;
    PEOptionalHeader: TPEOptionalHeader;
    DataDirectories: array [0..15] of TDataDirectory;
    CodeSectionHeader, DataSectionHeader, BSSSectionHeader, ImportSectionHeader: TPESectionHeader;	
  end;
  
  
  TImportFuncName = array [0..31] of Char;


  TImportDirectoryTableEntry = packed record
    Characteristics: LongInt;
    TimeDateStamp: LongInt;
    ForwarderChain: LongInt;
    Name: LongInt;
    FirstThunk: LongInt;
  end; 


  TImportNameTableEntry = packed record
    Hint: Word;
    Name: TImportFuncName;
  end;

  
  TImportSection = packed record
    DirectoryTable: array [0..MAXIMPORTLIBS] of TImportDirectoryTableEntry;
    LibraryNames: array [0..MAXIMPORTLIBS - 1, 0..15] of Char;
    LookupTable: array [0..MAXIMPORTS + MAXIMPORTLIBS - 1] of LongInt;
    NameTable: array [0..MAXIMPORTS - 1] of TImportNameTableEntry;
  end;
  


const
  DOSStub: TDOSStub = 
    (
    $4D, $5A, $90, $00, $03, $00, $00, $00, $04, $00, $00, $00, $FF, $FF, $00, $00,
    $B8, $00, $00, $00, $00, $00, $00, $00, $40, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $80, $00, $00, $00,
    $0E, $1F, $BA, $0E, $00, $B4, $09, $CD, $21, $B8, $01, $4C, $CD, $21, $54, $68,
    $69, $73, $20, $70, $72, $6F, $67, $72, $61, $6D, $20, $63, $61, $6E, $6E, $6F,
    $74, $20, $62, $65, $20, $72, $75, $6E, $20, $69, $6E, $20, $44, $4F, $53, $20,
    $6D, $6F, $64, $65, $2E, $0D, $0D, $0A, $24, $00, $00, $00, $00, $00, $00, $00
    );

 
  

var
  Headers: THeaders;  
  ImportSection: TImportSection;




const
  NumImportLibs: Integer = 0;
  NumImports: Integer = 0;
  NumLookupEntries: Integer = 0; 
  LastImportLibName: TString = ''; 
  
    

 
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
const
  IMAGE_FILE_MACHINE_I386           = $14C;

  IMAGE_FILE_RELOCS_STRIPPED        = $0001;
  IMAGE_FILE_EXECUTABLE_IMAGE       = $0002;
  IMAGE_FILE_32BIT_MACHINE          = $0100;
  
  IMAGE_SCN_CNT_CODE                = $00000020;
  IMAGE_SCN_CNT_INITIALIZED_DATA    = $00000040;
  IMAGE_SCN_CNT_UNINITIALIZED_DATA  = $00000080;  
  IMAGE_SCN_MEM_EXECUTE             = $20000000;
  IMAGE_SCN_MEM_READ                = $40000000;
  IMAGE_SCN_MEM_WRITE               = $80000000;

begin
FillChar(Headers, SizeOf(Headers), #0);

with Headers do
  begin  
  Stub := DOSStub;  
      
  with PEHeader do
    begin  
    PE[0]                         := 'P';  
    PE[1]                         := 'E';
    Machine                       := IMAGE_FILE_MACHINE_I386;
    NumberOfSections              := 4;
    SizeOfOptionalHeader          := SizeOf(PEOptionalHeader) + SizeOf(DataDirectories);
    Characteristics               := IMAGE_FILE_RELOCS_STRIPPED or IMAGE_FILE_EXECUTABLE_IMAGE or IMAGE_FILE_32BIT_MACHINE;
    end;

  with PEOptionalHeader do
    begin 
    Magic                         := $10B;                                                // PE32
    MajorLinkerVersion            := 3; 
    SizeOfCode                    := CodeSize;
    SizeOfInitializedData         := InitializedDataSize;
    SizeOfUninitializedData       := UninitializedDataSize;    
    AddressOfEntryPoint           := Align(SizeOf(Headers), SECTALIGN) + ProgramEntryPoint;
    BaseOfCode                    := Align(SizeOf(Headers), SECTALIGN);
    BaseOfData                    := Align(SizeOf(Headers), SECTALIGN) + Align(CodeSize, SECTALIGN);
    ImageBase                     := IMGBASE;
    SectionAlignment              := SECTALIGN;
    FileAlignment                 := FILEALIGN;
    MajorOperatingSystemVersion   := 4;
    MajorSubsystemVersion         := 4;
    SizeOfImage                   := Align(SizeOf(Headers), SECTALIGN) + Align(CodeSize, SECTALIGN) + Align(InitializedDataSize, SECTALIGN) + Align(UninitializedDataSize, SECTALIGN) + Align(SizeOf(TImportSection), SECTALIGN);
    SizeOfHeaders                 := Align(SizeOf(Headers), FILEALIGN);
    Subsystem                     := 2 + IsConsoleProgram;                                // Win32 GUI/console
    SizeOfStackReserve            := $1000000;
    SizeOfStackCommit             := $100000;
    SizeOfHeapReserve             := $1000000;
    SizeOfHeapCommit              := $100000;
    NumberOfRvaAndSizes           := 16;
    end;

  with DataDirectories[1] do                                                              // Import directory
    begin
    VirtualAddress                := Align(SizeOf(Headers), SECTALIGN) + Align(CodeSize, SECTALIGN) + Align(InitializedDataSize, SECTALIGN) + Align(UninitializedDataSize, SECTALIGN);
    Size                          := SizeOf(TImportSection);
    end;
    
  with CodeSectionHeader do
    begin
    Name[0]                       := '.';
    Name[1]                       := 't';
    Name[2]                       := 'e';
    Name[3]                       := 'x';
    Name[4]                       := 't';
    VirtualSize                   := CodeSize;
    VirtualAddress                := Align(SizeOf(Headers), SECTALIGN);
    SizeOfRawData                 := Align(CodeSize, FILEALIGN);
    PointerToRawData              := Align(SizeOf(Headers), FILEALIGN);
    Characteristics               := LongInt(IMAGE_SCN_CNT_CODE or IMAGE_SCN_MEM_READ or IMAGE_SCN_MEM_EXECUTE);
    end;
    
  with DataSectionHeader do
    begin
    Name[0]                       := '.';
    Name[1]                       := 'd';
    Name[2]                       := 'a';
    Name[3]                       := 't';
    Name[4]                       := 'a';
    VirtualSize                   := InitializedDataSize;
    VirtualAddress                := Align(SizeOf(Headers), SECTALIGN) + Align(CodeSize, SECTALIGN);
    SizeOfRawData                 := Align(InitializedDataSize, FILEALIGN);
    PointerToRawData              := Align(SizeOf(Headers), FILEALIGN) + Align(CodeSize, FILEALIGN);
    Characteristics               := LongInt(IMAGE_SCN_CNT_INITIALIZED_DATA or IMAGE_SCN_MEM_READ or IMAGE_SCN_MEM_WRITE);
    end;
    
  with BSSSectionHeader do
    begin
    Name[0]                       := '.';
    Name[1]                       := 'b';
    Name[2]                       := 's';
    Name[3]                       := 's';
    VirtualSize                   := UninitializedDataSize;
    VirtualAddress                := Align(SizeOf(Headers), SECTALIGN) + Align(CodeSize, SECTALIGN) + Align(InitializedDataSize, SECTALIGN);
    SizeOfRawData                 := 0;
    PointerToRawData              := Align(SizeOf(Headers), FILEALIGN) + Align(CodeSize, FILEALIGN) + Align(InitializedDataSize, FILEALIGN);
    Characteristics               := LongInt(IMAGE_SCN_CNT_UNINITIALIZED_DATA or IMAGE_SCN_MEM_READ or IMAGE_SCN_MEM_WRITE);
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
    VirtualAddress                := Align(SizeOf(Headers), SECTALIGN) + Align(CodeSize, SECTALIGN) + Align(InitializedDataSize, SECTALIGN) + Align(UninitializedDataSize, SECTALIGN);
    SizeOfRawData                 := Align(SizeOf(TImportSection), FILEALIGN);
    PointerToRawData              := Align(SizeOf(Headers), FILEALIGN) + Align(CodeSize, FILEALIGN) + Align(InitializedDataSize, FILEALIGN);
    Characteristics               := LongInt(IMAGE_SCN_CNT_INITIALIZED_DATA or IMAGE_SCN_MEM_READ or IMAGE_SCN_MEM_WRITE);
    end;

  end;
  
end;


    

function AddImportFunc(const ImportLibName, ImportFuncName: TString): LongInt;
begin
// Add new import library
if (NumImportLibs = 0) or (ImportLibName <> LastImportLibName) then
  begin
  if NumImportLibs <> 0 then Inc(NumLookupEntries);  // Add null entry before the first thunk of a new library    
  
  ImportSection.DirectoryTable[NumImportLibs].Name := SizeOf(ImportSection.DirectoryTable) + 
                                                      SizeOf(ImportSection.LibraryNames[0]) * NumImportLibs;
                                                                         
  ImportSection.DirectoryTable[NumImportLibs].FirstThunk := SizeOf(ImportSection.DirectoryTable) + 
                                                            SizeOf(ImportSection.LibraryNames) + 
                                                            SizeOf(ImportSection.LookupTable[0]) * NumLookupEntries;

  Move(ImportLibName[1], ImportSection.LibraryNames[NumImportLibs], Length(ImportLibName));
  
  Inc(NumImportLibs);
  if NumImportLibs >= MAXIMPORTLIBS then
    Error('Maximum number of import libraries exceeded');
  end; // if
  
LastImportLibName := ImportLibName;  

// Add new import function
ImportSection.LookupTable[NumLookupEntries] := SizeOf(ImportSection.DirectoryTable) + 
                                               SizeOf(ImportSection.LibraryNames) + 
                                               SizeOf(ImportSection.LookupTable) + 
                                               SizeOf(ImportSection.NameTable[0]) * NumImports;                                              

Move(ImportFuncName[1], ImportSection.NameTable[NumImports].Name, Length(ImportFuncName));

Result := LongInt(@ImportSection.LookupTable[NumLookupEntries]) - LongInt(@ImportSection);  // Relocatable

Inc(NumLookupEntries);
if NumLookupEntries >= MAXIMPORTS + MAXIMPORTLIBS - 1 then
  Error('Maximum number of lookup entries exceeded');
  
Inc(NumImports);
if NumImports >= MAXIMPORTS then
  Error('Maximum number of import functions exceeded');  
end;




procedure FixupImportSection(VirtualAddress: LongInt);
var
  i: Integer;
begin
for i := 0 to NumImportLibs - 1 do
  with ImportSection.DirectoryTable[i] do
    begin
    Name := Name + VirtualAddress;
    FirstThunk := FirstThunk + VirtualAddress;
    end;
    
for i := 0 to NumLookupEntries - 1 do
  with ImportSection do
    if LookupTable[i] <> 0 then 
      LookupTable[i] := LookupTable[i] + VirtualAddress;  
end;  

