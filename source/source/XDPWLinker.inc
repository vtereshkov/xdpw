// XD Pascal - a 32-bit compiler for Windows
// Developed by Vasiliy Tereshkov, 2009-2010, 2019

// Linker


 
const
  IMAGEBASE         = $400000;
  SECTIONALIGNMENT  = $1000;
  FILEALIGNMENT     = $200;
  
  MAXIMPORTS        = 64;
  


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
  NumImports: Integer;   
  
    

 
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

// MZ header
Headers.MZHeader.MZ[0]                                := 'M';  
Headers.MZHeader.MZ[1]                                := 'Z';
Headers.MZHeader.PEHeaderOffset                       := SizeOf(Headers.MZHeader);  
  
// PE header  
Headers.PEHeader.PE[0]                                := 'P';  
Headers.PEHeader.PE[1]                                := 'E';
Headers.PEHeader.Machine                              := $14C;                                                // Intel 386
Headers.PEHeader.NumberOfSections                     := 3;
Headers.PEHeader.SizeOfOptionalHeader                 := SizeOf(Headers.PEOptionalHeader) + SizeOf(Headers.DataDirectories);
Headers.PEHeader.Characteristics                      := $103;                                                // No relocations, executable, 32 bit

// PE optional header  
Headers.PEOptionalHeader.Magic                        := $10B;                                                // PE32
Headers.PEOptionalHeader.AddressOfEntryPoint          := Align(SizeOf(Headers), SECTIONALIGNMENT) + Align(SizeOf(TImportSection), SECTIONALIGNMENT) + Align(DataSize, SECTIONALIGNMENT) + ProgramEntryPoint;
Headers.PEOptionalHeader.ImageBase                    := IMAGEBASE;
Headers.PEOptionalHeader.SectionAlignment             := SECTIONALIGNMENT;
Headers.PEOptionalHeader.FileAlignment                := FILEALIGNMENT;
Headers.PEOptionalHeader.MajorOperatingSystemVersion  := 4;
Headers.PEOptionalHeader.MajorSubsystemVersion        := 4;
Headers.PEOptionalHeader.SizeOfImage                  := Align(SizeOf(Headers), SECTIONALIGNMENT) + Align(SizeOf(TImportSection), SECTIONALIGNMENT) + Align(DataSize, SECTIONALIGNMENT) + Align(CodeSize, SECTIONALIGNMENT);
Headers.PEOptionalHeader.SizeOfHeaders                := Align(SizeOf(Headers), FILEALIGNMENT);
Headers.PEOptionalHeader.Subsystem                    := 2 + IsConsoleProgram;                                // Win32 GUI/console
Headers.PEOptionalHeader.SizeOfStackReserve           := $1000000;
Headers.PEOptionalHeader.SizeOfStackCommit            := $1000000;
Headers.PEOptionalHeader.SizeOfHeapReserve            := $1000000;
Headers.PEOptionalHeader.SizeOfHeapCommit             := $1000000;
Headers.PEOptionalHeader.NumberOfRvaAndSizes          := 16;

// Data directories: import
Headers.DataDirectories[1].VirtualAddress             := Align(SizeOf(Headers), SECTIONALIGNMENT);
Headers.DataDirectories[1].Size                       := SizeOf(TImportSection);

// Import section header
Headers.ImportSectionHeader.Name[0]                   := '.';
Headers.ImportSectionHeader.Name[1]                   := 'i';
Headers.ImportSectionHeader.Name[2]                   := 'd';
Headers.ImportSectionHeader.Name[3]                   := 'a';
Headers.ImportSectionHeader.Name[4]                   := 't';
Headers.ImportSectionHeader.Name[5]                   := 'a';
Headers.ImportSectionHeader.VirtualSize               := SizeOf(TImportSection);
Headers.ImportSectionHeader.VirtualAddress            := Align(SizeOf(Headers), SECTIONALIGNMENT);
Headers.ImportSectionHeader.SizeOfRawData             := Align(SizeOf(TImportSection), FILEALIGNMENT);
Headers.ImportSectionHeader.PointerToRawData          := Align(SizeOf(Headers), FILEALIGNMENT);
Headers.ImportSectionHeader.Characteristics           := LongInt($C0000040);                              // Data, readable, writable

// Data section header
Headers.DataSectionHeader.Name[0]                     := '.';
Headers.DataSectionHeader.Name[1]                     := 'd';
Headers.DataSectionHeader.Name[2]                     := 'a';
Headers.DataSectionHeader.Name[3]                     := 't';
Headers.DataSectionHeader.Name[4]                     := 'a';
Headers.DataSectionHeader.VirtualSize                 := DataSize;
Headers.DataSectionHeader.VirtualAddress              := Align(SizeOf(Headers), SECTIONALIGNMENT) + Align(SizeOf(TImportSection), SECTIONALIGNMENT);
Headers.DataSectionHeader.SizeOfRawData               := Align(InitializedDataSize, FILEALIGNMENT);
Headers.DataSectionHeader.PointerToRawData            := Align(SizeOf(Headers), FILEALIGNMENT) + Align(SizeOf(TImportSection), FILEALIGNMENT);
Headers.DataSectionHeader.Characteristics             := LongInt($C00000C0);                              // Data, readable, writable

// Code section header
Headers.CodeSectionHeader.Name[0]                     := '.';
Headers.CodeSectionHeader.Name[1]                     := 't';
Headers.CodeSectionHeader.Name[2]                     := 'e';
Headers.CodeSectionHeader.Name[3]                     := 'x';
Headers.CodeSectionHeader.Name[4]                     := 't';
Headers.CodeSectionHeader.VirtualSize                 := CodeSize;
Headers.CodeSectionHeader.VirtualAddress              := Align(SizeOf(Headers), SECTIONALIGNMENT) + Align(SizeOf(TImportSection), SECTIONALIGNMENT) + Align(DataSize, SECTIONALIGNMENT);
Headers.CodeSectionHeader.SizeOfRawData               := Align(CodeSize, FILEALIGNMENT);
Headers.CodeSectionHeader.PointerToRawData            := Align(SizeOf(Headers), FILEALIGNMENT) + Align(SizeOf(TImportSection), FILEALIGNMENT) + Align(InitializedDataSize, FILEALIGNMENT);
Headers.CodeSectionHeader.Characteristics             := LongInt($60000020);                              // Code, executable, readable
end;





function AddImportFunc(const ImportLibName, ImportFuncName: TString): LongInt;
var
  VirtualAddress: LongInt;
begin
VirtualAddress := Align(SizeOf(Headers), SECTIONALIGNMENT);

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

Result := IMAGEBASE + VirtualAddress + LongInt(@ImportSection.LookupTable[NumImports, 0]) - LongInt(@ImportSection);
Inc(NumImports);
end;
  
    

