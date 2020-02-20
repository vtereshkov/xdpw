// Windows API GUI definitions 

unit Windows;


interface


const
  WS_OVERLAPPEDWINDOW	= $CF0000;
  
  SW_SHOWDEFAULT      = 10;
  
  MB_OK               = 0;
  MB_OKCANCEL         = 1;  

  WM_PAINT            = $000F;
  WM_MOUSEMOVE        = $0200;
  WM_DESTROY          = $0002;
  
  MK_LBUTTON          = $0001;
  MK_RBUTTON          = $0002;
  
  COLOR_WINDOW        = 5;
  
  IDC_ARROW           = 32512;



type
  WNDCLASSA = record
    style: LongInt;
    lpfnWndProc: Pointer;
    cbClsExtra: Integer;
    cbWndExtra: Integer;
    hInstance: LongInt;
    hIcon: LongInt;
    hCursor: LongInt;
    hbrBackground: LongInt;
    lpszMenuName: PChar;
    lpszClassName: PChar;
  end;   
  
  
  POINT = record 
    x: LongInt; 
    y: LongInt; 
  end;


  RECT = record
    left: LongInt;
    top: LongInt;
    right: LongInt;
    bottom: LongInt;
  end; 
  
  
  MSG = record
    hwnd: LongInt;
    message: LongInt;
    wParam: LongInt;
    lParam: LongInt;
    time: LongInt;
    pt: POINT;
    lPrivate: LongInt;
  end;
  
  
  PAINTSTRUCT = record
    hdc: LongInt;
    fErase: Integer;
    rcPaint: RECT;
    fRestore: Integer;
    fIncUpdate: Integer;
    rgbReserved: array [0..31] of Byte;
  end;
  

  
function GetModuleHandleA(lpModuleName: Pointer): LongInt stdcall; external 'KERNEL32.DLL';  

function MessageBoxA(hWnd: LongInt; lpText, lpCaption: PChar;
                     uType: LongInt): Integer stdcall; external 'USER32.DLL';

function LoadCursorA(hInstance: LongInt; lpCursorName: Pointer): LongInt stdcall; external 'USER32.DLL';

function RegisterClassA(var lpWndClass: WNDCLASSA): Integer stdcall; external 'USER32.DLL';

function CreateWindowExA(dwExStyle: LongInt;
                         lpClassName: PChar;
                         lpWindowName: PChar;
                         dwStyle: LongInt;
                         X: Integer;
                         Y: Integer;
                         nWidth: Integer;
                         nHeight: Integer;
                         hWndParent: LongInt;
                         hMenu: LongInt;
                         hInstance: LongInt;
                         lpParam: Pointer): LongInt stdcall; external 'USER32.DLL';
                         
function ShowWindow(hWnd: LongInt; nCmdShow: Integer): Integer stdcall; external 'USER32.DLL';

function GetMessageA(var lpMsg: MSG; hWnd: LongInt; 
                     wMsgFilterMin, wMsgFilterMax: Integer): Integer stdcall; external 'USER32.DLL'; 

function TranslateMessage(var lpMsg: MSG): Integer stdcall; external 'USER32.DLL';

function DispatchMessageA(var lpMsg: MSG): Integer stdcall; external 'USER32.DLL';
                         
function DefWindowProcA(hWnd, uMsg, wParam, lParam: LongInt): Integer stdcall; external 'USER32.DLL';

function BeginPaint(hWnd: LongInt; var lpPaint: PAINTSTRUCT): LongInt stdcall; external 'USER32.DLL';

procedure EndPaint(hWnd: LongInt; var lpPaint: PAINTSTRUCT) stdcall; external 'USER32.DLL';

procedure FillRect(hDC: LongInt; var lprc: RECT; hbr: LongInt) stdcall; external 'USER32.DLL';

procedure InvalidateRect(hWnd: LongInt; lpRect: Pointer; bErase: Integer) stdcall; external 'USER32.DLL';

procedure Ellipse(hDC: LongInt; left, top, right, bottom: Integer) stdcall; external 'GDI32.DLL';

procedure PostQuitMessage(nExitCode: Integer) stdcall; external 'USER32.DLL';

procedure StrToChars(const S: string; var Chars: array of Char);




implementation


procedure StrToChars(const S: string; var Chars: array of Char);
var
  Len: Integer;
begin
Len := Length(S);
Move(S[1], Chars, Len);
Chars[Len] := #0;
end;


end.

