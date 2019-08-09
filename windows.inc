// Windows API library



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
    lpszMenuName: ^string;
    lpszClassName: ^string;
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
  

  
function GetModuleHandleA(lpModuleName: Pointer): LongInt; external 'KERNEL32.DLL' name 'GetModuleHandleA';  

function MessageBoxA(uType: LongInt; const lpCaption, lpText: string;
                     hWnd: LongInt): Integer; external 'USER32.DLL' name 'MessageBoxA';

function LoadCursorA(lpCursorName: Pointer; hInstance: LongInt): LongInt; external 'USER32.DLL' name 'LoadCursorA';

function RegisterClassA(var lpWndClass: WNDCLASSA): Integer; external 'USER32.DLL' name 'RegisterClassA';

function CreateWindowExA(lpParam: Pointer; 
                         hInstance: LongInt;
                         hMenu: LongInt;
                         hWndParent: LongInt;
                         nHeight: Integer;
                         nWidth: Integer;
                         Y: Integer; 
                         X: Integer;
                         dwStyle: LongInt;
                         const lpWindowName: string;
                         const lpClassName: string;
                         dwExStyle: LongInt): LongInt; external 'USER32.DLL' name 'CreateWindowExA';
                         
function ShowWindow(nCmdShow: Integer; hWnd: LongInt): Integer; external 'USER32.DLL' name 'ShowWindow';

function GetMessageA(wMsgFilterMax, wMsgFilterMin: Integer; 
                    hWnd: LongInt; var lpMsg: MSG): Integer; external 'USER32.DLL' name 'GetMessageA'; 

function TranslateMessage(var lpMsg: MSG): Integer; external 'USER32.DLL' name 'TranslateMessage';

function DispatchMessageA(var lpMsg: MSG): Integer; external 'USER32.DLL' name 'DispatchMessageA';
                         
function DefWindowProcA(lParam, wParam, uMsg, hWnd: LongInt): Integer; external 'USER32.DLL' name 'DefWindowProcA';

function BeginPaint(var lpPaint: PAINTSTRUCT; hWnd: LongInt): LongInt; external 'USER32.DLL' name 'BeginPaint';

procedure EndPaint(var lpPaint: PAINTSTRUCT; hWnd: LongInt); external 'USER32.DLL' name 'EndPaint';

procedure FillRect(hbr: LongInt; var lprc: RECT; hDC: LongInt); external 'USER32.DLL' name 'FillRect';

procedure InvalidateRect(bErase: Integer; lpRect: Pointer; hWnd: LongInt); external 'USER32.DLL' name 'InvalidateRect';

procedure Ellipse(bottom, right, top, left: Integer; hDC: LongInt); external 'GDI32.DLL' name 'Ellipse';

procedure PostQuitMessage(nExitCode: Integer); external 'USER32.DLL' name 'PostQuitMessage';
