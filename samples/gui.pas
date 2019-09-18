// GUI demo

{$A GUI}

program GUI;



{$I windows.inc}



const
  MAX_POINTS = 10000;



var
  Points: array [1..MAX_POINTS] of POINT;
  NumPoints: Integer;
  



procedure Repaint(hWnd: LongInt);
var
  ps: PAINTSTRUCT;
  hdc: LongInt;
  i: Integer;
  
begin
hdc := BeginPaint(hWnd, ps);

FillRect(hdc, ps.rcPaint, COLOR_WINDOW + 1);
for i := 1 to NumPoints do
  with Points[i] do
    Ellipse(hdc, x - 2, y - 2, x + 2, y + 2);
 
EndPaint(hWnd, ps);
end;


 

// Callback function: parameters reversed for compatibiity with 'stdcall'
function WindowProc(lParam, wParam, uMsg, hWnd: LongInt): Integer;  
begin
case uMsg of
  WM_PAINT:
    begin
    Repaint(hWnd);
    Result := 1;
    end;
    
  WM_MOUSEMOVE:
    begin
      if (wParam and MK_LBUTTON <> 0) and (NumPoints < MAX_POINTS) then
        begin
        Inc(NumPoints);
        
        with Points[NumPoints] do
          begin
          x := lParam and $FFFF;
          y := (lParam shr 16) and $FFFF;
          end;
         
        InvalidateRect(hWnd, nil, 0);
        end;
    Result := 1;
    end;    

  WM_DESTROY:
    begin
    PostQuitMessage(0);
    Result := 0;
    end
  else  
    Result := DefWindowProcA(hWnd, uMsg, wParam, lParam);
  end;
end;  



 
var
  wc: WNDCLASSA;
  hInst, hWnd: LongInt;
  message: MSG;
  ClassName: string; 

 


begin
NumPoints := 0;

hInst := GetModuleHandleA(nil);
ClassName := 'Main Window Class';

with wc do
  begin
  style         := 0;
  lpfnWndProc   := @WindowProc;
  cbClsExtra    := 0;
  cbWndExtra    := 0;
  hInstance     := hInst;
  hIcon         := 0;
  hCursor       := LoadCursorA(0, Pointer(IDC_ARROW));
  hbrBackground := 0;
  lpszMenuName  := nil;
  lpszClassName := @ClassName;
  end;  

RegisterClassA(wc);

hWnd := CreateWindowExA(0,                      // optional styles
                        ClassName,              // class
                        'GUI Demo',             // text
                        WS_OVERLAPPEDWINDOW,    // style
                        100,                    // position X
                        100,                    // position Y
                        640,                    // width
                        480,                    // height
                        0,                      // parent window
                        0,                      // menu
                        hInst,                  // handle
                        nil);                   // additional application data                       

ShowWindow(hWnd, SW_SHOWDEFAULT);

while GetMessageA(message, 0, 0, 0) <> 0 do
  begin 
  TranslateMessage(message);
  DispatchMessageA(message);
  end;

end.
  
  
  
