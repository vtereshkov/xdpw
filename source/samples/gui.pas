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
  Ellipse(hdc, Points[i].x - 2, Points[i].y - 2, Points[i].x + 2, Points[i].y + 2);
 
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
        Points[NumPoints].x := lParam and $FFFF;
        Points[NumPoints].y := (lParam shr 16) and $FFFF;
         
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
  hInstance, hWnd: LongInt;
  message: MSG;
  res: Integer;
  ClassName: string; 

 


begin
NumPoints := 0;

hInstance := GetModuleHandleA(nil);
ClassName := 'Main Window Class';

wc.style         := 0;
wc.lpfnWndProc   := @WindowProc;
wc.cbClsExtra    := 0;
wc.cbWndExtra    := 0;
wc.hInstance     := hInstance;
wc.hIcon         := 0;
wc.hCursor       := LoadCursorA(0, Pointer(IDC_ARROW));
wc.hbrBackground := 0;
wc.lpszMenuName  := nil;
wc.lpszClassName := @ClassName;  

res := RegisterClassA(wc);

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
                        hInstance,              // handle
                        nil);                   // additional application data                       

res := ShowWindow(hWnd, SW_SHOWDEFAULT);

while GetMessageA(message, 0, 0, 0) <> 0 do
  begin 
  res := TranslateMessage(message);
  res := DispatchMessageA(message);
  end;

end.
  
  
  
