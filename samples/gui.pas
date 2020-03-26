// GUI demo

{$APPTYPE GUI}
{$H-}

program GUI;


uses Windows;


const
  MAX_POINTS = 10000;



var
  Points: array [1..MAX_POINTS] of POINT;
  NumPoints: Integer = 0;
  



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


 

function WindowProc(hWnd, uMsg, wParam, lParam: LongInt): Integer stdcall; 
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




const
  ClassName = 'GUI Demo Window Class';
  WindowText = 'GUI Demo';
  
 
var
  wc: WNDCLASSA;
  hInst, hWnd: LongInt;
  message: MSG; 


begin
hInst := GetModuleHandleA(nil);

with wc do
  begin
  style         := 0;
  lpfnWndProc   := Pointer(@WindowProc);
  cbClsExtra    := 0;
  cbWndExtra    := 0;
  hInstance     := hInst;
  hIcon         := 0;
  hCursor       := LoadCursorA(0, Pointer(IDC_ARROW));
  hbrBackground := 0;
  lpszMenuName  := nil;
  lpszClassName := ClassName;
  end;  

RegisterClassA(wc);

hWnd := CreateWindowExA(0,                      // optional styles
                        ClassName,              // class
                        WindowText,             // text
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
  
  
  
