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
hdc := BeginPaint(ps, hWnd);

FillRect(COLOR_WINDOW + 1, ps.rcPaint, hdc);
for i := 1 to NumPoints do
  Ellipse(Points[i].y - 2, Points[i].x - 2, Points[i].y + 2, Points[i].x + 2, hdc);
 
EndPaint(ps, hWnd);
end;


 


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
         
        InvalidateRect(0, nil, hWnd);
        end;
    Result := 1;
    end;    

  WM_DESTROY:
    begin
    PostQuitMessage(0);
    Result := 0;
    end
  else  
    Result := DefWindowProcA(lParam, wParam, uMsg, hWnd);
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
wc.hCursor       := LoadCursorA(Pointer(IDC_ARROW), 0);
wc.hbrBackground := 0;
wc.lpszMenuName  := nil;
wc.lpszClassName := @ClassName;  

res := RegisterClassA(wc);

hWnd := CreateWindowExA(nil,                   // additional application data
                       hInstance,              // handle
                       0,                      // menu
                       0,                      // parent window
                       480,                    // height
                       640,                    // width
                       100,                    // position Y
                       100,                    // position X
                       WS_OVERLAPPEDWINDOW,    // style
                       'GUI Demo',             // text
                       ClassName,              // class
                       0);                     // optional styles

res := ShowWindow(SW_SHOWDEFAULT, hWnd);

while GetMessageA(0, 0, 0, message) <> 0 do
  begin 
  res := TranslateMessage(message);
  res := DispatchMessageA(message);
  end;

end.
  
  
  
