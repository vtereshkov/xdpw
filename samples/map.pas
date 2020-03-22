// Heterogenous list operations and Map function demo

{$APPTYPE CONSOLE}

program Map;


uses 
  SysUtils;


// List definition

type
  IData = interface
    ToStr: function: string;
    Swap: procedure;
  end;  
  
  TProc = procedure (var D: IData);
  
  PNode = ^TNode;
  
  TNode = record
    Data: IData;
    Next: PNode;
  end;  
  
  TList = record
    Head, Tail: PNode;
  end;
  
  
// List methods    

procedure Create for L: TList;
begin
L.Head := nil;
L.Tail := nil;
end;


procedure Add for L: TList (var D: IData); 
var
  N: PNode;
begin
New(N);
N^.Data := D;
N^.Next := nil;
if L.Head = nil then L.Head := N else L.Tail^.Next := N;
L.Tail := N;  
end;


procedure Map for L: TList (Proc: TProc); 
var
  N: PNode;
begin
N := L.Head;
while N <> nil do
  begin
  Proc(N^.Data);
  N := N^.Next;
  end;
end;


procedure Destroy for L: TList; 
var
  N, Next: PNode;
begin
N := L.Head;
while N <> nil do
  begin
  Next := N^.Next; 
  Dispose(N^.Data.Self); 
  Dispose(N); 
  N := Next;
  end;
end;


// Point definition

type
  TPoint = record
    x, y: Integer;
  end;
  
  PPoint = ^TPoint;
  

// Point methods 
  
function ToStr for P: TPoint: string;
begin
Result := 'Point:  (' + IntToStr(P.x) + ', ' + IntToStr(P.y) + ')';
end;


procedure Swap for P: TPoint;
var
  Temp: Integer;
begin
Temp := P.x;
P.x := P.y;
P.y := Temp;
end;


// Person definition

type
  TPerson = record
    Name, Surname: string;
  end;
  
  PPerson = ^TPerson;
  
  
// Person methods      

function ToStr for P: TPerson: string;
begin
Result := 'Person: ' + P.Name + ' ' + P.Surname;
end;


procedure Swap for P: TPerson;
var
  Temp: string;
begin
Temp := P.Name;
P.Name := P.Surname;
P.Surname := Temp;
end;


// List processing

procedure Fill(var L: TList); 
var
  Person: PPerson;
  Point: PPoint;  
begin
New(Person);
with Person^ do begin Name := 'Washington'; Surname := 'Irving' end;
L.Add(Person^);

New(Point);
with Point^ do begin x := 5; y := 7 end;
L.Add(Point^);
end;


procedure Print(var D: IData);
begin
WriteLn(D.ToStr());
end;


procedure Swap(var D: IData);
begin
D.Swap();
end;


// Main program

var
  L: TList;      
  
begin
WriteLn;
WriteLn('Heterogenous list operations and Map function demo');
WriteLn;

L.Create();
Fill(L);

WriteLn('Before swapping:');
WriteLn;
L.Map(@Print);
WriteLn;

WriteLn('After swapping:');
WriteLn;
L.Map(@Swap);
L.Map(@Print);
WriteLn;

L.Destroy();
Writeln('Done.');

ReadLn;
end.

  
