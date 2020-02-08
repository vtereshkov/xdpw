// Linked list operations demo

{$APPTYPE CONSOLE}

program List;



type
  PPerson = ^TPerson;

  TPerson = record
    Next: PPerson;
    Name, Surname: string [40];
    Born: SmallInt;
  end;


var
  Head, Node, NewNode: PPerson;
  ch: Char;



begin
WriteLn;
WriteLn('Linked list operations demo');
WriteLn;

New(Node);
Node^.Next := nil;
Head := Node;


// Fill the list
repeat
  Write('Add new record? (Y/N): '); ReadLn(ch);
  WriteLn;

  if (ch = 'y') or (ch = 'Y') then
    begin
    New(NewNode);
    Node^.Next := NewNode;
    Node := NewNode;
    Node^.Next := nil;
    Write('Name    : '); ReadLn(Node^.Name);
    Write('Surname : '); ReadLn(Node^.Surname);
    Write('Born in : '); ReadLn(Node^.Born);
    WriteLn; 
    end;
until (ch = 'n') or (ch = 'N');


WriteLn;
WriteLn('Record list: ');
WriteLn;


// Traverse the list
Node := Head^.Next;

while Node <> nil do
  begin
  WriteLn(Node^.Name, ' ', Node^.Surname, ', b. ', Node^.Born);
  Node := Node^.Next;
  end;


// Clear the list
Node := Head;

while Node <> nil do
  begin
  NewNode := Node^.Next;
  Dispose(Node);
  Node := NewNode;
  end;

WriteLn;
WriteLn('Done.');

ReadLn;
end.




