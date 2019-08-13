// XD Pascal - a 32-bit compiler for Windows
// Developed by Vasiliy Tereshkov, 2009-2010, 2019

// Optimizer



procedure AddCallGraphChild(ParentBlock, ChildBlock: Integer);
begin
// Set bit at ParentBlock row, ChildBlock column
CallGraph[ParentBlock - 1, (ChildBlock - 1) div 8] := CallGraph[ParentBlock - 1, (ChildBlock - 1) div 8] or 
                                                     (1 shl ((ChildBlock - 1) mod 8));
end;





procedure MarkBlockNotDead(ParentBlock: Integer);
var
  ChildBlock: Integer;
begin
BlockIsNotDead[ParentBlock] := TRUE;
for ChildBlock := 1 to NumBlocks do
  if ((CallGraph[ParentBlock - 1, (ChildBlock - 1) div 8] and (1 shl ((ChildBlock - 1) mod 8))) <> 0) and 
     not BlockIsNotDead[ChildBlock] then
    MarkBlockNotDead(ChildBlock);
end;


