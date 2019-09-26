
with Program.Parsers.On_Reduce_1;
with Program.Parsers.On_Reduce_501;
with Program.Parsers.On_Reduce_1001;
with Program.Parsers.On_Reduce_1501;
with Program.Parsers.On_Reduce_2001;
procedure Program.Parsers.On_Reduce
  (Self  : access Parse_Context;
   Prod  : Anagram.Grammars.Production_Index;
   Nodes : in out Program.Parsers.Nodes.Node_Array) is
begin
   case Prod is
      when 1 .. 500 =>
         On_Reduce_1 (Self, Prod, Nodes);
      when 501 .. 1000 =>
         On_Reduce_501 (Self, Prod, Nodes);
      when 1001 .. 1500 =>
         On_Reduce_1001 (Self, Prod, Nodes);
      when 1501 .. 2000 =>
         On_Reduce_1501 (Self, Prod, Nodes);
      when 2001 .. 2239 =>
         On_Reduce_2001 (Self, Prod, Nodes);
      when others =>
         raise Constraint_Error;
   end case;
end Program.Parsers.On_Reduce;
