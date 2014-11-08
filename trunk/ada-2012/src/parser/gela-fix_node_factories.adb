with Gela.Nodes.Fixed_Operator_Symbols;

package body Gela.Fix_Node_Factories is

   ---------------------
   -- Operator_Symbol --
   ---------------------

   overriding function Operator_Symbol
     (Self : in out Element_Factory;
      Operator_Symbol_Token : Gela.Lexical_Types.Token_Count)
      return Gela.Elements.Operator_Symbols.Operator_Symbol_Access
   is
      Result : constant Gela.Nodes.Fixed_Operator_Symbols
        .Operator_Symbol_Access :=
           new Gela.Nodes.Fixed_Operator_Symbols.Operator_Symbol'
            (Gela.Nodes.Fixed_Operator_Symbols.Create
              (Self.Comp,
               Operator_Symbol_Token));
   begin
      return Gela.Elements.Operator_Symbols.Operator_Symbol_Access (Result);
   end Operator_Symbol;

end Gela.Fix_Node_Factories;
