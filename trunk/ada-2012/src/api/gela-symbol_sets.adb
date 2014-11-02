package body Gela.Symbol_Sets is

   ------------
   -- Parent --
   ------------

   function Parent
     (Self  : Symbol_Set'Class;
      Value : Gela.Lexical_Types.Symbol)
      return Gela.Lexical_Types.Symbol
   is
      use type Gela.Lexical_Types.Symbol;

      Prefix : constant Gela.Lexical_Types.Symbol := Self.Prefix (Value);
   begin
      if Value = Gela.Lexical_Types.Predefined_Symbols.Standard then
         return Gela.Lexical_Types.No_Symbol;
      elsif Prefix = Gela.Lexical_Types.No_Symbol then
         return Gela.Lexical_Types.Predefined_Symbols.Standard;
      else
         return Prefix;
      end if;
   end Parent;

end Gela.Symbol_Sets;
