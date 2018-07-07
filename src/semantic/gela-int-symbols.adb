with Gela.Int.Visiters;

package body Gela.Int.Symbols is

   ------------
   -- Create --
   ------------

   function Create
     (Down  : Gela.Interpretations.Interpretation_Index_Array;
      Value : Gela.Lexical_Types.Symbol) return Symbol is
   begin
      return (Index  => 0,
              Length => Down'Length,
              Symbol => Value,
              Down   => Down);
   end Create;

   ----------------
   -- Get_Symbol --
   ----------------

   function Get_Symbol (Self : Symbol) return Gela.Lexical_Types.Symbol is
   begin
      return Self.Symbol;
   end Get_Symbol;

   -----------
   -- Visit --
   -----------

   overriding procedure Visit
     (Self    : Symbol;
      Visiter : access Gela.Int.Visiters.Visiter'Class) is
   begin
      Visiter.Symbol (Self);
   end Visit;

end Gela.Int.Symbols;
