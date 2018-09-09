with Gela.Int.Visiters;

package body Gela.Int.Attr_Functions is

   ------------
   -- Create --
   ------------

   function Create
     (Down     : Gela.Interpretations.Interpretation_Index_Array;
      Tipe     : Gela.Semantic_Types.Type_View_Index;
      Kind     : Gela.Lexical_Types.Predefined_Symbols.Attribute)
      return Attr_Function is
   begin
      return (Index  => 0,
              Length => Down'Length,
              Tipe   => Tipe,
              Kind   => Kind,
              Down   => Down);
   end Create;

   ----------
   -- Kind --
   ----------

   function Kind
     (Self : Attr_Function)
      return Gela.Lexical_Types.Predefined_Symbols.Attribute is
   begin
      return Self.Kind;
   end Kind;

   ----------
   -- Tipe --
   ----------

   not overriding function Tipe
     (Self : Attr_Function) return Gela.Semantic_Types.Type_View_Index is
   begin
      return Self.Tipe;
   end Tipe;

   -----------
   -- Visit --
   -----------

   overriding procedure Visit
     (Self    : Attr_Function;
      Visiter : access Gela.Int.Visiters.Visiter'Class) is
   begin
      Visiter.Attr_Function (Self);
   end Visit;

end Gela.Int.Attr_Functions;
