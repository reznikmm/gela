with Gela.Lexical_Types;
with Gela.Semantic_Types;

package Gela.Int.Attr_Functions is
   pragma Preelaborate;

   type Attr_Function is new Interpretation with private;

   function Create
     (Down     : Gela.Interpretations.Interpretation_Index_Array;
      Tipe     : Gela.Semantic_Types.Type_Index;
      Kind     : Gela.Lexical_Types.Predefined_Symbols.Attribute)
      return Attr_Function;

   not overriding function Kind
     (Self : Attr_Function)
      return Gela.Lexical_Types.Predefined_Symbols.Attribute;

   not overriding function Tipe
     (Self : Attr_Function) return Gela.Semantic_Types.Type_Index;

private

   type Attr_Function is new Interpretation with record
      Tipe : Gela.Semantic_Types.Type_Index;
      Kind : Gela.Lexical_Types.Predefined_Symbols.Attribute;
   end record;

   overriding procedure Visit
     (Self    : Attr_Function;
      Visiter : access Gela.Int.Visiters.Visiter'Class);

end Gela.Int.Attr_Functions;
