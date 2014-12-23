with Gela.Lexical_Types;

--  limited with Gela.Int.Visiters;

package Gela.Int.Attr_Functions is
   pragma Preelaborate;

   type Attr_Function is new Interpretation with private;

   function Create
     (Down     : Gela.Interpretations.Interpretation_Index_Array;
      Kind     : Gela.Lexical_Types.Predefined_Symbols.Attribute)
      return Attr_Function;

   function Kind
     (Self : Attr_Function)
      return Gela.Lexical_Types.Predefined_Symbols.Attribute;

private

   type Attr_Function is new Interpretation with record
      Kind : Gela.Lexical_Types.Predefined_Symbols.Attribute;
   end record;

   overriding procedure Visit
     (Self    : Attr_Function;
      Visiter : access Gela.Int.Visiters.Visiter'Class);

end Gela.Int.Attr_Functions;
