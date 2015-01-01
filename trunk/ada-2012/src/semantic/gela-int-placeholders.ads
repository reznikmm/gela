package Gela.Int.Placeholders is
   pragma Preelaborate;

   type Placeholder is new Interpretation with private;

   function Create
     (Down : Gela.Interpretations.Interpretation_Index_Array;
      Kind : Gela.Interpretations.Placeholder_Kind)
      return Placeholder;

   function Placeholder_Kind
     (Self : Placeholder)
      return Gela.Interpretations.Placeholder_Kind;

private

   type Placeholder is new Interpretation with record
      Placeholder_Kind : Gela.Interpretations.Placeholder_Kind;
   end record;

   overriding procedure Visit
     (Self    : Placeholder;
      Visiter : access Gela.Int.Visiters.Visiter'Class);

end Gela.Int.Placeholders;
