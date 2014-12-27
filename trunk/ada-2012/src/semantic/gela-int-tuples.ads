limited with Gela.Int.Visiters;

package Gela.Int.Tuples is
   pragma Preelaborate;

   type Tuple (<>) is new Interpretation with private;

   function Create
     (Value : Gela.Interpretations.Interpretation_Set_Index_Array)
      return Tuple;

   function Value
     (Self : Tuple) return Gela.Interpretations.Interpretation_Set_Index_Array;

   type Chosen_Tuple is new Interpretation with null record;

private

   type Tuple (Length : Natural; Size : Positive) is
     new Interpretation (Length) with
   record
      Value : Gela.Interpretations.Interpretation_Set_Index_Array (1 .. Size);
   end record;

   overriding procedure Visit
     (Self    : Tuple;
      Visiter : access Gela.Int.Visiters.Visiter'Class);

   overriding procedure Visit
     (Self    : Chosen_Tuple;
      Visiter : access Gela.Int.Visiters.Visiter'Class);

end Gela.Int.Tuples;
