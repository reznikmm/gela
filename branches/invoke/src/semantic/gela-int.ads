limited with Gela.Int.Visiters;
with Gela.Interpretations;

package Gela.Int is
   pragma Preelaborate;

   type Interpretation (Length : Natural) is abstract tagged record
      Down : Gela.Interpretations.Interpretation_Index_Array (1 .. Length);
   end record;

   type Interpretation_Access is access Interpretation'Class;

   not overriding procedure Visit
     (Self    : Interpretation;
      Visiter : access Gela.Int.Visiters.Visiter'Class) is abstract;

end Gela.Int;
