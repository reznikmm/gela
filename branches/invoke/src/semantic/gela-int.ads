limited with Gela.Int.Visiters;
with Gela.Saves;

package Gela.Int is
   pragma Preelaborate;

   type Interpretation is abstract tagged null record;
   type Interpretation_Access is access Interpretation'Class;

   not overriding procedure Visit
     (Self    : Interpretation;
      Visiter : access Gela.Int.Visiters.Visiter'Class) is abstract;

   not overriding function Save
     (Self : Interpretation) return Gela.Saves.Save_Access is abstract;

end Gela.Int;
