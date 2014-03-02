limited with Gela.Int.Visiters;

package Gela.Int is
   pragma Preelaborate;

   type Interpretation is abstract tagged null record;
   type Interpretation_Access is access Interpretation'Class;

   not overriding procedure Visit
     (Self    : Interpretation;
      Visiter : access Gela.Int.Visiters.Visiter'Class) is abstract;

end Gela.Int;
