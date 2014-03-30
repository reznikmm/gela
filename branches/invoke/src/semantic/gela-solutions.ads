limited with Gela.Solutions.Visiters;
with Gela.Saves;

package Gela.Solutions is
   pragma Preelaborate;

   type Solution (Save : Gela.Saves.Save_Access) is
     abstract tagged null record;
   type Solution_Access is access Solution'Class;

   not overriding procedure Visit
     (Self    : Solution;
      Visiter : access Gela.Solutions.Visiters.Visiter'Class) is abstract;

end Gela.Solutions;
