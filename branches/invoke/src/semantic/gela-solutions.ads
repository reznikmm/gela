limited with Gela.Solutions.Visiters;

package Gela.Solutions is
   pragma Preelaborate;

   type Solution is abstract tagged null record;
   type Solution_Access is access Solution'Class;

   not overriding procedure Visit
     (Self    : Solution;
      Visiter : access Gela.Solutions.Visiters.Visiter'Class) is abstract;

end Gela.Solutions;
