with Gela.Int.Defining_Names;

package Gela.Int.Visiters is
   pragma Preelaborate;

   type Visiter is limited interface;

   not overriding procedure Defining_Name
     (Self  : access Visiter;
      Value : Gela.Int.Defining_Names.Defining_Name) is null;

   type Solution_Visiter is limited interface;

end Gela.Int.Visiters;