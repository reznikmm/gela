with Gela.Solutions.Defining_Names;

package Gela.Solutions.Visiters is
   pragma Preelaborate;

   type Visiter is limited interface;

   not overriding procedure Defining_Name
     (Self : access Visiter;
      Value : Gela.Solutions.Defining_Names.Defining_Name_Solution) is null;

end Gela.Solutions.Visiters;
