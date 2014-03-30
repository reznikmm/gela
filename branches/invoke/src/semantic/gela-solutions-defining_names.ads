with Gela.Elements.Defining_Names;

package Gela.Solutions.Defining_Names is
   pragma Preelaborate;

   type Defining_Name_Solution is new Solution with private;

   not overriding function Name
     (Self : Defining_Name_Solution)
      return Gela.Elements.Defining_Names.Defining_Name_Access;

   not overriding function Create
     (Name : Gela.Elements.Defining_Names.Defining_Name_Access;
      Save : Gela.Saves.Save_Access)
      return Defining_Name_Solution;

private

   type Defining_Name_Solution is new Solution with record
      Name : Gela.Elements.Defining_Names.Defining_Name_Access;
   end record;

   overriding procedure Visit
     (Self    : Defining_Name_Solution;
      Visiter : access Gela.Solutions.Visiters.Visiter'Class);

end Gela.Solutions.Defining_Names;
