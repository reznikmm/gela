with Gela.Elements.Defining_Names;

limited with Gela.Int.Visiters;

package Gela.Int.Defining_Names is
   pragma Preelaborate;

   type Defining_Name is new Interpretation with private;

   function Create
     (Name : Gela.Elements.Defining_Names.Defining_Name_Access;
      Save : Gela.Saves.Save_Access)
      return Defining_Name;

   function Name
     (Self : Defining_Name)
      return Gela.Elements.Defining_Names.Defining_Name_Access;

private

   type Defining_Name is new Interpretation with record
      Name : Gela.Elements.Defining_Names.Defining_Name_Access;
      Save : Gela.Saves.Save_Access;
   end record;

   overriding procedure Visit
     (Self    : Defining_Name;
      Visiter : access Gela.Int.Visiters.Visiter'Class);

   overriding function Save
     (Self : Defining_Name) return Gela.Saves.Save_Access;

end Gela.Int.Defining_Names;