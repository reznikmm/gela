with Gela.Solutions.Visiters;

package body Gela.Solutions.Defining_Names is

   ------------
   -- Create --
   ------------

   not overriding function Create
     (Name : Gela.Elements.Defining_Names.Defining_Name_Access;
      Save : Gela.Saves.Save_Access)
      return Defining_Name_Solution is
   begin
      return (Save, Name);
   end Create;

   ----------
   -- Name --
   ----------

   not overriding function Name
     (Self : Defining_Name_Solution)
      return Gela.Elements.Defining_Names.Defining_Name_Access is
   begin
      return Self.Name;
   end Name;

   -----------
   -- Visit --
   -----------

   overriding procedure Visit
     (Self    : Defining_Name_Solution;
      Visiter : access Gela.Solutions.Visiters.Visiter'Class) is
   begin
      Visiter.Defining_Name (Self);
   end Visit;

end Gela.Solutions.Defining_Names;
