with Gela.Int.Visiters;

package body Gela.Int.Defining_Names is

   ------------
   -- Create --
   ------------

   function Create
     (Name : Gela.Elements.Defining_Names.Defining_Name_Access;
      Save : Gela.Saves.Save_Access)
      return Defining_Name is
   begin
      return (Name, Save);
   end Create;

   ----------
   -- Name --
   ----------

   function Name
     (Self : Defining_Name)
      return Gela.Elements.Defining_Names.Defining_Name_Access is
   begin
      return Self.Name;
   end Name;

   ----------
   -- Save --
   ----------

   overriding function Save
     (Self : Defining_Name) return Gela.Saves.Save_Access is
   begin
      return Self.Save;
   end Save;

   -----------
   -- Visit --
   -----------

   overriding procedure Visit
     (Self    : Defining_Name;
      Visiter : access Gela.Int.Visiters.Visiter'Class) is
   begin
      Visiter.Defining_Name (Self);
   end Visit;

end Gela.Int.Defining_Names;
