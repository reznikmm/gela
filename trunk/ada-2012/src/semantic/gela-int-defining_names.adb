with Gela.Int.Visiters;

package body Gela.Int.Defining_Names is

   ------------
   -- Create --
   ------------

   function Create
     (Down     : Gela.Interpretations.Interpretation_Index_Array;
      Name     : Gela.Elements.Defining_Names.Defining_Name_Access)
      return Defining_Name is
   begin
      return (Length => Down'Length, Name => Name, Down => Down);
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
