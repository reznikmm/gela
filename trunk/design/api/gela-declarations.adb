with Gela.Element_Sequences;
with Gela.Elements.Defining_Names;

package body Gela.Declarations is

   ----------
   -- Name --
   ----------

   function Name
     (Self : aliased Declaration'Class)
      return Gela.Elements.Defining_Names.Defining_Name_Access
   is
      Seq : constant Gela.Element_Sequences.Defining_Name_Sequence_Access :=
        Self.Names;
   begin
      for J in Seq.Iterate loop
         return J;
      end loop;

      return null;
   end Name;

end Gela.Declarations;
