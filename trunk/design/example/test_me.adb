with Ada.Wide_Wide_Text_IO;

with Asis.Implementation;

with Gela.A4G.Contexts;
with Gela.Compilation_Unit_Sets;

procedure Test_Me is
   Context : aliased Gela.A4G.Contexts.Context;
begin
   Asis.Implementation.Initialize;
   Context.Initialize ("");

   declare
      Set : constant Gela.Compilation_Unit_Sets.Compilation_Unit_Set_Access :=
        Context.Library_Unit_Declarations;
   begin
      for J of Set.all loop
         Ada.Wide_Wide_Text_IO.Put_Line
           (J.Name.Image.To_Wide_Wide_String);
      end loop;
   end;
end Test_Me;
