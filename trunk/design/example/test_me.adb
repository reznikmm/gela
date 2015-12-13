with Ada.Wide_Wide_Text_IO;

with Asis.Implementation;

with Gela.A4G.Contexts;
with Gela.Compilation_Unit_Sets;
with Gela.Declarations.Package_Declarations;
with Gela.Element_Sequences;
with Gela.Element_Visiters;

procedure Test_Me is
   Context : aliased Gela.A4G.Contexts.Context;

   type Declaration_Visiter is new Gela.Element_Visiters.Visiter
     with null record;

   overriding procedure Package_Declaration
     (Self    : access Declaration_Visiter;
      Element : aliased Gela.Declarations.Package_Declarations
                       .Package_Declaration'Class);

   overriding procedure Package_Declaration
     (Self    : access Declaration_Visiter;
      Element : aliased Gela.Declarations.Package_Declarations
                       .Package_Declaration'Class)
   is
      Seq : constant Gela.Element_Sequences.Element_Sequence_Access :=
        Element.Visible_Part;
   begin
      for J in Seq.Iterate loop
         J.Visit (Self.all);
      end loop;
   end Package_Declaration;

   V : Declaration_Visiter;

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
         J.Unit_Declaration.Visit (V);
      end loop;

      Ada.Wide_Wide_Text_IO.Put_Line
        ("Got by name: " & Set.all ("system").Name.Image.To_Wide_Wide_String);

   end;
end Test_Me;
