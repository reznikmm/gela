with Asis.Elements;

with Gela.A4G.Contexts;

package body Gela.A4G.Elements is

   ------------
   -- Create --
   ------------

   function Create
     (Context : not null access Gela.A4G.Contexts.Context'Class;
      Node    : Asis.Element) return Element_Access
   is
      Result : constant Element_Access := new Element'
        (Context => Context,
         Node    => Node,
         Next    => null);

   begin
      return Result;
   end Create;

   -----------------------
   -- Enclosing_Element --
   -----------------------

   overriding function Enclosing_Element
     (Self : aliased Element)
      return Gela.Elements.Element_Access
   is
      Parent : constant Asis.Element :=
        Asis.Elements.Enclosing_Element (Self.Node);
      Result : constant Element_Access := Self.Context.Create_Element (Parent);
   begin
      return Gela.Elements.Element_Access (Result);
   end Enclosing_Element;

   ----------------------
   -- Compilation_Unit --
   ----------------------

   overriding function Compilation_Unit
     (Self : aliased Element)
      return Gela.Compilation_Units.Compilation_Unit_Access
   is
      Unit : constant Asis.Compilation_Unit :=
        Asis.Elements.Enclosing_Compilation_Unit (Self.Node);
   begin
      return Gela.Compilation_Units.Compilation_Unit_Access
        (Self.Context.Create_Compilation_Unit (Unit));
   end Compilation_Unit;

   -----------
   -- Names --
   -----------

   overriding function Names
     (Self : aliased Element)
      return Gela.Element_Sequences.Defining_Name_Sequence_Access
   is
      List : Asis.Element_List :=
        Asis.Declarations.Names (Self.Node);
   begin
   end Names;

   -----------
   -- Visit --
   -----------

   overriding procedure Visit
     (Self    : aliased Element;
      Visiter : in out Gela.Element_Visiters.Visiter'Class)
   is
   begin
      null;
   end Visit;

end Gela.A4G.Elements;
