with Asis;

with Gela.Elements;
with Gela.Element_Visiters;
with Gela.Element_Sequences;
with Gela.Compilation_Units;
with Gela.Declarations.Package_Declarations;

limited with Gela.A4G.Contexts;

package Gela.A4G.Elements is

   type Element is new Gela.Elements.Element
     and Gela.Declarations.Package_Declarations.Package_Declaration
       with private;

   type Element_Access is access all Element'Class;

   function Create
     (Context : not null access Gela.A4G.Contexts.Context'Class;
      Node    : Asis.Element) return Element_Access;

   type Element_Sequence is new Gela.Element_Sequences.Element_Sequence
   with private;

   type Element_Sequence_Access is access all Element_Sequence'Class;

   function Create_List
     (Context : not null access Gela.A4G.Contexts.Context'Class;
      Node    : Asis.Element_List) return Element_Sequence_Access;

private

   type Element is new Gela.Elements.Element
     and Gela.Declarations.Package_Declarations.Package_Declaration with
   record
      Context : not null access Gela.A4G.Contexts.Context'Class;
      Node    : Asis.Element;
      Next    : Element_Access;
   end record;

   overriding function Enclosing_Element
     (Self : aliased Element)
      return Gela.Elements.Element_Access;
   --  Return upper element if any

   overriding function Compilation_Unit
     (Self : aliased Element)
      return Gela.Compilation_Units.Compilation_Unit_Access;
   --  Return corresponding compilation unit

   overriding procedure Visit
     (Self    : aliased Element;
      Visiter : in out Gela.Element_Visiters.Visiter'Class);

   overriding function Names
     (Self : aliased Element)
      return Gela.Element_Sequences.Defining_Name_Sequence_Access;

   overriding function Aspects
     (Self : aliased Element)
      return Gela.Element_Sequences.Element_Sequence_Access;

   overriding function Visible_Part
     (Self : aliased Element)
      return Gela.Element_Sequences.Defining_Name_Sequence_Access;

   overriding function Private_Part
     (Self : aliased Element)
      return Gela.Element_Sequences.Defining_Name_Sequence_Access;

   type Element_Sequence is new Gela.Element_Sequences.Element_Sequence
   with null record;

   overriding function To_Element_Sequence
     (Self : aliased Element_Sequence)
      return Gela.Element_Sequences.Element_Sequence_Access;

   overriding function Is_Empty
     (Self : aliased Element_Sequence) return Boolean;

   overriding function Length
     (Self : aliased Element_Sequence) return Natural;

   overriding function Iterate
     (Self : aliased Element_Sequence)
      return Gela.Element_Sequences.Element_Sequences
               .Iterator_Interfaces.Forward_Iterator'Class;

end Gela.A4G.Elements;
