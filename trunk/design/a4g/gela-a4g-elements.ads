with Asis;

with Gela.Elements;
with Gela.Elements.Defining_Names;
with Gela.Elements.Pragmas;
with Gela.Element_Visiters;
with Gela.Element_Sequences;
with Gela.Compilation_Units;
with Gela.Declarations.Entry_Body_Declarations;
with Gela.Declarations.Entry_Declarations;
with Gela.Declarations.Generic_Subprogram_Declarations;
with Gela.Declarations.Package_Declarations;
with Gela.Declarations.Subprogram_Body_Declarations;
with Gela.Declarations.Subprogram_Declarations;
with Gela.Declarations.Subtype_Declarations;
with Gela.Declarations.Type_Declarations;
with Gela.Symbols;

limited with Gela.A4G.Contexts;

package Gela.A4G.Elements is

   type Element is new Gela.Elements.Element
     and Gela.Declarations.Entry_Body_Declarations.Entry_Body_Declaration
     and Gela.Declarations.Entry_Declarations.Entry_Declaration
     and Gela.Declarations.Generic_Subprogram_Declarations
       .Generic_Subprogram_Declaration
     and Gela.Declarations.Package_Declarations.Package_Declaration
     and Gela.Declarations.Subprogram_Body_Declarations
         .Subprogram_Body_Declaration
     and Gela.Declarations.Subprogram_Declarations.Subprogram_Declaration
     and Gela.Declarations.Subtype_Declarations.Subtype_Declaration
     and Gela.Declarations.Type_Declarations.Type_Declaration
     and Gela.Elements.Defining_Names.Defining_Name
     and Gela.Elements.Pragmas.Pragma_Element
       with private;

   type Element_Access is access all Element'Class;

   function Create
     (Context : not null access Gela.A4G.Contexts.Context'Class;
      Node    : Asis.Element) return Element_Access;

   type Element_Sequence is new Gela.Element_Sequences.Element_Sequence
     and Gela.Element_Sequences.Defining_Name_Sequence
   with private;

   type Element_Sequence_Access is access all Element_Sequence'Class;

   function Create_List
     (Context : not null access Gela.A4G.Contexts.Context'Class;
      Node    : Asis.Element_List) return Element_Sequence_Access;

private

   type Element is new Gela.Elements.Element
     and Gela.Declarations.Entry_Body_Declarations.Entry_Body_Declaration
     and Gela.Declarations.Entry_Declarations.Entry_Declaration
     and Gela.Declarations.Generic_Subprogram_Declarations
       .Generic_Subprogram_Declaration
     and Gela.Declarations.Package_Declarations.Package_Declaration
     and Gela.Declarations.Subprogram_Body_Declarations
         .Subprogram_Body_Declaration
     and Gela.Declarations.Subprogram_Declarations.Subprogram_Declaration
     and Gela.Declarations.Subtype_Declarations.Subtype_Declaration
     and Gela.Declarations.Type_Declarations.Type_Declaration
     and Gela.Elements.Defining_Names.Defining_Name
     and Gela.Elements.Pragmas.Pragma_Element
   with record
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
      Visiter : in out Gela.Element_Visiters.Abstract_Visiter'Class);

   overriding function Arguments
     (Self : aliased Element)
      return Gela.Element_Sequences.Element_Sequence_Access;

   overriding function Body_Declarative_Items
     (Self : aliased Element)
      return Gela.Element_Sequences.Element_Sequence_Access;

   overriding function Body_Statements
     (Self : aliased Element)
      return Gela.Element_Sequences.Element_Sequence_Access;

   overriding function Body_Exception_Handlers
     (Self : aliased Element)
      return Gela.Element_Sequences.Element_Sequence_Access;

   overriding function Entry_Index_Specification
     (Self : aliased Element) return Gela.Elements.Element_Access;

   overriding function Entry_Barrier
     (Self : aliased Element) return Gela.Elements.Element_Access;

   overriding function Entry_Family_Definition
     (Self : aliased Element) return Gela.Elements.Element_Access;

   overriding function Symbol
     (Self : aliased Element) return Gela.Symbols.Symbol_Access;

   overriding function Parameter_Prolile
     (Self : aliased Element)
      return Gela.Element_Sequences.Element_Sequence_Access;

   overriding function Result_Subtype
     (Self : aliased Element) return Gela.Elements.Element_Access;

   overriding function Prefix
     (Self : aliased Element) return Gela.Elements.Element_Access;

   overriding function Names
     (Self : aliased Element)
      return Gela.Element_Sequences.Defining_Name_Sequence_Access;

   overriding function Aspects
     (Self : aliased Element)
      return Gela.Element_Sequences.Element_Sequence_Access;

   overriding function Is_Abstract (Self : aliased Element) return Boolean;

   overriding function Is_Null_Procedure
     (Self : aliased Element) return Boolean;

   overriding function Is_Expression_Function
     (Self : aliased Element) return Boolean;

   overriding function Expression
     (Self : aliased Element) return Gela.Elements.Element_Access;

   overriding function Is_Renaming_Declaration
     (Self : aliased Element) return Boolean;

   overriding function Renamed_Entity
     (Self : aliased Element) return Gela.Elements.Element_Access;

   overriding function Is_Function (Self : aliased Element) return Boolean;

   overriding function Is_Procedure (Self : aliased Element) return Boolean;

   overriding function Generic_Formal_Part
     (Self : aliased Element)
      return Gela.Element_Sequences.Element_Sequence_Access;

   overriding function Visible_Part
     (Self : aliased Element)
      return Gela.Element_Sequences.Element_Sequence_Access;

   overriding function Private_Part
     (Self : aliased Element)
      return Gela.Element_Sequences.Element_Sequence_Access;

   overriding function Subtype_Indication
     (Self : aliased Element) return Gela.Elements.Element_Access;

   overriding function Discriminants
     (Self : aliased Element)
      return Gela.Element_Sequences.Element_Sequence_Access;

   overriding function Type_Definition
     (Self : aliased Element) return Gela.Elements.Element_Access;

   type Element_Sequence is new Gela.Element_Sequences.Element_Sequence
     and Gela.Element_Sequences.Defining_Name_Sequence
   with record
      First  : Element_Access;
      Length : Natural;
   end record;

   overriding function To_Element_Sequence
     (Self : aliased Element_Sequence)
      return Gela.Element_Sequences.Element_Sequence_Access;

   overriding function To_Parent_Sequence
     (Self : aliased Element_Sequence)
      return Gela.Element_Sequences.Element_Sequence_Access
        renames To_Element_Sequence;

   overriding function Is_Empty
     (Self : aliased Element_Sequence) return Boolean;

   overriding function Length
     (Self : aliased Element_Sequence) return Natural;

   overriding function Iterate
     (Self : aliased Element_Sequence)
      return Gela.Element_Sequences.Element_Sequences
               .Iterator_Interfaces.Forward_Iterator'Class;

   overriding function Iterate
     (Self : aliased Element_Sequence)
      return Gela.Element_Sequences.Defining_Name_Sequences
               .Iterator_Interfaces.Forward_Iterator'Class;

end Gela.A4G.Elements;
