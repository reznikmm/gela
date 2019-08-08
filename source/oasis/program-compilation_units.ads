--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

limited with Program.Compilations;
limited with Program.Library_Items;
limited with Program.Library_Unit_Bodies;
limited with Program.Library_Unit_Declarations;
limited with Program.Subunits;

with Program.Element_Vectors;
with Program.Elements;

package Program.Compilation_Units is
   pragma Pure;

   --  A specific Compilation_Unit value is valid (usable) for as long as the
   --  Context variable, used to create it, remains open. Once an Context is
   --  closed, all associated Compilation_Unit values become invalid. It is
   --  erroneous to use an invalid Compilation_Unit value.

   type Compilation_Unit is limited interface;
   --  The Ada Compilation Unit abstraction:
   --
   --  The text of a program is submitted to the compiler in one or more
   --  compilations.  Each compilation is a succession of compilation units.
   --
   --  Compilation units are composed of three distinct parts:
   --
   --  a) A context clause.
   --
   --  b) The declaration of a library_item or unit.
   --
--  c) Pragmas that apply to the compilation, of which the unit is a part.
   --
   --  The context clause contains zero or more with clauses, use clauses,
   --  pragma elaborates, and possibly other pragmas.
   --
   --  We treat Pragmas that appear immediately after the context clause and
   --  before the subsequent declaration part as belonging to the context
   --  clause part.
   --
   --  The declaration associated with a compilation unit is one of: a package,
   --  a procedure, a function, a generic, or a subunit for normal units.
   --
   --  The abstract type Compilation_Unit is a handle for compilation units as
   --  a whole. An object of the type Compilation_Unit deals with the external
   --  view of compilation units such as their relationships with other units
   --  or their compilation attributes.

   type Compilation_Unit_Access is access all Compilation_Unit'Class
     with Storage_Size => 0;

   function Assigned (Self : access Compilation_Unit'Class) return Boolean
     is (Self /= null);

   not overriding function Compilation (Self : Compilation_Unit)
     return Program.Compilations.Compilation_Access is abstract;
   --  Return corresponding compilation

   not overriding function Full_Name
     (Self : Compilation_Unit) return Text is abstract;
   --  Returns the string image of the fully expanded Ada name of the given
   --  compilation unit.  This may be a simple name ("A") of a root library
   --  unit, or an expanded name ("A.B") of a subunit or non-root child unit.
   --  An expanded name shall contain the full parent_unit_name as its prefix.

   not overriding function Context_Clause_Elements
     (Self : Compilation_Unit)
      return Program.Element_Vectors.Element_Vector_Access is abstract
     with Post'Class =>
       (Context_Clause_Elements'Result.Is_Empty
        or else (for all X in Context_Clause_Elements'Result.Each_Element
                   => X.Element.Is_Pragma
                      or X.Element.Is_With_Clause
                      or X.Element.Is_Use_Clause));
--  Returns a list of with clauses, use clauses, and pragmas that explicitly
--  appear in the context clause of the compilation unit, in their order of
--  appearance.

   not overriding function Unit_Declaration (Self : Compilation_Unit)
     return not null Program.Elements.Element_Access is abstract
       with Post'Class => (Unit_Declaration'Result.Is_Declaration);
   --  Returns the element representing the declaration of the compilation_unit

   not overriding function Is_Subunit_Unit
     (Self : Compilation_Unit) return Boolean is abstract;
   --  Return True if Self is a subunit.

   function Is_Subunit
     (Self : Compilation_Unit'Class) return Boolean
       is (Self.Is_Subunit_Unit);
   --  Return True if Self is a subunit.

   function To_Subunit (Self : access Compilation_Unit'Class)
     return Program.Subunits.Subunit_Access
       with Pre => Self.Is_Subunit;
   --  Convert to the subunit type.

   not overriding function Is_Library_Item_Unit
     (Self : Compilation_Unit) return Boolean is abstract;
   --  Return True if Self is a library_item.

   function Is_Library_Item
     (Self : Compilation_Unit'Class) return Boolean
       is (Self.Is_Library_Item_Unit);
   --  Return True if Self is a library_item.

   function To_Library_Item (Self : access Compilation_Unit'Class)
     return Program.Library_Items.Library_Item_Access
       with Pre => Self.Is_Library_Item;
   --  Convert to the library_item type.

   not overriding function Is_Library_Unit_Body_Unit
     (Self : Compilation_Unit) return Boolean is abstract;
   --  Return True if Self is a library_unit_body.

   function Is_Library_Unit_Body
     (Self : Compilation_Unit'Class) return Boolean
       is (Self.Is_Library_Unit_Body_Unit);
   --  Return True if Self is a library_unit_body.

   function To_Library_Unit_Body (Self : access Compilation_Unit'Class)
     return Program.Library_Unit_Bodies.Library_Unit_Body_Access
       with Pre => Self.Is_Library_Unit_Body;
   --  Convert to the library_unit_body type.

   not overriding function Is_Library_Unit_Declaration_Unit
     (Self : Compilation_Unit) return Boolean is abstract;
   --  Return True if Self is a library_unit_declaration.

   function Is_Library_Unit_Declaration
     (Self : Compilation_Unit'Class) return Boolean
       is (Self.Is_Library_Unit_Declaration_Unit);
   --  Return True if Self is a library_unit_declaration.

   function To_Library_Unit_Declaration (Self : access Compilation_Unit'Class)
     return Program.Library_Unit_Declarations.Library_Unit_Declaration_Access
       with Pre => Self.Is_Library_Unit_Declaration;
   --  Convert to the library_unit_declaration type.

end Program.Compilation_Units;
