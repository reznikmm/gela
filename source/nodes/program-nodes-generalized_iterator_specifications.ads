--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Defining_Identifiers;
with Program.Lexical_Elements;
with Program.Elements.Expressions;
with Program.Elements.Generalized_Iterator_Specifications;
with Program.Element_Visitors;

package Program.Nodes.Generalized_Iterator_Specifications is

   pragma Preelaborate;

   type Generalized_Iterator_Specification is
     new Program.Nodes.Node
         and Program.Elements.Generalized_Iterator_Specifications
           .Generalized_Iterator_Specification
         and Program.Elements.Generalized_Iterator_Specifications
           .Generalized_Iterator_Specification_Text
     with private;

   function Create
    (Name          : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     In_Token      : not null Program.Lexical_Elements.Lexical_Element_Access;
     Reverse_Token : Program.Lexical_Elements.Lexical_Element_Access;
     Iterator_Name : not null Program.Elements.Expressions.Expression_Access)
      return Generalized_Iterator_Specification;

   type Implicit_Generalized_Iterator_Specification is
     new Program.Nodes.Node
         and Program.Elements.Generalized_Iterator_Specifications
           .Generalized_Iterator_Specification
     with private;

   function Create
    (Name                 : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Iterator_Name        : not null Program.Elements.Expressions
         .Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False;
     Has_Reverse          : Boolean := False)
      return Implicit_Generalized_Iterator_Specification
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Generalized_Iterator_Specification is
     abstract new Program.Nodes.Node
       and Program.Elements.Generalized_Iterator_Specifications
         .Generalized_Iterator_Specification
     with record
        Name          : not null Program.Elements.Defining_Identifiers
          .Defining_Identifier_Access;
        Iterator_Name : not null Program.Elements.Expressions
          .Expression_Access;
     end record;

   procedure Initialize
    (Self : aliased in out Base_Generalized_Iterator_Specification'Class);

   overriding procedure Visit
    (Self    : not null access Base_Generalized_Iterator_Specification;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Name
    (Self : Base_Generalized_Iterator_Specification)
      return not null Program.Elements.Defining_Identifiers
          .Defining_Identifier_Access;

   overriding function Iterator_Name
    (Self : Base_Generalized_Iterator_Specification)
      return not null Program.Elements.Expressions.Expression_Access;

   overriding function Is_Generalized_Iterator_Specification
    (Self : Base_Generalized_Iterator_Specification)
      return Boolean;

   overriding function Is_Declaration
    (Self : Base_Generalized_Iterator_Specification)
      return Boolean;

   type Generalized_Iterator_Specification is
     new Base_Generalized_Iterator_Specification
       and Program.Elements.Generalized_Iterator_Specifications
         .Generalized_Iterator_Specification_Text
     with record
        In_Token      : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Reverse_Token : Program.Lexical_Elements.Lexical_Element_Access;
     end record;

   overriding function To_Generalized_Iterator_Specification_Text
    (Self : aliased in out Generalized_Iterator_Specification)
      return Program.Elements.Generalized_Iterator_Specifications
          .Generalized_Iterator_Specification_Text_Access;

   overriding function In_Token
    (Self : Generalized_Iterator_Specification)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Reverse_Token
    (Self : Generalized_Iterator_Specification)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Has_Reverse
    (Self : Generalized_Iterator_Specification)
      return Boolean;

   type Implicit_Generalized_Iterator_Specification is
     new Base_Generalized_Iterator_Specification
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
        Has_Reverse          : Boolean;
     end record;

   overriding function To_Generalized_Iterator_Specification_Text
    (Self : aliased in out Implicit_Generalized_Iterator_Specification)
      return Program.Elements.Generalized_Iterator_Specifications
          .Generalized_Iterator_Specification_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Generalized_Iterator_Specification)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Generalized_Iterator_Specification)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Generalized_Iterator_Specification)
      return Boolean;

   overriding function Has_Reverse
    (Self : Implicit_Generalized_Iterator_Specification)
      return Boolean;

end Program.Nodes.Generalized_Iterator_Specifications;
