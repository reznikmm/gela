--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Defining_Identifiers;
with Program.Elements.Enumeration_Literal_Specifications;
with Program.Element_Visitors;

package Program.Nodes.Enumeration_Literal_Specifications is

   pragma Preelaborate;

   type Enumeration_Literal_Specification is
     new Program.Nodes.Node
         and Program.Elements.Enumeration_Literal_Specifications
           .Enumeration_Literal_Specification
         and Program.Elements.Enumeration_Literal_Specifications
           .Enumeration_Literal_Specification_Text
     with private;

   function Create
    (Name : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access)
      return Enumeration_Literal_Specification;

   type Implicit_Enumeration_Literal_Specification is
     new Program.Nodes.Node
         and Program.Elements.Enumeration_Literal_Specifications
           .Enumeration_Literal_Specification
     with private;

   function Create
    (Name                 : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Enumeration_Literal_Specification
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Enumeration_Literal_Specification is
     abstract new Program.Nodes.Node
       and Program.Elements.Enumeration_Literal_Specifications
         .Enumeration_Literal_Specification
     with record
        Name : not null Program.Elements.Defining_Identifiers
          .Defining_Identifier_Access;
     end record;

   procedure Initialize
    (Self : aliased in out Base_Enumeration_Literal_Specification'Class);

   overriding procedure Visit
    (Self    : not null access Base_Enumeration_Literal_Specification;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Name
    (Self : Base_Enumeration_Literal_Specification)
      return not null Program.Elements.Defining_Identifiers
          .Defining_Identifier_Access;

   overriding function Is_Enumeration_Literal_Specification_Element
    (Self : Base_Enumeration_Literal_Specification)
      return Boolean;

   overriding function Is_Declaration_Element
    (Self : Base_Enumeration_Literal_Specification)
      return Boolean;

   type Enumeration_Literal_Specification is
     new Base_Enumeration_Literal_Specification
       and Program.Elements.Enumeration_Literal_Specifications
         .Enumeration_Literal_Specification_Text
     with null record;

   overriding function To_Enumeration_Literal_Specification_Text
    (Self : aliased in out Enumeration_Literal_Specification)
      return Program.Elements.Enumeration_Literal_Specifications
          .Enumeration_Literal_Specification_Text_Access;

   type Implicit_Enumeration_Literal_Specification is
     new Base_Enumeration_Literal_Specification
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
     end record;

   overriding function To_Enumeration_Literal_Specification_Text
    (Self : aliased in out Implicit_Enumeration_Literal_Specification)
      return Program.Elements.Enumeration_Literal_Specifications
          .Enumeration_Literal_Specification_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Enumeration_Literal_Specification)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Enumeration_Literal_Specification)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Enumeration_Literal_Specification)
      return Boolean;

end Program.Nodes.Enumeration_Literal_Specifications;
