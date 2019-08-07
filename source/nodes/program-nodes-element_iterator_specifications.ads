--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Defining_Identifiers;
with Program.Lexical_Elements;
with Program.Elements.Subtype_Indications;
with Program.Elements.Expressions;
with Program.Elements.Element_Iterator_Specifications;
with Program.Element_Visitors;

package Program.Nodes.Element_Iterator_Specifications is

   pragma Preelaborate;

   type Element_Iterator_Specification is
     new Program.Nodes.Node
         and Program.Elements.Element_Iterator_Specifications
           .Element_Iterator_Specification
         and Program.Elements.Element_Iterator_Specifications
           .Element_Iterator_Specification_Text
     with private;

   function Create
    (Name               : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Colon_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Subtype_Indication : not null Program.Elements.Subtype_Indications
         .Subtype_Indication_Access;
     Of_Token           : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Reverse_Token      : Program.Lexical_Elements.Lexical_Element_Access;
     Iterable_Name      : not null Program.Elements.Expressions
         .Expression_Access)
      return Element_Iterator_Specification;

   type Implicit_Element_Iterator_Specification is
     new Program.Nodes.Node
         and Program.Elements.Element_Iterator_Specifications
           .Element_Iterator_Specification
     with private;

   function Create
    (Name                 : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Subtype_Indication   : not null Program.Elements.Subtype_Indications
         .Subtype_Indication_Access;
     Iterable_Name        : not null Program.Elements.Expressions
         .Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False;
     Has_Reverse          : Boolean := False)
      return Implicit_Element_Iterator_Specification
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Element_Iterator_Specification is
     abstract new Program.Nodes.Node
       and Program.Elements.Element_Iterator_Specifications
         .Element_Iterator_Specification
     with record
        Name               : not null Program.Elements.Defining_Identifiers
          .Defining_Identifier_Access;
        Subtype_Indication : not null Program.Elements.Subtype_Indications
          .Subtype_Indication_Access;
        Iterable_Name      : not null Program.Elements.Expressions
          .Expression_Access;
     end record;

   procedure Initialize
    (Self : aliased in out Base_Element_Iterator_Specification'Class);

   overriding procedure Visit
    (Self    : not null access Base_Element_Iterator_Specification;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Name
    (Self : Base_Element_Iterator_Specification)
      return not null Program.Elements.Defining_Identifiers
          .Defining_Identifier_Access;

   overriding function Subtype_Indication
    (Self : Base_Element_Iterator_Specification)
      return not null Program.Elements.Subtype_Indications
          .Subtype_Indication_Access;

   overriding function Iterable_Name
    (Self : Base_Element_Iterator_Specification)
      return not null Program.Elements.Expressions.Expression_Access;

   overriding function Is_Element_Iterator_Specification
    (Self : Base_Element_Iterator_Specification)
      return Boolean;

   overriding function Is_Declaration
    (Self : Base_Element_Iterator_Specification)
      return Boolean;

   type Element_Iterator_Specification is
     new Base_Element_Iterator_Specification
       and Program.Elements.Element_Iterator_Specifications
         .Element_Iterator_Specification_Text
     with record
        Colon_Token   : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Of_Token      : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Reverse_Token : Program.Lexical_Elements.Lexical_Element_Access;
     end record;

   overriding function To_Element_Iterator_Specification_Text
    (Self : aliased in out Element_Iterator_Specification)
      return Program.Elements.Element_Iterator_Specifications
          .Element_Iterator_Specification_Text_Access;

   overriding function Colon_Token
    (Self : Element_Iterator_Specification)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Of_Token
    (Self : Element_Iterator_Specification)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Reverse_Token
    (Self : Element_Iterator_Specification)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Has_Reverse
    (Self : Element_Iterator_Specification)
      return Boolean;

   type Implicit_Element_Iterator_Specification is
     new Base_Element_Iterator_Specification
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
        Has_Reverse          : Boolean;
     end record;

   overriding function To_Element_Iterator_Specification_Text
    (Self : aliased in out Implicit_Element_Iterator_Specification)
      return Program.Elements.Element_Iterator_Specifications
          .Element_Iterator_Specification_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Element_Iterator_Specification)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Element_Iterator_Specification)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Element_Iterator_Specification)
      return Boolean;

   overriding function Has_Reverse
    (Self : Implicit_Element_Iterator_Specification)
      return Boolean;

end Program.Nodes.Element_Iterator_Specifications;
