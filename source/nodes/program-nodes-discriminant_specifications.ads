--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Defining_Identifiers;
with Program.Lexical_Elements;
with Program.Elements.Expressions;
with Program.Elements.Discriminant_Specifications;
with Program.Element_Visitors;

package Program.Nodes.Discriminant_Specifications is

   pragma Preelaborate;

   type Discriminant_Specification is
     new Program.Nodes.Node
         and Program.Elements.Discriminant_Specifications
           .Discriminant_Specification
         and Program.Elements.Discriminant_Specifications
           .Discriminant_Specification_Text
     with private;

   function Create
    (Names              : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Vector_Access;
     Colon_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Not_Token          : Program.Lexical_Elements.Lexical_Element_Access;
     Null_Token         : Program.Lexical_Elements.Lexical_Element_Access;
     Object_Subtype     : not null Program.Elements.Element_Access;
     Assignment_Token   : Program.Lexical_Elements.Lexical_Element_Access;
     Default_Expression : Program.Elements.Expressions.Expression_Access;
     Semicolon_Token    : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Discriminant_Specification;

   type Implicit_Discriminant_Specification is
     new Program.Nodes.Node
         and Program.Elements.Discriminant_Specifications
           .Discriminant_Specification
     with private;

   function Create
    (Names                : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Vector_Access;
     Object_Subtype       : not null Program.Elements.Element_Access;
     Default_Expression   : Program.Elements.Expressions.Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False;
     Has_Not_Null         : Boolean := False)
      return Implicit_Discriminant_Specification
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Discriminant_Specification is
     abstract new Program.Nodes.Node
       and Program.Elements.Discriminant_Specifications
         .Discriminant_Specification
     with record
        Names              : not null Program.Elements.Defining_Identifiers
          .Defining_Identifier_Vector_Access;
        Object_Subtype     : not null Program.Elements.Element_Access;
        Default_Expression : Program.Elements.Expressions.Expression_Access;
     end record;

   procedure Initialize
    (Self : aliased in out Base_Discriminant_Specification'Class);

   overriding procedure Visit
    (Self    : not null access Base_Discriminant_Specification;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Names
    (Self : Base_Discriminant_Specification)
      return not null Program.Elements.Defining_Identifiers
          .Defining_Identifier_Vector_Access;

   overriding function Object_Subtype
    (Self : Base_Discriminant_Specification)
      return not null Program.Elements.Element_Access;

   overriding function Default_Expression
    (Self : Base_Discriminant_Specification)
      return Program.Elements.Expressions.Expression_Access;

   overriding function Is_Discriminant_Specification_Element
    (Self : Base_Discriminant_Specification)
      return Boolean;

   overriding function Is_Declaration_Element
    (Self : Base_Discriminant_Specification)
      return Boolean;

   type Discriminant_Specification is
     new Base_Discriminant_Specification
       and Program.Elements.Discriminant_Specifications
         .Discriminant_Specification_Text
     with record
        Colon_Token      : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Not_Token        : Program.Lexical_Elements.Lexical_Element_Access;
        Null_Token       : Program.Lexical_Elements.Lexical_Element_Access;
        Assignment_Token : Program.Lexical_Elements.Lexical_Element_Access;
        Semicolon_Token  : not null Program.Lexical_Elements
          .Lexical_Element_Access;
     end record;

   overriding function To_Discriminant_Specification_Text
    (Self : aliased in out Discriminant_Specification)
      return Program.Elements.Discriminant_Specifications
          .Discriminant_Specification_Text_Access;

   overriding function Colon_Token
    (Self : Discriminant_Specification)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Not_Token
    (Self : Discriminant_Specification)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Null_Token
    (Self : Discriminant_Specification)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Assignment_Token
    (Self : Discriminant_Specification)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Semicolon_Token
    (Self : Discriminant_Specification)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Has_Not_Null
    (Self : Discriminant_Specification)
      return Boolean;

   type Implicit_Discriminant_Specification is
     new Base_Discriminant_Specification
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
        Has_Not_Null         : Boolean;
     end record;

   overriding function To_Discriminant_Specification_Text
    (Self : aliased in out Implicit_Discriminant_Specification)
      return Program.Elements.Discriminant_Specifications
          .Discriminant_Specification_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Discriminant_Specification)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Discriminant_Specification)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Discriminant_Specification)
      return Boolean;

   overriding function Has_Not_Null
    (Self : Implicit_Discriminant_Specification)
      return Boolean;

end Program.Nodes.Discriminant_Specifications;
