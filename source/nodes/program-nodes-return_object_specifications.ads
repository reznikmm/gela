--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Defining_Identifiers;
with Program.Lexical_Elements;
with Program.Elements.Expressions;
with Program.Elements.Return_Object_Specifications;
with Program.Element_Visitors;

package Program.Nodes.Return_Object_Specifications is

   pragma Preelaborate;

   type Return_Object_Specification is
     new Program.Nodes.Node
         and Program.Elements.Return_Object_Specifications
           .Return_Object_Specification
         and Program.Elements.Return_Object_Specifications
           .Return_Object_Specification_Text
     with private;

   function Create
    (Name             : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Colon_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Aliased_Token    : Program.Lexical_Elements.Lexical_Element_Access;
     Constant_Token   : Program.Lexical_Elements.Lexical_Element_Access;
     Object_Subtype   : not null Program.Elements.Element_Access;
     Assignment_Token : Program.Lexical_Elements.Lexical_Element_Access;
     Expression       : Program.Elements.Expressions.Expression_Access)
      return Return_Object_Specification;

   type Implicit_Return_Object_Specification is
     new Program.Nodes.Node
         and Program.Elements.Return_Object_Specifications
           .Return_Object_Specification
     with private;

   function Create
    (Name                 : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Object_Subtype       : not null Program.Elements.Element_Access;
     Expression           : Program.Elements.Expressions.Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False;
     Has_Aliased          : Boolean := False;
     Has_Constant         : Boolean := False)
      return Implicit_Return_Object_Specification
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Return_Object_Specification is
     abstract new Program.Nodes.Node
       and Program.Elements.Return_Object_Specifications
         .Return_Object_Specification
     with record
        Name           : not null Program.Elements.Defining_Identifiers
          .Defining_Identifier_Access;
        Object_Subtype : not null Program.Elements.Element_Access;
        Expression     : Program.Elements.Expressions.Expression_Access;
     end record;

   procedure Initialize
    (Self : aliased in out Base_Return_Object_Specification'Class);

   overriding procedure Visit
    (Self    : not null access Base_Return_Object_Specification;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Name
    (Self : Base_Return_Object_Specification)
      return not null Program.Elements.Defining_Identifiers
          .Defining_Identifier_Access;

   overriding function Object_Subtype
    (Self : Base_Return_Object_Specification)
      return not null Program.Elements.Element_Access;

   overriding function Expression
    (Self : Base_Return_Object_Specification)
      return Program.Elements.Expressions.Expression_Access;

   overriding function Is_Return_Object_Specification_Element
    (Self : Base_Return_Object_Specification)
      return Boolean;

   overriding function Is_Declaration_Element
    (Self : Base_Return_Object_Specification)
      return Boolean;

   type Return_Object_Specification is
     new Base_Return_Object_Specification
       and Program.Elements.Return_Object_Specifications
         .Return_Object_Specification_Text
     with record
        Colon_Token      : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Aliased_Token    : Program.Lexical_Elements.Lexical_Element_Access;
        Constant_Token   : Program.Lexical_Elements.Lexical_Element_Access;
        Assignment_Token : Program.Lexical_Elements.Lexical_Element_Access;
     end record;

   overriding function To_Return_Object_Specification_Text
    (Self : aliased in out Return_Object_Specification)
      return Program.Elements.Return_Object_Specifications
          .Return_Object_Specification_Text_Access;

   overriding function Colon_Token
    (Self : Return_Object_Specification)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Aliased_Token
    (Self : Return_Object_Specification)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Constant_Token
    (Self : Return_Object_Specification)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Assignment_Token
    (Self : Return_Object_Specification)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Has_Aliased
    (Self : Return_Object_Specification)
      return Boolean;

   overriding function Has_Constant
    (Self : Return_Object_Specification)
      return Boolean;

   type Implicit_Return_Object_Specification is
     new Base_Return_Object_Specification
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
        Has_Aliased          : Boolean;
        Has_Constant         : Boolean;
     end record;

   overriding function To_Return_Object_Specification_Text
    (Self : aliased in out Implicit_Return_Object_Specification)
      return Program.Elements.Return_Object_Specifications
          .Return_Object_Specification_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Return_Object_Specification)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Return_Object_Specification)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Return_Object_Specification)
      return Boolean;

   overriding function Has_Aliased
    (Self : Implicit_Return_Object_Specification)
      return Boolean;

   overriding function Has_Constant
    (Self : Implicit_Return_Object_Specification)
      return Boolean;

end Program.Nodes.Return_Object_Specifications;
