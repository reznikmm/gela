--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Defining_Identifiers;
with Program.Lexical_Elements;
with Program.Elements.Component_Definitions;
with Program.Elements.Expressions;
with Program.Elements.Aspect_Specifications;
with Program.Elements.Component_Declarations;
with Program.Element_Visitors;

package Program.Nodes.Component_Declarations is

   pragma Preelaborate;

   type Component_Declaration is
     new Program.Nodes.Node
         and Program.Elements.Component_Declarations.Component_Declaration
         and Program.Elements.Component_Declarations.Component_Declaration_Text
     with private;

   function Create
    (Names              : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Vector_Access;
     Colon_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Object_Subtype     : not null Program.Elements.Component_Definitions
         .Component_Definition_Access;
     Assignment_Token   : Program.Lexical_Elements.Lexical_Element_Access;
     Default_Expression : Program.Elements.Expressions.Expression_Access;
     With_Token         : Program.Lexical_Elements.Lexical_Element_Access;
     Aspects            : Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Semicolon_Token    : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Component_Declaration;

   type Implicit_Component_Declaration is
     new Program.Nodes.Node
         and Program.Elements.Component_Declarations.Component_Declaration
     with private;

   function Create
    (Names                : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Vector_Access;
     Object_Subtype       : not null Program.Elements.Component_Definitions
         .Component_Definition_Access;
     Default_Expression   : Program.Elements.Expressions.Expression_Access;
     Aspects              : Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Component_Declaration
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Component_Declaration is
     abstract new Program.Nodes.Node
       and Program.Elements.Component_Declarations.Component_Declaration
     with record
        Names              : not null Program.Elements.Defining_Identifiers
          .Defining_Identifier_Vector_Access;
        Object_Subtype     : not null Program.Elements.Component_Definitions
          .Component_Definition_Access;
        Default_Expression : Program.Elements.Expressions.Expression_Access;
        Aspects            : Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access;
     end record;

   procedure Initialize
    (Self : aliased in out Base_Component_Declaration'Class);

   overriding procedure Visit
    (Self    : not null access Base_Component_Declaration;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Names
    (Self : Base_Component_Declaration)
      return not null Program.Elements.Defining_Identifiers
          .Defining_Identifier_Vector_Access;

   overriding function Object_Subtype
    (Self : Base_Component_Declaration)
      return not null Program.Elements.Component_Definitions
          .Component_Definition_Access;

   overriding function Default_Expression
    (Self : Base_Component_Declaration)
      return Program.Elements.Expressions.Expression_Access;

   overriding function Aspects
    (Self : Base_Component_Declaration)
      return Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access;

   overriding function Is_Component_Declaration_Element
    (Self : Base_Component_Declaration)
      return Boolean;

   overriding function Is_Declaration_Element
    (Self : Base_Component_Declaration)
      return Boolean;

   type Component_Declaration is
     new Base_Component_Declaration
       and Program.Elements.Component_Declarations.Component_Declaration_Text
     with record
        Colon_Token      : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Assignment_Token : Program.Lexical_Elements.Lexical_Element_Access;
        With_Token       : Program.Lexical_Elements.Lexical_Element_Access;
        Semicolon_Token  : not null Program.Lexical_Elements
          .Lexical_Element_Access;
     end record;

   overriding function To_Component_Declaration_Text
    (Self : aliased in out Component_Declaration)
      return Program.Elements.Component_Declarations
          .Component_Declaration_Text_Access;

   overriding function Colon_Token
    (Self : Component_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Assignment_Token
    (Self : Component_Declaration)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function With_Token
    (Self : Component_Declaration)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Semicolon_Token
    (Self : Component_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   type Implicit_Component_Declaration is
     new Base_Component_Declaration
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
     end record;

   overriding function To_Component_Declaration_Text
    (Self : aliased in out Implicit_Component_Declaration)
      return Program.Elements.Component_Declarations
          .Component_Declaration_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Component_Declaration)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Component_Declaration)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Component_Declaration)
      return Boolean;

end Program.Nodes.Component_Declarations;
