--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Defining_Identifiers;
with Program.Lexical_Elements;
with Program.Elements.Definitions;
with Program.Elements.Expressions;
with Program.Elements.Aspect_Specifications;
with Program.Elements.Object_Declarations;
with Program.Element_Visitors;

package Program.Nodes.Object_Declarations is

   pragma Preelaborate;

   type Object_Declaration is
     new Program.Nodes.Node
         and Program.Elements.Object_Declarations.Object_Declaration
         and Program.Elements.Object_Declarations.Object_Declaration_Text
     with private;

   function Create
    (Names                     : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Vector_Access;
     Colon_Token               : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Aliased_Token             : Program.Lexical_Elements
         .Lexical_Element_Access;
     Constant_Token            : Program.Lexical_Elements
         .Lexical_Element_Access;
     Object_Subtype            : not null Program.Elements.Definitions
         .Definition_Access;
     Assignment_Token          : Program.Lexical_Elements
         .Lexical_Element_Access;
     Initialization_Expression : Program.Elements.Expressions
         .Expression_Access;
     With_Token                : Program.Lexical_Elements
         .Lexical_Element_Access;
     Aspects                   : Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Semicolon_Token           : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Object_Declaration;

   type Implicit_Object_Declaration is
     new Program.Nodes.Node
         and Program.Elements.Object_Declarations.Object_Declaration
     with private;

   function Create
    (Names                     : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Vector_Access;
     Object_Subtype            : not null Program.Elements.Definitions
         .Definition_Access;
     Initialization_Expression : Program.Elements.Expressions
         .Expression_Access;
     Aspects                   : Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Is_Part_Of_Implicit       : Boolean := False;
     Is_Part_Of_Inherited      : Boolean := False;
     Is_Part_Of_Instance       : Boolean := False;
     Has_Aliased               : Boolean := False;
     Has_Constant              : Boolean := False)
      return Implicit_Object_Declaration
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Object_Declaration is
     abstract new Program.Nodes.Node
       and Program.Elements.Object_Declarations.Object_Declaration
     with record
        Names                     : not null Program.Elements
          .Defining_Identifiers.Defining_Identifier_Vector_Access;
        Object_Subtype            : not null Program.Elements.Definitions
          .Definition_Access;
        Initialization_Expression : Program.Elements.Expressions
          .Expression_Access;
        Aspects                   : Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access;
     end record;

   procedure Initialize (Self : aliased in out Base_Object_Declaration'Class);

   overriding procedure Visit
    (Self    : not null access Base_Object_Declaration;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Names
    (Self : Base_Object_Declaration)
      return not null Program.Elements.Defining_Identifiers
          .Defining_Identifier_Vector_Access;

   overriding function Object_Subtype
    (Self : Base_Object_Declaration)
      return not null Program.Elements.Definitions.Definition_Access;

   overriding function Initialization_Expression
    (Self : Base_Object_Declaration)
      return Program.Elements.Expressions.Expression_Access;

   overriding function Aspects
    (Self : Base_Object_Declaration)
      return Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access;

   overriding function Is_Object_Declaration_Element
    (Self : Base_Object_Declaration)
      return Boolean;

   overriding function Is_Declaration_Element
    (Self : Base_Object_Declaration)
      return Boolean;

   type Object_Declaration is
     new Base_Object_Declaration
       and Program.Elements.Object_Declarations.Object_Declaration_Text
     with record
        Colon_Token      : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Aliased_Token    : Program.Lexical_Elements.Lexical_Element_Access;
        Constant_Token   : Program.Lexical_Elements.Lexical_Element_Access;
        Assignment_Token : Program.Lexical_Elements.Lexical_Element_Access;
        With_Token       : Program.Lexical_Elements.Lexical_Element_Access;
        Semicolon_Token  : not null Program.Lexical_Elements
          .Lexical_Element_Access;
     end record;

   overriding function To_Object_Declaration_Text
    (Self : aliased in out Object_Declaration)
      return Program.Elements.Object_Declarations
          .Object_Declaration_Text_Access;

   overriding function Colon_Token
    (Self : Object_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Aliased_Token
    (Self : Object_Declaration)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Constant_Token
    (Self : Object_Declaration)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Assignment_Token
    (Self : Object_Declaration)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function With_Token
    (Self : Object_Declaration)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Semicolon_Token
    (Self : Object_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Has_Aliased (Self : Object_Declaration) return Boolean;

   overriding function Has_Constant (Self : Object_Declaration) return Boolean;

   type Implicit_Object_Declaration is
     new Base_Object_Declaration
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
        Has_Aliased          : Boolean;
        Has_Constant         : Boolean;
     end record;

   overriding function To_Object_Declaration_Text
    (Self : aliased in out Implicit_Object_Declaration)
      return Program.Elements.Object_Declarations
          .Object_Declaration_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Object_Declaration)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Object_Declaration)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Object_Declaration)
      return Boolean;

   overriding function Has_Aliased
    (Self : Implicit_Object_Declaration)
      return Boolean;

   overriding function Has_Constant
    (Self : Implicit_Object_Declaration)
      return Boolean;

end Program.Nodes.Object_Declarations;
