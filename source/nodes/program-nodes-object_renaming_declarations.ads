--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Defining_Identifiers;
with Program.Lexical_Elements;
with Program.Elements.Expressions;
with Program.Elements.Aspect_Specifications;
with Program.Elements.Object_Renaming_Declarations;
with Program.Element_Visitors;

package Program.Nodes.Object_Renaming_Declarations is

   pragma Pure (Program.Nodes.Object_Renaming_Declarations);

   type Object_Renaming_Declaration is
     new Program.Nodes.Node
         and Program.Elements.Object_Renaming_Declarations
           .Object_Renaming_Declaration
         and Program.Elements.Object_Renaming_Declarations
           .Object_Renaming_Declaration_Text
     with private;

   function Create
    (Names           : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Vector_Access;
     Colon_Token     : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Not_Token       : Program.Lexical_Elements.Lexical_Element_Access;
     Null_Token      : Program.Lexical_Elements.Lexical_Element_Access;
     Object_Subtype  : not null Program.Elements.Element_Access;
     Renames_Token   : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Renamed_Object  : not null Program.Elements.Expressions.Expression_Access;
     With_Token      : Program.Lexical_Elements.Lexical_Element_Access;
     Aspects         : Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Semicolon_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Object_Renaming_Declaration;

   type Implicit_Object_Renaming_Declaration is
     new Program.Nodes.Node
         and Program.Elements.Object_Renaming_Declarations
           .Object_Renaming_Declaration
     with private;

   function Create
    (Names                : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Vector_Access;
     Object_Subtype       : not null Program.Elements.Element_Access;
     Renamed_Object       : not null Program.Elements.Expressions
         .Expression_Access;
     Aspects              : Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False;
     Has_Not_Null         : Boolean := False)
      return Implicit_Object_Renaming_Declaration
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Object_Renaming_Declaration is
     abstract new Program.Nodes.Node
       and Program.Elements.Object_Renaming_Declarations
         .Object_Renaming_Declaration
     with record
        Names          : not null Program.Elements.Defining_Identifiers
          .Defining_Identifier_Vector_Access;
        Object_Subtype : not null Program.Elements.Element_Access;
        Renamed_Object : not null Program.Elements.Expressions
          .Expression_Access;
        Aspects        : Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access;
     end record;

   procedure Initialize
    (Self : aliased in out Base_Object_Renaming_Declaration'Class);

   overriding procedure Visit
    (Self    : not null access Base_Object_Renaming_Declaration;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Names
    (Self : Base_Object_Renaming_Declaration)
      return not null Program.Elements.Defining_Identifiers
          .Defining_Identifier_Vector_Access;

   overriding function Object_Subtype
    (Self : Base_Object_Renaming_Declaration)
      return not null Program.Elements.Element_Access;

   overriding function Renamed_Object
    (Self : Base_Object_Renaming_Declaration)
      return not null Program.Elements.Expressions.Expression_Access;

   overriding function Aspects
    (Self : Base_Object_Renaming_Declaration)
      return Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access;

   overriding function Is_Object_Renaming_Declaration
    (Self : Base_Object_Renaming_Declaration)
      return Boolean;

   overriding function Is_Declaration
    (Self : Base_Object_Renaming_Declaration)
      return Boolean;

   type Object_Renaming_Declaration is
     new Base_Object_Renaming_Declaration
       and Program.Elements.Object_Renaming_Declarations
         .Object_Renaming_Declaration_Text
     with record
        Colon_Token     : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Not_Token       : Program.Lexical_Elements.Lexical_Element_Access;
        Null_Token      : Program.Lexical_Elements.Lexical_Element_Access;
        Renames_Token   : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        With_Token      : Program.Lexical_Elements.Lexical_Element_Access;
        Semicolon_Token : not null Program.Lexical_Elements
          .Lexical_Element_Access;
     end record;

   overriding function To_Object_Renaming_Declaration_Text
    (Self : aliased in out Object_Renaming_Declaration)
      return Program.Elements.Object_Renaming_Declarations
          .Object_Renaming_Declaration_Text_Access;

   overriding function Colon_Token
    (Self : Object_Renaming_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Not_Token
    (Self : Object_Renaming_Declaration)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Null_Token
    (Self : Object_Renaming_Declaration)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Renames_Token
    (Self : Object_Renaming_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function With_Token
    (Self : Object_Renaming_Declaration)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Semicolon_Token
    (Self : Object_Renaming_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Has_Not_Null
    (Self : Object_Renaming_Declaration)
      return Boolean;

   type Implicit_Object_Renaming_Declaration is
     new Base_Object_Renaming_Declaration
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
        Has_Not_Null         : Boolean;
     end record;

   overriding function To_Object_Renaming_Declaration_Text
    (Self : aliased in out Implicit_Object_Renaming_Declaration)
      return Program.Elements.Object_Renaming_Declarations
          .Object_Renaming_Declaration_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Object_Renaming_Declaration)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Object_Renaming_Declaration)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Object_Renaming_Declaration)
      return Boolean;

   overriding function Has_Not_Null
    (Self : Implicit_Object_Renaming_Declaration)
      return Boolean;

end Program.Nodes.Object_Renaming_Declarations;
