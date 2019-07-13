--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Declarations;
with Program.Elements.Defining_Identifiers;
with Program.Lexical_Elements;
with Program.Elements.Expressions;
with Program.Elements.Aspect_Specifications;

package Program.Elements.Formal_Object_Declarations is

   pragma Pure (Program.Elements.Formal_Object_Declarations);

   type Formal_Object_Declaration is
     limited interface and Program.Elements.Declarations.Declaration;

   type Formal_Object_Declaration_Access is
     access all Formal_Object_Declaration'Class with Storage_Size => 0;

   not overriding function Names
    (Self : Formal_Object_Declaration)
      return not null Program.Elements.Defining_Identifiers
          .Defining_Identifier_Vector_Access is abstract;

   not overriding function Object_Subtype
    (Self : Formal_Object_Declaration)
      return not null Program.Elements.Element_Access is abstract;

   not overriding function Default_Expression
    (Self : Formal_Object_Declaration)
      return Program.Elements.Expressions.Expression_Access is abstract;

   not overriding function Aspects
    (Self : Formal_Object_Declaration)
      return not null Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access is abstract;

   not overriding function Has_In
    (Self : Formal_Object_Declaration)
      return Boolean is abstract;

   not overriding function Has_Out
    (Self : Formal_Object_Declaration)
      return Boolean is abstract;

   not overriding function Has_Not_Null
    (Self : Formal_Object_Declaration)
      return Boolean is abstract;

   type Formal_Object_Declaration_Text is limited interface;

   type Formal_Object_Declaration_Text_Access is
     access all Formal_Object_Declaration_Text'Class with Storage_Size => 0;

   not overriding function To_Formal_Object_Declaration_Text
    (Self : aliased Formal_Object_Declaration)
      return Formal_Object_Declaration_Text_Access is abstract;

   not overriding function Colon_Token
    (Self : Formal_Object_Declaration_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function In_Token
    (Self : Formal_Object_Declaration_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Out_Token
    (Self : Formal_Object_Declaration_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Not_Token
    (Self : Formal_Object_Declaration_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Null_Token
    (Self : Formal_Object_Declaration_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Assignment_Token
    (Self : Formal_Object_Declaration_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function With_Token
    (Self : Formal_Object_Declaration_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Semicolon_Token
    (Self : Formal_Object_Declaration_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

end Program.Elements.Formal_Object_Declarations;
