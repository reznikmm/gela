--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Declarations;
with Program.Elements.Defining_Identifiers;
with Program.Lexical_Elements;
with Program.Elements.Definitions;
with Program.Elements.Expressions;
with Program.Elements.Aspect_Specifications;

package Program.Elements.Object_Declarations is

   pragma Pure (Program.Elements.Object_Declarations);

   type Object_Declaration is
     limited interface and Program.Elements.Declarations.Declaration;

   type Object_Declaration_Access is access all Object_Declaration'Class
     with Storage_Size => 0;

   not overriding function Names
    (Self : Object_Declaration)
      return not null Program.Elements.Defining_Identifiers
          .Defining_Identifier_Vector_Access is abstract;

   not overriding function Object_Subtype
    (Self : Object_Declaration)
      return not null Program.Elements.Definitions.Definition_Access
     is abstract;

   not overriding function Initialization_Expression
    (Self : Object_Declaration)
      return Program.Elements.Expressions.Expression_Access is abstract;

   not overriding function Aspects
    (Self : Object_Declaration)
      return not null Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access is abstract;

   type Object_Declaration_Text is limited interface;

   type Object_Declaration_Text_Access is
     access all Object_Declaration_Text'Class with Storage_Size => 0;

   not overriding function To_Object_Declaration_Text
    (Self : aliased Object_Declaration)
      return Object_Declaration_Text_Access is abstract;

   not overriding function Colon_Token
    (Self : Object_Declaration_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Aliased_Token
    (Self : Object_Declaration_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Constant_Token
    (Self : Object_Declaration_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Assignment_Token
    (Self : Object_Declaration_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function With_Token
    (Self : Object_Declaration_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Semicolon_Token
    (Self : Object_Declaration_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

end Program.Elements.Object_Declarations;
