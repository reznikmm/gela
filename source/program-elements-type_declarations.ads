--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Declarations;
with Program.Lexical_Elements;
with Program.Elements.Defining_Identifiers;
with Program.Elements.Definitions;
with Program.Elements.Aspect_Specifications;

package Program.Elements.Type_Declarations is

   pragma Pure (Program.Elements.Type_Declarations);

   type Type_Declaration is
     limited interface and Program.Elements.Declarations.Declaration;

   type Type_Declaration_Access is access all Type_Declaration'Class
     with Storage_Size => 0;

   not overriding function Name
    (Self : Type_Declaration)
      return not null Program.Elements.Defining_Identifiers
          .Defining_Identifier_Access is abstract;

   not overriding function Discriminant_Part
    (Self : Type_Declaration)
      return Program.Elements.Definitions.Definition_Access is abstract;

   not overriding function Definition
    (Self : Type_Declaration)
      return not null Program.Elements.Definitions.Definition_Access
     is abstract;

   not overriding function Aspects
    (Self : Type_Declaration)
      return not null Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access is abstract;

   type Type_Declaration_Text is limited interface;

   type Type_Declaration_Text_Access is access all Type_Declaration_Text'Class
     with Storage_Size => 0;

   not overriding function To_Type_Declaration_Text
    (Self : aliased Type_Declaration)
      return Type_Declaration_Text_Access is abstract;

   not overriding function Type_Token
    (Self : Type_Declaration_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Is_Token
    (Self : Type_Declaration_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function With_Token
    (Self : Type_Declaration_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Semicolon_Token
    (Self : Type_Declaration_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

end Program.Elements.Type_Declarations;
