--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Declarations;
with Program.Elements.Defining_Identifiers;
with Program.Lexical_Elements;
with Program.Elements.Expressions;

package Program.Elements.Number_Declarations is

   pragma Pure (Program.Elements.Number_Declarations);

   type Number_Declaration is
     limited interface and Program.Elements.Declarations.Declaration;

   type Number_Declaration_Access is access all Number_Declaration'Class
     with Storage_Size => 0;

   not overriding function Names
    (Self : Number_Declaration)
      return not null Program.Elements.Defining_Identifiers
          .Defining_Identifier_Vector_Access is abstract;

   not overriding function Expression
    (Self : Number_Declaration)
      return not null Program.Elements.Expressions.Expression_Access
     is abstract;

   type Number_Declaration_Text is limited interface;

   type Number_Declaration_Text_Access is
     access all Number_Declaration_Text'Class with Storage_Size => 0;

   not overriding function To_Number_Declaration_Text
    (Self : aliased Number_Declaration)
      return Number_Declaration_Text_Access is abstract;

   not overriding function Colon_Token
    (Self : Number_Declaration_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Constant_Token
    (Self : Number_Declaration_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Assignment_Token
    (Self : Number_Declaration_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Semicolon_Token
    (Self : Number_Declaration_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

end Program.Elements.Number_Declarations;
