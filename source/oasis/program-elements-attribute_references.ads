--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Expressions;
with Program.Lexical_Elements;
with Program.Elements.Identifiers;

package Program.Elements.Attribute_References is

   pragma Pure (Program.Elements.Attribute_References);

   type Attribute_Reference is
     limited interface and Program.Elements.Expressions.Expression;

   type Attribute_Reference_Access is access all Attribute_Reference'Class
     with Storage_Size => 0;

   not overriding function Prefix
    (Self : Attribute_Reference)
      return not null Program.Elements.Expressions.Expression_Access
     is abstract;

   not overriding function Attribute_Designator
    (Self : Attribute_Reference)
      return not null Program.Elements.Identifiers.Identifier_Access
     is abstract;

   not overriding function Expressions
    (Self : Attribute_Reference)
      return Program.Elements.Expressions.Expression_Access is abstract;

   type Attribute_Reference_Text is limited interface;

   type Attribute_Reference_Text_Access is
     access all Attribute_Reference_Text'Class with Storage_Size => 0;

   not overriding function To_Attribute_Reference_Text
    (Self : aliased in out Attribute_Reference)
      return Attribute_Reference_Text_Access is abstract;

   not overriding function Apostrophe_Token
    (Self : Attribute_Reference_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Left_Bracket_Token
    (Self : Attribute_Reference_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Right_Bracket_Token
    (Self : Attribute_Reference_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

end Program.Elements.Attribute_References;
