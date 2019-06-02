--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Expressions;
with Program.Tokens;
with Program.Elements.Identifiers;

package Program.Elements.Attribute_References is

   pragma Pure (Program.Elements.Attribute_References);

   type Attribute_Reference is
     limited interface and Program.Elements.Expressions.Expression;

   type Attribute_Reference_Access is access all Attribute_Reference'Class
     with Storage_Size => 0;

   not overriding function Prefix
    (Self : Attribute_Reference)
      return Program.Elements.Expressions.Expression_Access is abstract;

   not overriding function Apostrophe_Token
    (Self : Attribute_Reference)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Attribute_Designator
    (Self : Attribute_Reference)
      return Program.Elements.Identifiers.Identifier_Access is abstract;

   not overriding function Left_Bracket_Token
    (Self : Attribute_Reference)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Expressions
    (Self : Attribute_Reference)
      return Program.Elements.Expressions.Expression_Access is abstract;

   not overriding function Right_Bracket_Token
    (Self : Attribute_Reference)
      return Program.Tokens.Token_Access is abstract;

end Program.Elements.Attribute_References;
