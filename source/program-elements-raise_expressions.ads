--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Expressions;
with Program.Tokens;

package Program.Elements.Raise_Expressions is

   pragma Pure (Program.Elements.Raise_Expressions);

   type Raise_Expression is
     limited interface and Program.Elements.Expressions.Expression;

   type Raise_Expression_Access is access all Raise_Expression'Class
     with Storage_Size => 0;

   not overriding function Raise_Token
    (Self : Raise_Expression)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Exception_Name
    (Self : Raise_Expression)
      return not null Program.Elements.Expressions.Expression_Access
     is abstract;

   not overriding function With_Token
    (Self : Raise_Expression)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Associated_Message
    (Self : Raise_Expression)
      return Program.Elements.Expressions.Expression_Access is abstract;

end Program.Elements.Raise_Expressions;
