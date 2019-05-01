--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Expressions;
with Program.Tokens;

package Program.Elements.Identifiers is

   pragma Pure (Program.Elements.Identifiers);

   type Identifier is
     limited interface and Program.Elements.Expressions.Expression;

   type Identifier_Access is access all Identifier'Class
     with Storage_Size => 0;

   not overriding function Identifier_Token
    (Self : Identifier)
      return Program.Tokens.Token_Access is abstract;

end Program.Elements.Identifiers;
