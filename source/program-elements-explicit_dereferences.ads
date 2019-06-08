--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Expressions;
with Program.Tokens;

package Program.Elements.Explicit_Dereferences is

   pragma Pure (Program.Elements.Explicit_Dereferences);

   type Explicit_Dereference is
     limited interface and Program.Elements.Expressions.Expression;

   type Explicit_Dereference_Access is access all Explicit_Dereference'Class
     with Storage_Size => 0;

   not overriding function Prefix
    (Self : Explicit_Dereference)
      return not null Program.Elements.Expressions.Expression_Access
     is abstract;

   not overriding function Dot_Token
    (Self : Explicit_Dereference)
      return Program.Tokens.Token_Access is abstract;

   not overriding function All_Token
    (Self : Explicit_Dereference)
      return Program.Tokens.Token_Access is abstract;

end Program.Elements.Explicit_Dereferences;
