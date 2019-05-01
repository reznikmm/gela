--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Expressions;
with Program.Tokens;

package Program.Elements.Indexed_Components is

   pragma Pure (Program.Elements.Indexed_Components);

   type Indexed_Component is
     limited interface and Program.Elements.Expressions.Expression;

   type Indexed_Component_Access is access all Indexed_Component'Class
     with Storage_Size => 0;

   not overriding function Prefix
    (Self : Indexed_Component)
      return Program.Elements.Expressions.Expression_Access is abstract;

   not overriding function Left_Bracket_Token
    (Self : Indexed_Component)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Right_Bracket_Token
    (Self : Indexed_Component)
      return Program.Tokens.Token_Access is abstract;

end Program.Elements.Indexed_Components;
