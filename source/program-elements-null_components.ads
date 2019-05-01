--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Definitions;
with Program.Tokens;

package Program.Elements.Null_Components is

   pragma Pure (Program.Elements.Null_Components);

   type Null_Component is
     limited interface and Program.Elements.Definitions.Definition;

   type Null_Component_Access is access all Null_Component'Class
     with Storage_Size => 0;

   not overriding function Null_Token
    (Self : Null_Component)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Semicolon_Token
    (Self : Null_Component)
      return Program.Tokens.Token_Access is abstract;

end Program.Elements.Null_Components;
