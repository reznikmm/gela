--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Definitions;
with Program.Tokens;

package Program.Elements.Component_Definitions is

   pragma Pure (Program.Elements.Component_Definitions);

   type Component_Definition is
     limited interface and Program.Elements.Definitions.Definition;

   type Component_Definition_Access is access all Component_Definition'Class
     with Storage_Size => 0;

   not overriding function Aliased_Token
    (Self : Component_Definition)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Subtype_Indication
    (Self : Component_Definition)
      return not null Program.Elements.Element_Access is abstract;

end Program.Elements.Component_Definitions;
