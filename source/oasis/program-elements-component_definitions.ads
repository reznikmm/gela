--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Definitions;
with Program.Lexical_Elements;

package Program.Elements.Component_Definitions is

   pragma Pure (Program.Elements.Component_Definitions);

   type Component_Definition is
     limited interface and Program.Elements.Definitions.Definition;

   type Component_Definition_Access is access all Component_Definition'Class
     with Storage_Size => 0;

   not overriding function Subtype_Indication
    (Self : Component_Definition)
      return not null Program.Elements.Element_Access is abstract;

   not overriding function Has_Aliased
    (Self : Component_Definition)
      return Boolean is abstract;

   type Component_Definition_Text is limited interface;

   type Component_Definition_Text_Access is
     access all Component_Definition_Text'Class with Storage_Size => 0;

   not overriding function To_Component_Definition_Text
    (Self : aliased in out Component_Definition)
      return Component_Definition_Text_Access is abstract;

   not overriding function Aliased_Token
    (Self : Component_Definition_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

end Program.Elements.Component_Definitions;
