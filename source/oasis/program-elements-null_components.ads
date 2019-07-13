--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Definitions;
with Program.Lexical_Elements;

package Program.Elements.Null_Components is

   pragma Pure (Program.Elements.Null_Components);

   type Null_Component is
     limited interface and Program.Elements.Definitions.Definition;

   type Null_Component_Access is access all Null_Component'Class
     with Storage_Size => 0;

   type Null_Component_Text is limited interface;

   type Null_Component_Text_Access is access all Null_Component_Text'Class
     with Storage_Size => 0;

   not overriding function To_Null_Component_Text
    (Self : aliased Null_Component)
      return Null_Component_Text_Access is abstract;

   not overriding function Null_Token
    (Self : Null_Component_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Semicolon_Token
    (Self : Null_Component_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

end Program.Elements.Null_Components;
