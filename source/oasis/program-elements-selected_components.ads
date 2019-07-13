--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Expressions;
with Program.Lexical_Elements;

package Program.Elements.Selected_Components is

   pragma Pure (Program.Elements.Selected_Components);

   type Selected_Component is
     limited interface and Program.Elements.Expressions.Expression;

   type Selected_Component_Access is access all Selected_Component'Class
     with Storage_Size => 0;

   not overriding function Prefix
    (Self : Selected_Component)
      return not null Program.Elements.Expressions.Expression_Access
     is abstract;

   not overriding function Selector
    (Self : Selected_Component)
      return not null Program.Elements.Expressions.Expression_Access
     is abstract;

   type Selected_Component_Text is limited interface;

   type Selected_Component_Text_Access is
     access all Selected_Component_Text'Class with Storage_Size => 0;

   not overriding function To_Selected_Component_Text
    (Self : aliased Selected_Component)
      return Selected_Component_Text_Access is abstract;

   not overriding function Dot_Token
    (Self : Selected_Component_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

end Program.Elements.Selected_Components;
