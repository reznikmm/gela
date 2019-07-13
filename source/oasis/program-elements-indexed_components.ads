--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Expressions;
with Program.Lexical_Elements;

package Program.Elements.Indexed_Components is

   pragma Pure (Program.Elements.Indexed_Components);

   type Indexed_Component is
     limited interface and Program.Elements.Expressions.Expression;

   type Indexed_Component_Access is access all Indexed_Component'Class
     with Storage_Size => 0;

   not overriding function Prefix
    (Self : Indexed_Component)
      return not null Program.Elements.Expressions.Expression_Access
     is abstract;

   not overriding function Expressions
    (Self : Indexed_Component)
      return not null Program.Elements.Expressions.Expression_Vector_Access
     is abstract;

   type Indexed_Component_Text is limited interface;

   type Indexed_Component_Text_Access is
     access all Indexed_Component_Text'Class with Storage_Size => 0;

   not overriding function To_Indexed_Component_Text
    (Self : aliased in out Indexed_Component)
      return Indexed_Component_Text_Access is abstract;

   not overriding function Left_Bracket_Token
    (Self : Indexed_Component_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Right_Bracket_Token
    (Self : Indexed_Component_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

end Program.Elements.Indexed_Components;
