--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Expressions;
with Program.Lexical_Elements;

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

   type Explicit_Dereference_Text is limited interface;

   type Explicit_Dereference_Text_Access is
     access all Explicit_Dereference_Text'Class with Storage_Size => 0;

   not overriding function To_Explicit_Dereference_Text
    (Self : aliased in out Explicit_Dereference)
      return Explicit_Dereference_Text_Access is abstract;

   not overriding function Dot_Token
    (Self : Explicit_Dereference_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function All_Token
    (Self : Explicit_Dereference_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

end Program.Elements.Explicit_Dereferences;
