--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Expressions;
with Program.Lexical_Elements;

package Program.Elements.Qualified_Expressions is

   pragma Pure (Program.Elements.Qualified_Expressions);

   type Qualified_Expression is
     limited interface and Program.Elements.Expressions.Expression;

   type Qualified_Expression_Access is access all Qualified_Expression'Class
     with Storage_Size => 0;

   not overriding function Subtype_Mark
    (Self : Qualified_Expression)
      return not null Program.Elements.Expressions.Expression_Access
     is abstract;

   not overriding function Operand
    (Self : Qualified_Expression)
      return not null Program.Elements.Expressions.Expression_Access
     is abstract;

   type Qualified_Expression_Text is limited interface;

   type Qualified_Expression_Text_Access is
     access all Qualified_Expression_Text'Class with Storage_Size => 0;

   not overriding function To_Qualified_Expression_Text
    (Self : aliased Qualified_Expression)
      return Qualified_Expression_Text_Access is abstract;

   not overriding function Apostrophe_Token
    (Self : Qualified_Expression_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Left_Bracket_Token
    (Self : Qualified_Expression_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Right_Bracket_Token
    (Self : Qualified_Expression_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

end Program.Elements.Qualified_Expressions;
