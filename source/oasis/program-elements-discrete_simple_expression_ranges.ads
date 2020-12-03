--  SPDX-FileCopyrightText: 2019 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Program.Elements.Discrete_Ranges;
with Program.Elements.Expressions;
with Program.Lexical_Elements;

package Program.Elements.Discrete_Simple_Expression_Ranges is

   pragma Pure (Program.Elements.Discrete_Simple_Expression_Ranges);

   type Discrete_Simple_Expression_Range is
     limited interface and Program.Elements.Discrete_Ranges.Discrete_Range;

   type Discrete_Simple_Expression_Range_Access is
     access all Discrete_Simple_Expression_Range'Class with Storage_Size => 0;

   not overriding function Lower_Bound
    (Self : Discrete_Simple_Expression_Range)
      return not null Program.Elements.Expressions.Expression_Access
     is abstract;

   not overriding function Upper_Bound
    (Self : Discrete_Simple_Expression_Range)
      return not null Program.Elements.Expressions.Expression_Access
     is abstract;

   type Discrete_Simple_Expression_Range_Text is limited interface;

   type Discrete_Simple_Expression_Range_Text_Access is
     access all Discrete_Simple_Expression_Range_Text'Class
     with Storage_Size => 0;

   not overriding function To_Discrete_Simple_Expression_Range_Text
    (Self : aliased in out Discrete_Simple_Expression_Range)
      return Discrete_Simple_Expression_Range_Text_Access is abstract;

   not overriding function Double_Dot_Token
    (Self : Discrete_Simple_Expression_Range_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

end Program.Elements.Discrete_Simple_Expression_Ranges;
