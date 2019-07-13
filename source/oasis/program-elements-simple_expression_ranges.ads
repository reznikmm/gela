--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Constraints;
with Program.Elements.Discrete_Subtype_Definitions;
with Program.Elements.Discrete_Ranges;
with Program.Elements.Expressions;
with Program.Lexical_Elements;

package Program.Elements.Simple_Expression_Ranges is

   pragma Pure (Program.Elements.Simple_Expression_Ranges);

   type Simple_Expression_Range is
     limited interface and Program.Elements.Constraints.Constraint
       and Program.Elements.Discrete_Subtype_Definitions
         .Discrete_Subtype_Definition
       and Program.Elements.Discrete_Ranges.Discrete_Range;

   type Simple_Expression_Range_Access is
     access all Simple_Expression_Range'Class with Storage_Size => 0;

   not overriding function Lower_Bound
    (Self : Simple_Expression_Range)
      return not null Program.Elements.Expressions.Expression_Access
     is abstract;

   not overriding function Upper_Bound
    (Self : Simple_Expression_Range)
      return not null Program.Elements.Expressions.Expression_Access
     is abstract;

   type Simple_Expression_Range_Text is limited interface;

   type Simple_Expression_Range_Text_Access is
     access all Simple_Expression_Range_Text'Class with Storage_Size => 0;

   not overriding function To_Simple_Expression_Range_Text
    (Self : aliased in out Simple_Expression_Range)
      return Simple_Expression_Range_Text_Access is abstract;

   not overriding function Double_Dot_Token
    (Self : Simple_Expression_Range_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

end Program.Elements.Simple_Expression_Ranges;
