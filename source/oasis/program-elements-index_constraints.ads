--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Constraints;
with Program.Lexical_Elements;
with Program.Elements.Discrete_Ranges;

package Program.Elements.Index_Constraints is

   pragma Pure (Program.Elements.Index_Constraints);

   type Index_Constraint is
     limited interface and Program.Elements.Constraints.Constraint;

   type Index_Constraint_Access is access all Index_Constraint'Class
     with Storage_Size => 0;

   not overriding function Ranges
    (Self : Index_Constraint)
      return not null Program.Elements.Discrete_Ranges
          .Discrete_Range_Vector_Access is abstract;

   type Index_Constraint_Text is limited interface;

   type Index_Constraint_Text_Access is access all Index_Constraint_Text'Class
     with Storage_Size => 0;

   not overriding function To_Index_Constraint_Text
    (Self : aliased Index_Constraint)
      return Index_Constraint_Text_Access is abstract;

   not overriding function Left_Bracket_Token
    (Self : Index_Constraint_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Right_Bracket_Token
    (Self : Index_Constraint_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

end Program.Elements.Index_Constraints;
