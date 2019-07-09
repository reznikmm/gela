--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Constraints;
with Program.Lexical_Elements;
with Program.Elements.Expressions;

package Program.Elements.Delta_Constraints is

   pragma Pure (Program.Elements.Delta_Constraints);

   type Delta_Constraint is
     limited interface and Program.Elements.Constraints.Constraint;

   type Delta_Constraint_Access is access all Delta_Constraint'Class
     with Storage_Size => 0;

   not overriding function Delta_Expression
    (Self : Delta_Constraint)
      return not null Program.Elements.Expressions.Expression_Access
     is abstract;

   not overriding function Real_Range_Constraint
    (Self : Delta_Constraint)
      return Program.Elements.Constraints.Constraint_Access is abstract;

   type Delta_Constraint_Text is limited interface;

   type Delta_Constraint_Text_Access is access all Delta_Constraint_Text'Class
     with Storage_Size => 0;

   not overriding function To_Delta_Constraint_Text
    (Self : aliased Delta_Constraint)
      return Delta_Constraint_Text_Access is abstract;

   not overriding function Delta_Token
    (Self : Delta_Constraint_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Range_Token
    (Self : Delta_Constraint_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

end Program.Elements.Delta_Constraints;
