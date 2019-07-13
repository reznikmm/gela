--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Constraints;
with Program.Lexical_Elements;
with Program.Elements.Expressions;

package Program.Elements.Digits_Constraints is

   pragma Pure (Program.Elements.Digits_Constraints);

   type Digits_Constraint is
     limited interface and Program.Elements.Constraints.Constraint;

   type Digits_Constraint_Access is access all Digits_Constraint'Class
     with Storage_Size => 0;

   not overriding function Digits_Expression
    (Self : Digits_Constraint)
      return not null Program.Elements.Expressions.Expression_Access
     is abstract;

   not overriding function Real_Range_Constraint
    (Self : Digits_Constraint)
      return Program.Elements.Constraints.Constraint_Access is abstract;

   type Digits_Constraint_Text is limited interface;

   type Digits_Constraint_Text_Access is
     access all Digits_Constraint_Text'Class with Storage_Size => 0;

   not overriding function To_Digits_Constraint_Text
    (Self : aliased in out Digits_Constraint)
      return Digits_Constraint_Text_Access is abstract;

   not overriding function Digits_Token
    (Self : Digits_Constraint_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Range_Token
    (Self : Digits_Constraint_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

end Program.Elements.Digits_Constraints;
