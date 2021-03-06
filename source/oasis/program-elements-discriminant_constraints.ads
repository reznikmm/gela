--  SPDX-FileCopyrightText: 2019 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Program.Elements.Constraints;
with Program.Lexical_Elements;
with Program.Elements.Discriminant_Associations;

package Program.Elements.Discriminant_Constraints is

   pragma Pure (Program.Elements.Discriminant_Constraints);

   type Discriminant_Constraint is
     limited interface and Program.Elements.Constraints.Constraint;

   type Discriminant_Constraint_Access is
     access all Discriminant_Constraint'Class with Storage_Size => 0;

   not overriding function Discriminants
    (Self : Discriminant_Constraint)
      return not null Program.Elements.Discriminant_Associations
          .Discriminant_Association_Vector_Access is abstract;

   type Discriminant_Constraint_Text is limited interface;

   type Discriminant_Constraint_Text_Access is
     access all Discriminant_Constraint_Text'Class with Storage_Size => 0;

   not overriding function To_Discriminant_Constraint_Text
    (Self : in out Discriminant_Constraint)
      return Discriminant_Constraint_Text_Access is abstract;

   not overriding function Left_Bracket_Token
    (Self : Discriminant_Constraint_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Right_Bracket_Token
    (Self : Discriminant_Constraint_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

end Program.Elements.Discriminant_Constraints;
