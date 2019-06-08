--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Constraints;
with Program.Tokens;
with Program.Elements.Discriminant_Associations;

package Program.Elements.Discriminant_Constraints is

   pragma Pure (Program.Elements.Discriminant_Constraints);

   type Discriminant_Constraint is
     limited interface and Program.Elements.Constraints.Constraint;

   type Discriminant_Constraint_Access is
     access all Discriminant_Constraint'Class with Storage_Size => 0;

   not overriding function Left_Bracket_Token
    (Self : Discriminant_Constraint)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Discriminants
    (Self : Discriminant_Constraint)
      return not null Program.Elements.Discriminant_Associations
          .Discriminant_Association_Vector_Access is abstract;

   not overriding function Right_Bracket_Token
    (Self : Discriminant_Constraint)
      return Program.Tokens.Token_Access is abstract;

end Program.Elements.Discriminant_Constraints;
