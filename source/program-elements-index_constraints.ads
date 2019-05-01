--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Constraints;
with Program.Tokens;

package Program.Elements.Index_Constraints is

   pragma Pure (Program.Elements.Index_Constraints);

   type Index_Constraint is
     limited interface and Program.Elements.Constraints.Constraint;

   type Index_Constraint_Access is access all Index_Constraint'Class
     with Storage_Size => 0;

   not overriding function Left_Bracket_Token
    (Self : Index_Constraint)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Right_Bracket_Token
    (Self : Index_Constraint)
      return Program.Tokens.Token_Access is abstract;

end Program.Elements.Index_Constraints;
