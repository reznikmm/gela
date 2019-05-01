--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Associations;
with Program.Elements.Expressions;
with Program.Tokens;

package Program.Elements.Parameter_Associations is

   pragma Pure (Program.Elements.Parameter_Associations);

   type Parameter_Association is
     limited interface and Program.Elements.Associations.Association;

   type Parameter_Association_Access is access all Parameter_Association'Class
     with Storage_Size => 0;

   not overriding function Formal_Parameter
    (Self : Parameter_Association)
      return Program.Elements.Expressions.Expression_Access is abstract;

   not overriding function Arrow_Token
    (Self : Parameter_Association)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Actual_Parameter
    (Self : Parameter_Association)
      return Program.Elements.Expressions.Expression_Access is abstract;

end Program.Elements.Parameter_Associations;
