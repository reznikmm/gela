--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Associations;
with Program.Elements.Expressions;
with Program.Tokens;

package Program.Elements.Formal_Package_Associations is

   pragma Pure (Program.Elements.Formal_Package_Associations);

   type Formal_Package_Association is
     limited interface and Program.Elements.Associations.Association;

   type Formal_Package_Association_Access is
     access all Formal_Package_Association'Class with Storage_Size => 0;

   not overriding function Formal_Parameter
    (Self : Formal_Package_Association)
      return Program.Elements.Expressions.Expression_Access is abstract;

   not overriding function Arrow_Token
    (Self : Formal_Package_Association)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Actual_Parameter
    (Self : Formal_Package_Association)
      return Program.Elements.Expressions.Expression_Access is abstract;

   not overriding function Box_Token
    (Self : Formal_Package_Association)
      return Program.Tokens.Token_Access is abstract;

end Program.Elements.Formal_Package_Associations;
