--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Associations;
with Program.Tokens;
with Program.Elements.Expressions;

package Program.Elements.Discriminant_Associations is

   pragma Pure (Program.Elements.Discriminant_Associations);

   type Discriminant_Association is
     limited interface and Program.Elements.Associations.Association;

   type Discriminant_Association_Access is
     access all Discriminant_Association'Class with Storage_Size => 0;

   not overriding function Arrow_Token
    (Self : Discriminant_Association)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Discriminant_Expression
    (Self : Discriminant_Association)
      return Program.Elements.Expressions.Expression_Access is abstract;

end Program.Elements.Discriminant_Associations;
