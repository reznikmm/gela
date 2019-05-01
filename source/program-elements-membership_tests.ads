--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Expressions;
with Program.Tokens;

package Program.Elements.Membership_Tests is

   pragma Pure (Program.Elements.Membership_Tests);

   type Membership_Test is
     limited interface and Program.Elements.Expressions.Expression;

   type Membership_Test_Access is access all Membership_Test'Class
     with Storage_Size => 0;

   not overriding function Membership_Test_Expression
    (Self : Membership_Test)
      return Program.Elements.Expressions.Expression_Access is abstract;

   not overriding function Not_Token
    (Self : Membership_Test)
      return Program.Tokens.Token_Access is abstract;

   not overriding function In_Token
    (Self : Membership_Test)
      return Program.Tokens.Token_Access is abstract;

end Program.Elements.Membership_Tests;
