--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Expressions;

package Program.Elements.Membership_Tests is

   pragma Pure (Program.Elements.Membership_Tests);

   type Membership_Test is
     limited interface and Program.Elements.Expressions.Expression;

   type Membership_Test_Access is access all Membership_Test'Class
     with Storage_Size => 0;

end Program.Elements.Membership_Tests;
