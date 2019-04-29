--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Statements;

package Program.Elements.Delay_Statements is

   pragma Pure (Program.Elements.Delay_Statements);

   type Delay_Statement is
     limited interface and Program.Elements.Statements.Statement;

   type Delay_Statement_Access is access all Delay_Statement'Class
     with Storage_Size => 0;

end Program.Elements.Delay_Statements;
