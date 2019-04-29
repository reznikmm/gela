--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Statements;

package Program.Elements.Call_Statements is

   pragma Pure (Program.Elements.Call_Statements);

   type Call_Statement is
     limited interface and Program.Elements.Statements.Statement;

   type Call_Statement_Access is access all Call_Statement'Class
     with Storage_Size => 0;

end Program.Elements.Call_Statements;
