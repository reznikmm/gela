--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Statements;

package Program.Elements.Goto_Statements is

   pragma Pure (Program.Elements.Goto_Statements);

   type Goto_Statement is
     limited interface and Program.Elements.Statements.Statement;

   type Goto_Statement_Access is access all Goto_Statement'Class
     with Storage_Size => 0;

end Program.Elements.Goto_Statements;
