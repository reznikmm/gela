--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Statements;

package Program.Elements.Exit_Statements is

   pragma Pure (Program.Elements.Exit_Statements);

   type Exit_Statement is
     limited interface and Program.Elements.Statements.Statement;

   type Exit_Statement_Access is access all Exit_Statement'Class
     with Storage_Size => 0;

end Program.Elements.Exit_Statements;
