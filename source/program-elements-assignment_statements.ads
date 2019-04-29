--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Statements;

package Program.Elements.Assignment_Statements is

   pragma Pure (Program.Elements.Assignment_Statements);

   type Assignment_Statement is
     limited interface and Program.Elements.Statements.Statement;

   type Assignment_Statement_Access is access all Assignment_Statement'Class
     with Storage_Size => 0;

end Program.Elements.Assignment_Statements;
