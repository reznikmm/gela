--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Statements;

package Program.Elements.Accept_Statements is

   pragma Pure (Program.Elements.Accept_Statements);

   type Accept_Statement is
     limited interface and Program.Elements.Statements.Statement;

   type Accept_Statement_Access is access all Accept_Statement'Class
     with Storage_Size => 0;

end Program.Elements.Accept_Statements;
