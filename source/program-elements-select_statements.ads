--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Statements;

package Program.Elements.Select_Statements is

   pragma Pure (Program.Elements.Select_Statements);

   type Select_Statement is
     limited interface and Program.Elements.Statements.Statement;

   type Select_Statement_Access is access all Select_Statement'Class
     with Storage_Size => 0;

end Program.Elements.Select_Statements;
