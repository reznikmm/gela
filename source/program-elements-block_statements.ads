--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Statements;

package Program.Elements.Block_Statements is

   pragma Pure (Program.Elements.Block_Statements);

   type Block_Statement is
     limited interface and Program.Elements.Statements.Statement;

   type Block_Statement_Access is access all Block_Statement'Class
     with Storage_Size => 0;

end Program.Elements.Block_Statements;
