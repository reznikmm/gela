--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Statements;

package Program.Elements.Loop_Statements is

   pragma Pure (Program.Elements.Loop_Statements);

   type Loop_Statement is
     limited interface and Program.Elements.Statements.Statement;

   type Loop_Statement_Access is access all Loop_Statement'Class
     with Storage_Size => 0;

end Program.Elements.Loop_Statements;
