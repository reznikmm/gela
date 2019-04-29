--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Statements;

package Program.Elements.For_Loop_Statements is

   pragma Pure (Program.Elements.For_Loop_Statements);

   type For_Loop_Statement is
     limited interface and Program.Elements.Statements.Statement;

   type For_Loop_Statement_Access is access all For_Loop_Statement'Class
     with Storage_Size => 0;

end Program.Elements.For_Loop_Statements;
