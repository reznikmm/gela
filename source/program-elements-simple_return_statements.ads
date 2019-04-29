--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Statements;

package Program.Elements.Simple_Return_Statements is

   pragma Pure (Program.Elements.Simple_Return_Statements);

   type Simple_Return_Statement is
     limited interface and Program.Elements.Statements.Statement;

   type Simple_Return_Statement_Access is
     access all Simple_Return_Statement'Class with Storage_Size => 0;

end Program.Elements.Simple_Return_Statements;
