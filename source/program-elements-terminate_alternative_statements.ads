--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Statements;

package Program.Elements.Terminate_Alternative_Statements is

   pragma Pure (Program.Elements.Terminate_Alternative_Statements);

   type Terminate_Alternative_Statement is
     limited interface and Program.Elements.Statements.Statement;

   type Terminate_Alternative_Statement_Access is
     access all Terminate_Alternative_Statement'Class with Storage_Size => 0;

end Program.Elements.Terminate_Alternative_Statements;
