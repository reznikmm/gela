--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Statements;

package Program.Elements.Abort_Statements is

   pragma Pure (Program.Elements.Abort_Statements);

   type Abort_Statement is
     limited interface and Program.Elements.Statements.Statement;

   type Abort_Statement_Access is access all Abort_Statement'Class
     with Storage_Size => 0;

end Program.Elements.Abort_Statements;
