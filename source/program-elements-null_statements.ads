--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Statements;

package Program.Elements.Null_Statements is

   pragma Pure (Program.Elements.Null_Statements);

   type Null_Statement is
     limited interface and Program.Elements.Statements.Statement;

   type Null_Statement_Access is access all Null_Statement'Class
     with Storage_Size => 0;

end Program.Elements.Null_Statements;
