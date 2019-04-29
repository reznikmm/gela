--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Statements;

package Program.Elements.Code_Statements is

   pragma Pure (Program.Elements.Code_Statements);

   type Code_Statement is
     limited interface and Program.Elements.Statements.Statement;

   type Code_Statement_Access is access all Code_Statement'Class
     with Storage_Size => 0;

end Program.Elements.Code_Statements;
