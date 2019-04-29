--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Statements;

package Program.Elements.If_Statements is

   pragma Pure (Program.Elements.If_Statements);

   type If_Statement is
     limited interface and Program.Elements.Statements.Statement;

   type If_Statement_Access is access all If_Statement'Class
     with Storage_Size => 0;

end Program.Elements.If_Statements;
