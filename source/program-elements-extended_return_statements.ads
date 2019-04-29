--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Statements;

package Program.Elements.Extended_Return_Statements is

   pragma Pure (Program.Elements.Extended_Return_Statements);

   type Extended_Return_Statement is
     limited interface and Program.Elements.Statements.Statement;

   type Extended_Return_Statement_Access is
     access all Extended_Return_Statement'Class with Storage_Size => 0;

end Program.Elements.Extended_Return_Statements;