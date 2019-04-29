--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Statements;

package Program.Elements.Raise_Statements is

   pragma Pure (Program.Elements.Raise_Statements);

   type Raise_Statement is
     limited interface and Program.Elements.Statements.Statement;

   type Raise_Statement_Access is access all Raise_Statement'Class
     with Storage_Size => 0;

end Program.Elements.Raise_Statements;
