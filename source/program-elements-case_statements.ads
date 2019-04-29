--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Statements;

package Program.Elements.Case_Statements is

   pragma Pure (Program.Elements.Case_Statements);

   type Case_Statement is
     limited interface and Program.Elements.Statements.Statement;

   type Case_Statement_Access is access all Case_Statement'Class
     with Storage_Size => 0;

end Program.Elements.Case_Statements;
