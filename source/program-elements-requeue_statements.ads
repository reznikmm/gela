--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Statements;

package Program.Elements.Requeue_Statements is

   pragma Pure (Program.Elements.Requeue_Statements);

   type Requeue_Statement is
     limited interface and Program.Elements.Statements.Statement;

   type Requeue_Statement_Access is access all Requeue_Statement'Class
     with Storage_Size => 0;

end Program.Elements.Requeue_Statements;
