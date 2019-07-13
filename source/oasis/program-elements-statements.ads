--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package Program.Elements.Statements is

   pragma Pure (Program.Elements.Statements);

   type Statement is limited interface and Program.Elements.Element;

   type Statement_Access is access all Statement'Class with Storage_Size => 0;

end Program.Elements.Statements;
