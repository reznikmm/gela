--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Clauses;

package Program.Elements.Component_Clauses is

   pragma Pure (Program.Elements.Component_Clauses);

   type Component_Clause is
     limited interface and Program.Elements.Clauses.Clause;

   type Component_Clause_Access is access all Component_Clause'Class
     with Storage_Size => 0;

end Program.Elements.Component_Clauses;
