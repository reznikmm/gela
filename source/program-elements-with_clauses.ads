--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Clauses;

package Program.Elements.With_Clauses is

   pragma Pure (Program.Elements.With_Clauses);

   type With_Clause is limited interface and Program.Elements.Clauses.Clause;

   type With_Clause_Access is access all With_Clause'Class
     with Storage_Size => 0;

end Program.Elements.With_Clauses;
