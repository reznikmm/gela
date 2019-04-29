--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Representation_Clauses;

package Program.Elements.At_Clauses is

   pragma Pure (Program.Elements.At_Clauses);

   type At_Clause is
     limited interface
       and Program.Elements.Representation_Clauses.Representation_Clause;

   type At_Clause_Access is access all At_Clause'Class with Storage_Size => 0;

end Program.Elements.At_Clauses;
