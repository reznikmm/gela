--  SPDX-FileCopyrightText: 2019 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Program.Elements.Clauses;

package Program.Elements.Representation_Clauses is

   pragma Pure (Program.Elements.Representation_Clauses);

   type Representation_Clause is
     limited interface and Program.Elements.Clauses.Clause;

   type Representation_Clause_Access is access all Representation_Clause'Class
     with Storage_Size => 0;

end Program.Elements.Representation_Clauses;
