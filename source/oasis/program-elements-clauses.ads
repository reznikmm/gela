--  SPDX-FileCopyrightText: 2019 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

package Program.Elements.Clauses is

   pragma Pure (Program.Elements.Clauses);

   type Clause is limited interface and Program.Elements.Element;

   type Clause_Access is access all Clause'Class with Storage_Size => 0;

end Program.Elements.Clauses;
