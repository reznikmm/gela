--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Representation_Clauses;

package Program.Elements.Record_Representation_Clauses is

   pragma Pure (Program.Elements.Record_Representation_Clauses);

   type Record_Representation_Clause is
     limited interface
       and Program.Elements.Representation_Clauses.Representation_Clause;

   type Record_Representation_Clause_Access is
     access all Record_Representation_Clause'Class with Storage_Size => 0;

end Program.Elements.Record_Representation_Clauses;
