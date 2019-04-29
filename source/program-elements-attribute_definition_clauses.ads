--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Representation_Clauses;

package Program.Elements.Attribute_Definition_Clauses is

   pragma Pure (Program.Elements.Attribute_Definition_Clauses);

   type Attribute_Definition_Clause is
     limited interface
       and Program.Elements.Representation_Clauses.Representation_Clause;

   type Attribute_Definition_Clause_Access is
     access all Attribute_Definition_Clause'Class with Storage_Size => 0;

end Program.Elements.Attribute_Definition_Clauses;
