--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package Program.Elements.Clauses is

   pragma Pure (Program.Elements.Clauses);

   type Clause is limited interface and Program.Elements.Element;

   type Clause_Access is access all Clause'Class with Storage_Size => 0;

   type Clause_Text is limited interface;

   type Clause_Text_Access is access all Clause_Text'Class
     with Storage_Size => 0;

   not overriding function To_Clause_Text
    (Self : aliased in out Clause)
      return Clause_Text_Access is abstract;

end Program.Elements.Clauses;
