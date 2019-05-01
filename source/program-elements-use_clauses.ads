--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Clauses;
with Program.Tokens;

package Program.Elements.Use_Clauses is

   pragma Pure (Program.Elements.Use_Clauses);

   type Use_Clause is limited interface and Program.Elements.Clauses.Clause;

   type Use_Clause_Access is access all Use_Clause'Class
     with Storage_Size => 0;

   not overriding function Use_Token
    (Self : Use_Clause)
      return Program.Tokens.Token_Access is abstract;

   not overriding function All_Token
    (Self : Use_Clause)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Type_Token
    (Self : Use_Clause)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Semicolon_Token
    (Self : Use_Clause)
      return Program.Tokens.Token_Access is abstract;

end Program.Elements.Use_Clauses;
