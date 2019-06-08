--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Clauses;
with Program.Tokens;
with Program.Elements.Expressions;

package Program.Elements.With_Clauses is

   pragma Pure (Program.Elements.With_Clauses);

   type With_Clause is limited interface and Program.Elements.Clauses.Clause;

   type With_Clause_Access is access all With_Clause'Class
     with Storage_Size => 0;

   not overriding function Limited_Token
    (Self : With_Clause)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Private_Token
    (Self : With_Clause)
      return Program.Tokens.Token_Access is abstract;

   not overriding function With_Token
    (Self : With_Clause)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Clause_Names
    (Self : With_Clause)
      return not null Program.Elements.Expressions.Expression_Vector_Access
     is abstract;

   not overriding function Semicolon_Token
    (Self : With_Clause)
      return Program.Tokens.Token_Access is abstract;

end Program.Elements.With_Clauses;
