--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Representation_Clauses;
with Program.Tokens;
with Program.Elements.Expressions;
with Program.Elements.Array_Aggregates;

package Program.Elements.Enumeration_Representation_Clauses is

   pragma Pure (Program.Elements.Enumeration_Representation_Clauses);

   type Enumeration_Representation_Clause is
     limited interface
       and Program.Elements.Representation_Clauses.Representation_Clause;

   type Enumeration_Representation_Clause_Access is
     access all Enumeration_Representation_Clause'Class with Storage_Size => 0;

   not overriding function For_Token
    (Self : Enumeration_Representation_Clause)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Name
    (Self : Enumeration_Representation_Clause)
      return Program.Elements.Expressions.Expression_Access is abstract;

   not overriding function Use_Token
    (Self : Enumeration_Representation_Clause)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Expression
    (Self : Enumeration_Representation_Clause)
      return Program.Elements.Array_Aggregates.Array_Aggregate_Access
     is abstract;

   not overriding function Semicolon_Token
    (Self : Enumeration_Representation_Clause)
      return Program.Tokens.Token_Access is abstract;

end Program.Elements.Enumeration_Representation_Clauses;
