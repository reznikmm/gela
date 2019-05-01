--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Representation_Clauses;
with Program.Tokens;
with Program.Elements.Expressions;

package Program.Elements.Attribute_Definition_Clauses is

   pragma Pure (Program.Elements.Attribute_Definition_Clauses);

   type Attribute_Definition_Clause is
     limited interface
       and Program.Elements.Representation_Clauses.Representation_Clause;

   type Attribute_Definition_Clause_Access is
     access all Attribute_Definition_Clause'Class with Storage_Size => 0;

   not overriding function For_Token
    (Self : Attribute_Definition_Clause)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Name
    (Self : Attribute_Definition_Clause)
      return Program.Elements.Expressions.Expression_Access is abstract;

   not overriding function Use_Token
    (Self : Attribute_Definition_Clause)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Expression
    (Self : Attribute_Definition_Clause)
      return Program.Elements.Expressions.Expression_Access is abstract;

   not overriding function Semicolon_Token
    (Self : Attribute_Definition_Clause)
      return Program.Tokens.Token_Access is abstract;

end Program.Elements.Attribute_Definition_Clauses;
