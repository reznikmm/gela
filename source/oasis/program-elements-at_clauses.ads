--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Representation_Clauses;
with Program.Lexical_Elements;
with Program.Elements.Identifiers;
with Program.Elements.Expressions;

package Program.Elements.At_Clauses is

   pragma Pure (Program.Elements.At_Clauses);

   type At_Clause is
     limited interface
       and Program.Elements.Representation_Clauses.Representation_Clause;

   type At_Clause_Access is access all At_Clause'Class with Storage_Size => 0;

   not overriding function Name
    (Self : At_Clause)
      return not null Program.Elements.Identifiers.Identifier_Access
     is abstract;

   not overriding function Expression
    (Self : At_Clause)
      return not null Program.Elements.Expressions.Expression_Access
     is abstract;

   type At_Clause_Text is limited interface;

   type At_Clause_Text_Access is access all At_Clause_Text'Class
     with Storage_Size => 0;

   not overriding function To_At_Clause_Text
    (Self : aliased At_Clause)
      return At_Clause_Text_Access is abstract;

   not overriding function For_Token
    (Self : At_Clause_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Use_Token
    (Self : At_Clause_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function At_Token
    (Self : At_Clause_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Semicolon_Token
    (Self : At_Clause_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

end Program.Elements.At_Clauses;
