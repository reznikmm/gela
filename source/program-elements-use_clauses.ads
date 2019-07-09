--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Clauses;
with Program.Lexical_Elements;
with Program.Elements.Expressions;

package Program.Elements.Use_Clauses is

   pragma Pure (Program.Elements.Use_Clauses);

   type Use_Clause is limited interface and Program.Elements.Clauses.Clause;

   type Use_Clause_Access is access all Use_Clause'Class
     with Storage_Size => 0;

   not overriding function Clause_Names
    (Self : Use_Clause)
      return not null Program.Elements.Expressions.Expression_Vector_Access
     is abstract;

   type Use_Clause_Text is limited interface;

   type Use_Clause_Text_Access is access all Use_Clause_Text'Class
     with Storage_Size => 0;

   not overriding function To_Use_Clause_Text
    (Self : aliased Use_Clause)
      return Use_Clause_Text_Access is abstract;

   not overriding function Use_Token
    (Self : Use_Clause_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function All_Token
    (Self : Use_Clause_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Type_Token
    (Self : Use_Clause_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Semicolon_Token
    (Self : Use_Clause_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

end Program.Elements.Use_Clauses;
