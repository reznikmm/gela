--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Element_Vectors;
with Program.Elements.Clauses;
with Program.Elements.Identifiers;
with Program.Lexical_Elements;
with Program.Elements.Expressions;
with Program.Elements.Simple_Expression_Ranges;

package Program.Elements.Component_Clauses is

   pragma Pure (Program.Elements.Component_Clauses);

   type Component_Clause is
     limited interface and Program.Elements.Clauses.Clause;

   type Component_Clause_Access is access all Component_Clause'Class
     with Storage_Size => 0;

   not overriding function Clause_Name
    (Self : Component_Clause)
      return not null Program.Elements.Identifiers.Identifier_Access
     is abstract;

   not overriding function Position
    (Self : Component_Clause)
      return not null Program.Elements.Expressions.Expression_Access
     is abstract;

   not overriding function Clause_Range
    (Self : Component_Clause)
      return not null Program.Elements.Simple_Expression_Ranges
          .Simple_Expression_Range_Access is abstract;

   type Component_Clause_Text is limited interface;

   type Component_Clause_Text_Access is access all Component_Clause_Text'Class
     with Storage_Size => 0;

   not overriding function To_Component_Clause_Text
    (Self : aliased Component_Clause)
      return Component_Clause_Text_Access is abstract;

   not overriding function At_Token
    (Self : Component_Clause_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Range_Token
    (Self : Component_Clause_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Semicolon_Token
    (Self : Component_Clause_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   type Component_Clause_Vector is
     limited interface and Program.Element_Vectors.Element_Vector;

   type Component_Clause_Vector_Access is
     access all Component_Clause_Vector'Class with Storage_Size => 0;

   overriding function Element
    (Self  : Component_Clause_Vector;
     Index : Positive)
      return not null Program.Elements.Element_Access is abstract
     with Post'Class => Element'Result.Is_Component_Clause;

   function To_Component_Clause
    (Self  : Component_Clause_Vector'Class;
     Index : Positive)
      return not null Component_Clause_Access
     is (Self.Element (Index).To_Component_Clause);

end Program.Elements.Component_Clauses;
