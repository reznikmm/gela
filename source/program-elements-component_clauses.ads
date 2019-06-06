--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Clauses;
with Program.Elements.Identifiers;
with Program.Tokens;
with Program.Elements.Expressions;
with Program.Elements.Simple_Expression_Ranges;
with Program.Element_Vectors;

package Program.Elements.Component_Clauses is

   pragma Pure (Program.Elements.Component_Clauses);

   type Component_Clause is
     limited interface and Program.Elements.Clauses.Clause;

   type Component_Clause_Access is access all Component_Clause'Class
     with Storage_Size => 0;

   not overriding function Clause_Name
    (Self : Component_Clause)
      return Program.Elements.Identifiers.Identifier_Access is abstract;

   not overriding function At_Token
    (Self : Component_Clause)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Position
    (Self : Component_Clause)
      return Program.Elements.Expressions.Expression_Access is abstract;

   not overriding function Range_Token
    (Self : Component_Clause)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Clause_Range
    (Self : Component_Clause)
      return Program.Elements.Simple_Expression_Ranges
          .Simple_Expression_Range_Access is abstract;

   not overriding function Semicolon_Token
    (Self : Component_Clause)
      return Program.Tokens.Token_Access is abstract;

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
