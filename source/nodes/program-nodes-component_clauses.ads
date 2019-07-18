--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Identifiers;
with Program.Lexical_Elements;
with Program.Elements.Expressions;
with Program.Elements.Simple_Expression_Ranges;
with Program.Elements.Component_Clauses;
with Program.Element_Visitors;

package Program.Nodes.Component_Clauses is

   pragma Pure (Program.Nodes.Component_Clauses);

   type Component_Clause is
     new Program.Nodes.Node
         and Program.Elements.Component_Clauses.Component_Clause
         and Program.Elements.Component_Clauses.Component_Clause_Text
     with private;

   function Create
    (Clause_Name     : not null Program.Elements.Identifiers.Identifier_Access;
     At_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Position        : not null Program.Elements.Expressions.Expression_Access;
     Range_Token     : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Clause_Range    : not null Program.Elements.Simple_Expression_Ranges
         .Simple_Expression_Range_Access;
     Semicolon_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Component_Clause;

   type Implicit_Component_Clause is
     new Program.Nodes.Node
         and Program.Elements.Component_Clauses.Component_Clause
     with private;

   function Create
    (Clause_Name          : not null Program.Elements.Identifiers
         .Identifier_Access;
     Position             : not null Program.Elements.Expressions
         .Expression_Access;
     Clause_Range         : not null Program.Elements.Simple_Expression_Ranges
         .Simple_Expression_Range_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Component_Clause
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Component_Clause is
     abstract new Program.Nodes.Node
       and Program.Elements.Component_Clauses.Component_Clause
     with record
        Clause_Name  : not null Program.Elements.Identifiers.Identifier_Access;
        Position     : not null Program.Elements.Expressions.Expression_Access;
        Clause_Range : not null Program.Elements.Simple_Expression_Ranges
          .Simple_Expression_Range_Access;
     end record;

   procedure Initialize (Self : aliased in out Base_Component_Clause'Class);

   overriding procedure Visit
    (Self    : not null access Base_Component_Clause;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Clause_Name
    (Self : Base_Component_Clause)
      return not null Program.Elements.Identifiers.Identifier_Access;

   overriding function Position
    (Self : Base_Component_Clause)
      return not null Program.Elements.Expressions.Expression_Access;

   overriding function Clause_Range
    (Self : Base_Component_Clause)
      return not null Program.Elements.Simple_Expression_Ranges
          .Simple_Expression_Range_Access;

   overriding function Is_Component_Clause
    (Self : Base_Component_Clause)
      return Boolean;

   overriding function Is_Clause (Self : Base_Component_Clause) return Boolean;

   type Component_Clause is
     new Base_Component_Clause
       and Program.Elements.Component_Clauses.Component_Clause_Text
     with record
        At_Token        : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Range_Token     : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Semicolon_Token : not null Program.Lexical_Elements
          .Lexical_Element_Access;
     end record;

   overriding function To_Component_Clause_Text
    (Self : aliased in out Component_Clause)
      return Program.Elements.Component_Clauses.Component_Clause_Text_Access;

   overriding function At_Token
    (Self : Component_Clause)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Range_Token
    (Self : Component_Clause)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Semicolon_Token
    (Self : Component_Clause)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   type Implicit_Component_Clause is
     new Base_Component_Clause
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
     end record;

   overriding function To_Component_Clause_Text
    (Self : aliased in out Implicit_Component_Clause)
      return Program.Elements.Component_Clauses.Component_Clause_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Component_Clause)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Component_Clause)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Component_Clause)
      return Boolean;

end Program.Nodes.Component_Clauses;
