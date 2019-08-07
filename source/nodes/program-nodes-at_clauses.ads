--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Lexical_Elements;
with Program.Elements.Identifiers;
with Program.Elements.Expressions;
with Program.Elements.At_Clauses;
with Program.Element_Visitors;

package Program.Nodes.At_Clauses is

   pragma Preelaborate;

   type At_Clause is
     new Program.Nodes.Node and Program.Elements.At_Clauses.At_Clause
         and Program.Elements.At_Clauses.At_Clause_Text
     with private;

   function Create
    (For_Token       : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name            : not null Program.Elements.Identifiers.Identifier_Access;
     Use_Token       : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     At_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Expression      : not null Program.Elements.Expressions.Expression_Access;
     Semicolon_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return At_Clause;

   type Implicit_At_Clause is
     new Program.Nodes.Node and Program.Elements.At_Clauses.At_Clause
     with private;

   function Create
    (Name                 : not null Program.Elements.Identifiers
         .Identifier_Access;
     Expression           : not null Program.Elements.Expressions
         .Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_At_Clause
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_At_Clause is
     abstract new Program.Nodes.Node and Program.Elements.At_Clauses.At_Clause
     with record
        Name       : not null Program.Elements.Identifiers.Identifier_Access;
        Expression : not null Program.Elements.Expressions.Expression_Access;
     end record;

   procedure Initialize (Self : aliased in out Base_At_Clause'Class);

   overriding procedure Visit
    (Self    : not null access Base_At_Clause;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Name
    (Self : Base_At_Clause)
      return not null Program.Elements.Identifiers.Identifier_Access;

   overriding function Expression
    (Self : Base_At_Clause)
      return not null Program.Elements.Expressions.Expression_Access;

   overriding function Is_At_Clause (Self : Base_At_Clause) return Boolean;

   overriding function Is_Representation_Clause
    (Self : Base_At_Clause)
      return Boolean;

   overriding function Is_Clause (Self : Base_At_Clause) return Boolean;

   type At_Clause is
     new Base_At_Clause and Program.Elements.At_Clauses.At_Clause_Text
     with record
        For_Token       : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Use_Token       : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        At_Token        : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Semicolon_Token : not null Program.Lexical_Elements
          .Lexical_Element_Access;
     end record;

   overriding function To_At_Clause_Text
    (Self : aliased in out At_Clause)
      return Program.Elements.At_Clauses.At_Clause_Text_Access;

   overriding function For_Token
    (Self : At_Clause)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Use_Token
    (Self : At_Clause)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function At_Token
    (Self : At_Clause)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Semicolon_Token
    (Self : At_Clause)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   type Implicit_At_Clause is
     new Base_At_Clause
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
     end record;

   overriding function To_At_Clause_Text
    (Self : aliased in out Implicit_At_Clause)
      return Program.Elements.At_Clauses.At_Clause_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_At_Clause)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_At_Clause)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_At_Clause)
      return Boolean;

end Program.Nodes.At_Clauses;
