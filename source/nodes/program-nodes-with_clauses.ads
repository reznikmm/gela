--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Lexical_Elements;
with Program.Elements.Expressions;
with Program.Elements.With_Clauses;
with Program.Element_Visitors;

package Program.Nodes.With_Clauses is

   pragma Preelaborate;

   type With_Clause is
     new Program.Nodes.Node and Program.Elements.With_Clauses.With_Clause
         and Program.Elements.With_Clauses.With_Clause_Text
     with private;

   function Create
    (Limited_Token   : Program.Lexical_Elements.Lexical_Element_Access;
     Private_Token   : Program.Lexical_Elements.Lexical_Element_Access;
     With_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Clause_Names    : not null Program.Elements.Expressions
         .Expression_Vector_Access;
     Semicolon_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return With_Clause;

   type Implicit_With_Clause is
     new Program.Nodes.Node and Program.Elements.With_Clauses.With_Clause
     with private;

   function Create
    (Clause_Names         : not null Program.Elements.Expressions
         .Expression_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False;
     Has_Limited          : Boolean := False;
     Has_Private          : Boolean := False)
      return Implicit_With_Clause
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_With_Clause is
     abstract new Program.Nodes.Node
       and Program.Elements.With_Clauses.With_Clause
     with record
        Clause_Names : not null Program.Elements.Expressions
          .Expression_Vector_Access;
     end record;

   procedure Initialize (Self : aliased in out Base_With_Clause'Class);

   overriding procedure Visit
    (Self    : not null access Base_With_Clause;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Clause_Names
    (Self : Base_With_Clause)
      return not null Program.Elements.Expressions.Expression_Vector_Access;

   overriding function Is_With_Clause (Self : Base_With_Clause) return Boolean;

   overriding function Is_Clause (Self : Base_With_Clause) return Boolean;

   type With_Clause is
     new Base_With_Clause and Program.Elements.With_Clauses.With_Clause_Text
     with record
        Limited_Token   : Program.Lexical_Elements.Lexical_Element_Access;
        Private_Token   : Program.Lexical_Elements.Lexical_Element_Access;
        With_Token      : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Semicolon_Token : not null Program.Lexical_Elements
          .Lexical_Element_Access;
     end record;

   overriding function To_With_Clause_Text
    (Self : aliased in out With_Clause)
      return Program.Elements.With_Clauses.With_Clause_Text_Access;

   overriding function Limited_Token
    (Self : With_Clause)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Private_Token
    (Self : With_Clause)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function With_Token
    (Self : With_Clause)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Semicolon_Token
    (Self : With_Clause)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Has_Limited (Self : With_Clause) return Boolean;

   overriding function Has_Private (Self : With_Clause) return Boolean;

   type Implicit_With_Clause is
     new Base_With_Clause
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
        Has_Limited          : Boolean;
        Has_Private          : Boolean;
     end record;

   overriding function To_With_Clause_Text
    (Self : aliased in out Implicit_With_Clause)
      return Program.Elements.With_Clauses.With_Clause_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_With_Clause)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_With_Clause)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_With_Clause)
      return Boolean;

   overriding function Has_Limited
    (Self : Implicit_With_Clause)
      return Boolean;

   overriding function Has_Private
    (Self : Implicit_With_Clause)
      return Boolean;

end Program.Nodes.With_Clauses;
