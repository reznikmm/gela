--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Lexical_Elements;
with Program.Elements.Expressions;
with Program.Elements.Use_Clauses;
with Program.Element_Visitors;

package Program.Nodes.Use_Clauses is

   pragma Pure (Program.Nodes.Use_Clauses);

   type Use_Clause is
     new Program.Nodes.Node and Program.Elements.Use_Clauses.Use_Clause
         and Program.Elements.Use_Clauses.Use_Clause_Text
     with private;

   function Create
    (Use_Token       : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     All_Token       : Program.Lexical_Elements.Lexical_Element_Access;
     Type_Token      : Program.Lexical_Elements.Lexical_Element_Access;
     Clause_Names    : not null Program.Elements.Expressions
         .Expression_Vector_Access;
     Semicolon_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Use_Clause;

   type Implicit_Use_Clause is
     new Program.Nodes.Node and Program.Elements.Use_Clauses.Use_Clause
     with private;

   function Create
    (Clause_Names         : not null Program.Elements.Expressions
         .Expression_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False;
     Has_All              : Boolean := False;
     Has_Type             : Boolean := False)
      return Implicit_Use_Clause
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Use_Clause is
     abstract new Program.Nodes.Node
       and Program.Elements.Use_Clauses.Use_Clause
     with record
        Clause_Names : not null Program.Elements.Expressions
          .Expression_Vector_Access;
     end record;

   procedure Initialize (Self : aliased in out Base_Use_Clause'Class);

   overriding procedure Visit
    (Self    : not null access Base_Use_Clause;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Clause_Names
    (Self : Base_Use_Clause)
      return not null Program.Elements.Expressions.Expression_Vector_Access;

   overriding function Is_Use_Clause (Self : Base_Use_Clause) return Boolean;

   overriding function Is_Clause (Self : Base_Use_Clause) return Boolean;

   type Use_Clause is
     new Base_Use_Clause and Program.Elements.Use_Clauses.Use_Clause_Text
     with record
        Use_Token       : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        All_Token       : Program.Lexical_Elements.Lexical_Element_Access;
        Type_Token      : Program.Lexical_Elements.Lexical_Element_Access;
        Semicolon_Token : not null Program.Lexical_Elements
          .Lexical_Element_Access;
     end record;

   overriding function To_Use_Clause_Text
    (Self : aliased in out Use_Clause)
      return Program.Elements.Use_Clauses.Use_Clause_Text_Access;

   overriding function Use_Token
    (Self : Use_Clause)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function All_Token
    (Self : Use_Clause)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Type_Token
    (Self : Use_Clause)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Semicolon_Token
    (Self : Use_Clause)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Has_All (Self : Use_Clause) return Boolean;

   overriding function Has_Type (Self : Use_Clause) return Boolean;

   type Implicit_Use_Clause is
     new Base_Use_Clause
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
        Has_All              : Boolean;
        Has_Type             : Boolean;
     end record;

   overriding function To_Use_Clause_Text
    (Self : aliased in out Implicit_Use_Clause)
      return Program.Elements.Use_Clauses.Use_Clause_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Use_Clause)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Use_Clause)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Use_Clause)
      return Boolean;

   overriding function Has_All (Self : Implicit_Use_Clause) return Boolean;

   overriding function Has_Type (Self : Implicit_Use_Clause) return Boolean;

end Program.Nodes.Use_Clauses;
