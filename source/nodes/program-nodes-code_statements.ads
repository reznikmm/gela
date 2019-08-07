--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Qualified_Expressions;
with Program.Lexical_Elements;
with Program.Elements.Code_Statements;
with Program.Element_Visitors;

package Program.Nodes.Code_Statements is

   pragma Preelaborate;

   type Code_Statement is
     new Program.Nodes.Node and Program.Elements.Code_Statements.Code_Statement
         and Program.Elements.Code_Statements.Code_Statement_Text
     with private;

   function Create
    (Expression      : not null Program.Elements.Qualified_Expressions
         .Qualified_Expression_Access;
     Semicolon_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Code_Statement;

   type Implicit_Code_Statement is
     new Program.Nodes.Node and Program.Elements.Code_Statements.Code_Statement
     with private;

   function Create
    (Expression           : not null Program.Elements.Qualified_Expressions
         .Qualified_Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Code_Statement
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Code_Statement is
     abstract new Program.Nodes.Node
       and Program.Elements.Code_Statements.Code_Statement
     with record
        Expression : not null Program.Elements.Qualified_Expressions
          .Qualified_Expression_Access;
     end record;

   procedure Initialize (Self : aliased in out Base_Code_Statement'Class);

   overriding procedure Visit
    (Self    : not null access Base_Code_Statement;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Expression
    (Self : Base_Code_Statement)
      return not null Program.Elements.Qualified_Expressions
          .Qualified_Expression_Access;

   overriding function Is_Code_Statement
    (Self : Base_Code_Statement)
      return Boolean;

   overriding function Is_Statement
    (Self : Base_Code_Statement)
      return Boolean;

   type Code_Statement is
     new Base_Code_Statement
       and Program.Elements.Code_Statements.Code_Statement_Text
     with record
        Semicolon_Token : not null Program.Lexical_Elements
          .Lexical_Element_Access;
     end record;

   overriding function To_Code_Statement_Text
    (Self : aliased in out Code_Statement)
      return Program.Elements.Code_Statements.Code_Statement_Text_Access;

   overriding function Semicolon_Token
    (Self : Code_Statement)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   type Implicit_Code_Statement is
     new Base_Code_Statement
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
     end record;

   overriding function To_Code_Statement_Text
    (Self : aliased in out Implicit_Code_Statement)
      return Program.Elements.Code_Statements.Code_Statement_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Code_Statement)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Code_Statement)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Code_Statement)
      return Boolean;

end Program.Nodes.Code_Statements;
