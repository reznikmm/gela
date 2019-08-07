--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Expressions;
with Program.Lexical_Elements;
with Program.Elements.Assignment_Statements;
with Program.Element_Visitors;

package Program.Nodes.Assignment_Statements is

   pragma Preelaborate;

   type Assignment_Statement is
     new Program.Nodes.Node
         and Program.Elements.Assignment_Statements.Assignment_Statement
         and Program.Elements.Assignment_Statements.Assignment_Statement_Text
     with private;

   function Create
    (Variable_Name    : not null Program.Elements.Expressions
         .Expression_Access;
     Assignment_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Expression       : not null Program.Elements.Expressions
         .Expression_Access;
     Semicolon_Token  : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Assignment_Statement;

   type Implicit_Assignment_Statement is
     new Program.Nodes.Node
         and Program.Elements.Assignment_Statements.Assignment_Statement
     with private;

   function Create
    (Variable_Name        : not null Program.Elements.Expressions
         .Expression_Access;
     Expression           : not null Program.Elements.Expressions
         .Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Assignment_Statement
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Assignment_Statement is
     abstract new Program.Nodes.Node
       and Program.Elements.Assignment_Statements.Assignment_Statement
     with record
        Variable_Name : not null Program.Elements.Expressions
          .Expression_Access;
        Expression    : not null Program.Elements.Expressions
          .Expression_Access;
     end record;

   procedure Initialize
    (Self : aliased in out Base_Assignment_Statement'Class);

   overriding procedure Visit
    (Self    : not null access Base_Assignment_Statement;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Variable_Name
    (Self : Base_Assignment_Statement)
      return not null Program.Elements.Expressions.Expression_Access;

   overriding function Expression
    (Self : Base_Assignment_Statement)
      return not null Program.Elements.Expressions.Expression_Access;

   overriding function Is_Assignment_Statement
    (Self : Base_Assignment_Statement)
      return Boolean;

   overriding function Is_Statement
    (Self : Base_Assignment_Statement)
      return Boolean;

   type Assignment_Statement is
     new Base_Assignment_Statement
       and Program.Elements.Assignment_Statements.Assignment_Statement_Text
     with record
        Assignment_Token : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Semicolon_Token  : not null Program.Lexical_Elements
          .Lexical_Element_Access;
     end record;

   overriding function To_Assignment_Statement_Text
    (Self : aliased in out Assignment_Statement)
      return Program.Elements.Assignment_Statements
          .Assignment_Statement_Text_Access;

   overriding function Assignment_Token
    (Self : Assignment_Statement)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Semicolon_Token
    (Self : Assignment_Statement)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   type Implicit_Assignment_Statement is
     new Base_Assignment_Statement
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
     end record;

   overriding function To_Assignment_Statement_Text
    (Self : aliased in out Implicit_Assignment_Statement)
      return Program.Elements.Assignment_Statements
          .Assignment_Statement_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Assignment_Statement)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Assignment_Statement)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Assignment_Statement)
      return Boolean;

end Program.Nodes.Assignment_Statements;
