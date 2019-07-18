--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Lexical_Elements;
with Program.Elements.Expressions;
with Program.Elements.Simple_Return_Statements;
with Program.Element_Visitors;

package Program.Nodes.Simple_Return_Statements is

   pragma Pure (Program.Nodes.Simple_Return_Statements);

   type Simple_Return_Statement is
     new Program.Nodes.Node
         and Program.Elements.Simple_Return_Statements.Simple_Return_Statement
         and Program.Elements.Simple_Return_Statements
           .Simple_Return_Statement_Text
     with private;

   function Create
    (Return_Token    : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Expression      : Program.Elements.Expressions.Expression_Access;
     Semicolon_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Simple_Return_Statement;

   type Implicit_Simple_Return_Statement is
     new Program.Nodes.Node
         and Program.Elements.Simple_Return_Statements.Simple_Return_Statement
     with private;

   function Create
    (Expression           : Program.Elements.Expressions.Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Simple_Return_Statement
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Simple_Return_Statement is
     abstract new Program.Nodes.Node
       and Program.Elements.Simple_Return_Statements.Simple_Return_Statement
     with record
        Expression : Program.Elements.Expressions.Expression_Access;
     end record;

   procedure Initialize
    (Self : aliased in out Base_Simple_Return_Statement'Class);

   overriding procedure Visit
    (Self    : not null access Base_Simple_Return_Statement;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Expression
    (Self : Base_Simple_Return_Statement)
      return Program.Elements.Expressions.Expression_Access;

   overriding function Is_Simple_Return_Statement
    (Self : Base_Simple_Return_Statement)
      return Boolean;

   overriding function Is_Statement
    (Self : Base_Simple_Return_Statement)
      return Boolean;

   type Simple_Return_Statement is
     new Base_Simple_Return_Statement
       and Program.Elements.Simple_Return_Statements
         .Simple_Return_Statement_Text
     with record
        Return_Token    : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Semicolon_Token : not null Program.Lexical_Elements
          .Lexical_Element_Access;
     end record;

   overriding function To_Simple_Return_Statement_Text
    (Self : aliased in out Simple_Return_Statement)
      return Program.Elements.Simple_Return_Statements
          .Simple_Return_Statement_Text_Access;

   overriding function Return_Token
    (Self : Simple_Return_Statement)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Semicolon_Token
    (Self : Simple_Return_Statement)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   type Implicit_Simple_Return_Statement is
     new Base_Simple_Return_Statement
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
     end record;

   overriding function To_Simple_Return_Statement_Text
    (Self : aliased in out Implicit_Simple_Return_Statement)
      return Program.Elements.Simple_Return_Statements
          .Simple_Return_Statement_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Simple_Return_Statement)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Simple_Return_Statement)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Simple_Return_Statement)
      return Boolean;

end Program.Nodes.Simple_Return_Statements;
