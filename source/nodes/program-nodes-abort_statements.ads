--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Lexical_Elements;
with Program.Elements.Expressions;
with Program.Elements.Abort_Statements;
with Program.Element_Visitors;

package Program.Nodes.Abort_Statements is

   pragma Pure (Program.Nodes.Abort_Statements);

   type Abort_Statement is
     new Program.Nodes.Node
         and Program.Elements.Abort_Statements.Abort_Statement
         and Program.Elements.Abort_Statements.Abort_Statement_Text
     with private;

   function Create
    (Abort_Token     : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Aborted_Tasks   : not null Program.Elements.Expressions
         .Expression_Vector_Access;
     Semicolon_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Abort_Statement;

   type Implicit_Abort_Statement is
     new Program.Nodes.Node
         and Program.Elements.Abort_Statements.Abort_Statement
     with private;

   function Create
    (Aborted_Tasks        : not null Program.Elements.Expressions
         .Expression_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Abort_Statement
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Abort_Statement is
     abstract new Program.Nodes.Node
       and Program.Elements.Abort_Statements.Abort_Statement
     with record
        Aborted_Tasks : not null Program.Elements.Expressions
          .Expression_Vector_Access;
     end record;

   procedure Initialize (Self : aliased in out Base_Abort_Statement'Class);

   overriding procedure Visit
    (Self    : not null access Base_Abort_Statement;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Aborted_Tasks
    (Self : Base_Abort_Statement)
      return not null Program.Elements.Expressions.Expression_Vector_Access;

   overriding function Is_Abort_Statement
    (Self : Base_Abort_Statement)
      return Boolean;

   overriding function Is_Statement
    (Self : Base_Abort_Statement)
      return Boolean;

   type Abort_Statement is
     new Base_Abort_Statement
       and Program.Elements.Abort_Statements.Abort_Statement_Text
     with record
        Abort_Token     : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Semicolon_Token : not null Program.Lexical_Elements
          .Lexical_Element_Access;
     end record;

   overriding function To_Abort_Statement_Text
    (Self : aliased in out Abort_Statement)
      return Program.Elements.Abort_Statements.Abort_Statement_Text_Access;

   overriding function Abort_Token
    (Self : Abort_Statement)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Semicolon_Token
    (Self : Abort_Statement)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   type Implicit_Abort_Statement is
     new Base_Abort_Statement
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
     end record;

   overriding function To_Abort_Statement_Text
    (Self : aliased in out Implicit_Abort_Statement)
      return Program.Elements.Abort_Statements.Abort_Statement_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Abort_Statement)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Abort_Statement)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Abort_Statement)
      return Boolean;

end Program.Nodes.Abort_Statements;
