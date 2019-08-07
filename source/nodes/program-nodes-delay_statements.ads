--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Lexical_Elements;
with Program.Elements.Expressions;
with Program.Elements.Delay_Statements;
with Program.Element_Visitors;

package Program.Nodes.Delay_Statements is

   pragma Preelaborate;

   type Delay_Statement is
     new Program.Nodes.Node
         and Program.Elements.Delay_Statements.Delay_Statement
         and Program.Elements.Delay_Statements.Delay_Statement_Text
     with private;

   function Create
    (Delay_Token     : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Until_Token     : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Expression      : not null Program.Elements.Expressions.Expression_Access;
     Semicolon_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Delay_Statement;

   type Implicit_Delay_Statement is
     new Program.Nodes.Node
         and Program.Elements.Delay_Statements.Delay_Statement
     with private;

   function Create
    (Expression           : not null Program.Elements.Expressions
         .Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Delay_Statement
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Delay_Statement is
     abstract new Program.Nodes.Node
       and Program.Elements.Delay_Statements.Delay_Statement
     with record
        Expression : not null Program.Elements.Expressions.Expression_Access;
     end record;

   procedure Initialize (Self : aliased in out Base_Delay_Statement'Class);

   overriding procedure Visit
    (Self    : not null access Base_Delay_Statement;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Expression
    (Self : Base_Delay_Statement)
      return not null Program.Elements.Expressions.Expression_Access;

   overriding function Is_Delay_Statement
    (Self : Base_Delay_Statement)
      return Boolean;

   overriding function Is_Statement
    (Self : Base_Delay_Statement)
      return Boolean;

   type Delay_Statement is
     new Base_Delay_Statement
       and Program.Elements.Delay_Statements.Delay_Statement_Text
     with record
        Delay_Token     : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Until_Token     : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Semicolon_Token : not null Program.Lexical_Elements
          .Lexical_Element_Access;
     end record;

   overriding function To_Delay_Statement_Text
    (Self : aliased in out Delay_Statement)
      return Program.Elements.Delay_Statements.Delay_Statement_Text_Access;

   overriding function Delay_Token
    (Self : Delay_Statement)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Until_Token
    (Self : Delay_Statement)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Semicolon_Token
    (Self : Delay_Statement)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   type Implicit_Delay_Statement is
     new Base_Delay_Statement
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
     end record;

   overriding function To_Delay_Statement_Text
    (Self : aliased in out Implicit_Delay_Statement)
      return Program.Elements.Delay_Statements.Delay_Statement_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Delay_Statement)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Delay_Statement)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Delay_Statement)
      return Boolean;

end Program.Nodes.Delay_Statements;
