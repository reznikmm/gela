--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Lexical_Elements;
with Program.Elements.Expressions;
with Program.Elements.Goto_Statements;
with Program.Element_Visitors;

package Program.Nodes.Goto_Statements is

   pragma Preelaborate;

   type Goto_Statement is
     new Program.Nodes.Node and Program.Elements.Goto_Statements.Goto_Statement
         and Program.Elements.Goto_Statements.Goto_Statement_Text
     with private;

   function Create
    (Goto_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Goto_Label      : not null Program.Elements.Expressions.Expression_Access;
     Semicolon_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Goto_Statement;

   type Implicit_Goto_Statement is
     new Program.Nodes.Node and Program.Elements.Goto_Statements.Goto_Statement
     with private;

   function Create
    (Goto_Label           : not null Program.Elements.Expressions
         .Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Goto_Statement
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Goto_Statement is
     abstract new Program.Nodes.Node
       and Program.Elements.Goto_Statements.Goto_Statement
     with record
        Goto_Label : not null Program.Elements.Expressions.Expression_Access;
     end record;

   procedure Initialize (Self : aliased in out Base_Goto_Statement'Class);

   overriding procedure Visit
    (Self    : not null access Base_Goto_Statement;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Goto_Label
    (Self : Base_Goto_Statement)
      return not null Program.Elements.Expressions.Expression_Access;

   overriding function Is_Goto_Statement
    (Self : Base_Goto_Statement)
      return Boolean;

   overriding function Is_Statement
    (Self : Base_Goto_Statement)
      return Boolean;

   type Goto_Statement is
     new Base_Goto_Statement
       and Program.Elements.Goto_Statements.Goto_Statement_Text
     with record
        Goto_Token      : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Semicolon_Token : not null Program.Lexical_Elements
          .Lexical_Element_Access;
     end record;

   overriding function To_Goto_Statement_Text
    (Self : aliased in out Goto_Statement)
      return Program.Elements.Goto_Statements.Goto_Statement_Text_Access;

   overriding function Goto_Token
    (Self : Goto_Statement)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Semicolon_Token
    (Self : Goto_Statement)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   type Implicit_Goto_Statement is
     new Base_Goto_Statement
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
     end record;

   overriding function To_Goto_Statement_Text
    (Self : aliased in out Implicit_Goto_Statement)
      return Program.Elements.Goto_Statements.Goto_Statement_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Goto_Statement)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Goto_Statement)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Goto_Statement)
      return Boolean;

end Program.Nodes.Goto_Statements;
