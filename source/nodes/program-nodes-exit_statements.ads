--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Lexical_Elements;
with Program.Elements.Expressions;
with Program.Elements.Exit_Statements;
with Program.Element_Visitors;

package Program.Nodes.Exit_Statements is

   pragma Preelaborate;

   type Exit_Statement is
     new Program.Nodes.Node and Program.Elements.Exit_Statements.Exit_Statement
         and Program.Elements.Exit_Statements.Exit_Statement_Text
     with private;

   function Create
    (Exit_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Exit_Loop_Name  : Program.Elements.Expressions.Expression_Access;
     When_Token      : Program.Lexical_Elements.Lexical_Element_Access;
     Condition       : Program.Elements.Expressions.Expression_Access;
     Semicolon_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Exit_Statement;

   type Implicit_Exit_Statement is
     new Program.Nodes.Node and Program.Elements.Exit_Statements.Exit_Statement
     with private;

   function Create
    (Exit_Loop_Name       : Program.Elements.Expressions.Expression_Access;
     Condition            : Program.Elements.Expressions.Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Exit_Statement
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Exit_Statement is
     abstract new Program.Nodes.Node
       and Program.Elements.Exit_Statements.Exit_Statement
     with record
        Exit_Loop_Name : Program.Elements.Expressions.Expression_Access;
        Condition      : Program.Elements.Expressions.Expression_Access;
     end record;

   procedure Initialize (Self : aliased in out Base_Exit_Statement'Class);

   overriding procedure Visit
    (Self    : not null access Base_Exit_Statement;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Exit_Loop_Name
    (Self : Base_Exit_Statement)
      return Program.Elements.Expressions.Expression_Access;

   overriding function Condition
    (Self : Base_Exit_Statement)
      return Program.Elements.Expressions.Expression_Access;

   overriding function Is_Exit_Statement_Element
    (Self : Base_Exit_Statement)
      return Boolean;

   overriding function Is_Statement_Element
    (Self : Base_Exit_Statement)
      return Boolean;

   type Exit_Statement is
     new Base_Exit_Statement
       and Program.Elements.Exit_Statements.Exit_Statement_Text
     with record
        Exit_Token      : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        When_Token      : Program.Lexical_Elements.Lexical_Element_Access;
        Semicolon_Token : not null Program.Lexical_Elements
          .Lexical_Element_Access;
     end record;

   overriding function To_Exit_Statement_Text
    (Self : aliased in out Exit_Statement)
      return Program.Elements.Exit_Statements.Exit_Statement_Text_Access;

   overriding function Exit_Token
    (Self : Exit_Statement)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function When_Token
    (Self : Exit_Statement)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Semicolon_Token
    (Self : Exit_Statement)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   type Implicit_Exit_Statement is
     new Base_Exit_Statement
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
     end record;

   overriding function To_Exit_Statement_Text
    (Self : aliased in out Implicit_Exit_Statement)
      return Program.Elements.Exit_Statements.Exit_Statement_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Exit_Statement)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Exit_Statement)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Exit_Statement)
      return Boolean;

end Program.Nodes.Exit_Statements;
