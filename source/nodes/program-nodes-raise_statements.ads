--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Lexical_Elements;
with Program.Elements.Expressions;
with Program.Elements.Raise_Statements;
with Program.Element_Visitors;

package Program.Nodes.Raise_Statements is

   pragma Preelaborate;

   type Raise_Statement is
     new Program.Nodes.Node
         and Program.Elements.Raise_Statements.Raise_Statement
         and Program.Elements.Raise_Statements.Raise_Statement_Text
     with private;

   function Create
    (Raise_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Raised_Exception   : Program.Elements.Expressions.Expression_Access;
     With_Token         : Program.Lexical_Elements.Lexical_Element_Access;
     Associated_Message : Program.Elements.Expressions.Expression_Access;
     Semicolon_Token    : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Raise_Statement;

   type Implicit_Raise_Statement is
     new Program.Nodes.Node
         and Program.Elements.Raise_Statements.Raise_Statement
     with private;

   function Create
    (Raised_Exception     : Program.Elements.Expressions.Expression_Access;
     Associated_Message   : Program.Elements.Expressions.Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Raise_Statement
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Raise_Statement is
     abstract new Program.Nodes.Node
       and Program.Elements.Raise_Statements.Raise_Statement
     with record
        Raised_Exception   : Program.Elements.Expressions.Expression_Access;
        Associated_Message : Program.Elements.Expressions.Expression_Access;
     end record;

   procedure Initialize (Self : aliased in out Base_Raise_Statement'Class);

   overriding procedure Visit
    (Self    : not null access Base_Raise_Statement;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Raised_Exception
    (Self : Base_Raise_Statement)
      return Program.Elements.Expressions.Expression_Access;

   overriding function Associated_Message
    (Self : Base_Raise_Statement)
      return Program.Elements.Expressions.Expression_Access;

   overriding function Is_Raise_Statement
    (Self : Base_Raise_Statement)
      return Boolean;

   overriding function Is_Statement
    (Self : Base_Raise_Statement)
      return Boolean;

   type Raise_Statement is
     new Base_Raise_Statement
       and Program.Elements.Raise_Statements.Raise_Statement_Text
     with record
        Raise_Token     : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        With_Token      : Program.Lexical_Elements.Lexical_Element_Access;
        Semicolon_Token : not null Program.Lexical_Elements
          .Lexical_Element_Access;
     end record;

   overriding function To_Raise_Statement_Text
    (Self : aliased in out Raise_Statement)
      return Program.Elements.Raise_Statements.Raise_Statement_Text_Access;

   overriding function Raise_Token
    (Self : Raise_Statement)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function With_Token
    (Self : Raise_Statement)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Semicolon_Token
    (Self : Raise_Statement)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   type Implicit_Raise_Statement is
     new Base_Raise_Statement
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
     end record;

   overriding function To_Raise_Statement_Text
    (Self : aliased in out Implicit_Raise_Statement)
      return Program.Elements.Raise_Statements.Raise_Statement_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Raise_Statement)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Raise_Statement)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Raise_Statement)
      return Boolean;

end Program.Nodes.Raise_Statements;
