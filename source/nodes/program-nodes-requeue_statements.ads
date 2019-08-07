--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Lexical_Elements;
with Program.Elements.Expressions;
with Program.Elements.Requeue_Statements;
with Program.Element_Visitors;

package Program.Nodes.Requeue_Statements is

   pragma Preelaborate;

   type Requeue_Statement is
     new Program.Nodes.Node
         and Program.Elements.Requeue_Statements.Requeue_Statement
         and Program.Elements.Requeue_Statements.Requeue_Statement_Text
     with private;

   function Create
    (Requeue_Token   : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Entry_Name      : not null Program.Elements.Expressions.Expression_Access;
     With_Token      : Program.Lexical_Elements.Lexical_Element_Access;
     Abort_Token     : Program.Lexical_Elements.Lexical_Element_Access;
     Semicolon_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Requeue_Statement;

   type Implicit_Requeue_Statement is
     new Program.Nodes.Node
         and Program.Elements.Requeue_Statements.Requeue_Statement
     with private;

   function Create
    (Entry_Name           : not null Program.Elements.Expressions
         .Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False;
     Has_With_Abort       : Boolean := False)
      return Implicit_Requeue_Statement
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Requeue_Statement is
     abstract new Program.Nodes.Node
       and Program.Elements.Requeue_Statements.Requeue_Statement
     with record
        Entry_Name : not null Program.Elements.Expressions.Expression_Access;
     end record;

   procedure Initialize (Self : aliased in out Base_Requeue_Statement'Class);

   overriding procedure Visit
    (Self    : not null access Base_Requeue_Statement;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Entry_Name
    (Self : Base_Requeue_Statement)
      return not null Program.Elements.Expressions.Expression_Access;

   overriding function Is_Requeue_Statement
    (Self : Base_Requeue_Statement)
      return Boolean;

   overriding function Is_Statement
    (Self : Base_Requeue_Statement)
      return Boolean;

   type Requeue_Statement is
     new Base_Requeue_Statement
       and Program.Elements.Requeue_Statements.Requeue_Statement_Text
     with record
        Requeue_Token   : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        With_Token      : Program.Lexical_Elements.Lexical_Element_Access;
        Abort_Token     : Program.Lexical_Elements.Lexical_Element_Access;
        Semicolon_Token : not null Program.Lexical_Elements
          .Lexical_Element_Access;
     end record;

   overriding function To_Requeue_Statement_Text
    (Self : aliased in out Requeue_Statement)
      return Program.Elements.Requeue_Statements.Requeue_Statement_Text_Access;

   overriding function Requeue_Token
    (Self : Requeue_Statement)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function With_Token
    (Self : Requeue_Statement)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Abort_Token
    (Self : Requeue_Statement)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Semicolon_Token
    (Self : Requeue_Statement)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Has_With_Abort
    (Self : Requeue_Statement)
      return Boolean;

   type Implicit_Requeue_Statement is
     new Base_Requeue_Statement
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
        Has_With_Abort       : Boolean;
     end record;

   overriding function To_Requeue_Statement_Text
    (Self : aliased in out Implicit_Requeue_Statement)
      return Program.Elements.Requeue_Statements.Requeue_Statement_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Requeue_Statement)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Requeue_Statement)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Requeue_Statement)
      return Boolean;

   overriding function Has_With_Abort
    (Self : Implicit_Requeue_Statement)
      return Boolean;

end Program.Nodes.Requeue_Statements;
