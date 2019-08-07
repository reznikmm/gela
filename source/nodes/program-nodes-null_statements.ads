--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Lexical_Elements;
with Program.Elements.Null_Statements;
with Program.Element_Visitors;

package Program.Nodes.Null_Statements is

   pragma Preelaborate;

   type Null_Statement is
     new Program.Nodes.Node and Program.Elements.Null_Statements.Null_Statement
         and Program.Elements.Null_Statements.Null_Statement_Text
     with private;

   function Create
    (Null_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Semicolon_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Null_Statement;

   type Implicit_Null_Statement is
     new Program.Nodes.Node and Program.Elements.Null_Statements.Null_Statement
     with private;

   function Create
    (Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Null_Statement
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Null_Statement is
     abstract new Program.Nodes.Node
       and Program.Elements.Null_Statements.Null_Statement
     with null record;

   procedure Initialize (Self : aliased in out Base_Null_Statement'Class);

   overriding procedure Visit
    (Self    : not null access Base_Null_Statement;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Is_Null_Statement
    (Self : Base_Null_Statement)
      return Boolean;

   overriding function Is_Statement
    (Self : Base_Null_Statement)
      return Boolean;

   type Null_Statement is
     new Base_Null_Statement
       and Program.Elements.Null_Statements.Null_Statement_Text
     with record
        Null_Token      : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Semicolon_Token : not null Program.Lexical_Elements
          .Lexical_Element_Access;
     end record;

   overriding function To_Null_Statement_Text
    (Self : aliased in out Null_Statement)
      return Program.Elements.Null_Statements.Null_Statement_Text_Access;

   overriding function Null_Token
    (Self : Null_Statement)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Semicolon_Token
    (Self : Null_Statement)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   type Implicit_Null_Statement is
     new Base_Null_Statement
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
     end record;

   overriding function To_Null_Statement_Text
    (Self : aliased in out Implicit_Null_Statement)
      return Program.Elements.Null_Statements.Null_Statement_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Null_Statement)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Null_Statement)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Null_Statement)
      return Boolean;

end Program.Nodes.Null_Statements;
