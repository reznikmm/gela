--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Lexical_Elements;
with Program.Elements.Terminate_Alternative_Statements;
with Program.Element_Visitors;

package Program.Nodes.Terminate_Alternative_Statements is

   pragma Preelaborate;

   type Terminate_Alternative_Statement is
     new Program.Nodes.Node
         and Program.Elements.Terminate_Alternative_Statements
           .Terminate_Alternative_Statement
         and Program.Elements.Terminate_Alternative_Statements
           .Terminate_Alternative_Statement_Text
     with private;

   function Create
    (Terminate_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Semicolon_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Terminate_Alternative_Statement;

   type Implicit_Terminate_Alternative_Statement is
     new Program.Nodes.Node
         and Program.Elements.Terminate_Alternative_Statements
           .Terminate_Alternative_Statement
     with private;

   function Create
    (Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Terminate_Alternative_Statement
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Terminate_Alternative_Statement is
     abstract new Program.Nodes.Node
       and Program.Elements.Terminate_Alternative_Statements
         .Terminate_Alternative_Statement
     with null record;

   procedure Initialize
    (Self : aliased in out Base_Terminate_Alternative_Statement'Class);

   overriding procedure Visit
    (Self    : not null access Base_Terminate_Alternative_Statement;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Is_Terminate_Alternative_Statement_Element
    (Self : Base_Terminate_Alternative_Statement)
      return Boolean;

   overriding function Is_Statement_Element
    (Self : Base_Terminate_Alternative_Statement)
      return Boolean;

   type Terminate_Alternative_Statement is
     new Base_Terminate_Alternative_Statement
       and Program.Elements.Terminate_Alternative_Statements
         .Terminate_Alternative_Statement_Text
     with record
        Terminate_Token : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Semicolon_Token : not null Program.Lexical_Elements
          .Lexical_Element_Access;
     end record;

   overriding function To_Terminate_Alternative_Statement_Text
    (Self : aliased in out Terminate_Alternative_Statement)
      return Program.Elements.Terminate_Alternative_Statements
          .Terminate_Alternative_Statement_Text_Access;

   overriding function Terminate_Token
    (Self : Terminate_Alternative_Statement)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Semicolon_Token
    (Self : Terminate_Alternative_Statement)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   type Implicit_Terminate_Alternative_Statement is
     new Base_Terminate_Alternative_Statement
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
     end record;

   overriding function To_Terminate_Alternative_Statement_Text
    (Self : aliased in out Implicit_Terminate_Alternative_Statement)
      return Program.Elements.Terminate_Alternative_Statements
          .Terminate_Alternative_Statement_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Terminate_Alternative_Statement)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Terminate_Alternative_Statement)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Terminate_Alternative_Statement)
      return Boolean;

end Program.Nodes.Terminate_Alternative_Statements;
