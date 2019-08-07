--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Lexical_Elements;
with Program.Elements.Expressions;
with Program.Elements.Case_Paths;
with Program.Elements.Case_Statements;
with Program.Element_Visitors;

package Program.Nodes.Case_Statements is

   pragma Preelaborate;

   type Case_Statement is
     new Program.Nodes.Node and Program.Elements.Case_Statements.Case_Statement
         and Program.Elements.Case_Statements.Case_Statement_Text
     with private;

   function Create
    (Case_Token           : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Selecting_Expression : not null Program.Elements.Expressions
         .Expression_Access;
     Is_Token             : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Paths                : not null Program.Elements.Case_Paths
         .Case_Path_Vector_Access;
     End_Token            : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Case_Token_2         : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Semicolon_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Case_Statement;

   type Implicit_Case_Statement is
     new Program.Nodes.Node and Program.Elements.Case_Statements.Case_Statement
     with private;

   function Create
    (Selecting_Expression : not null Program.Elements.Expressions
         .Expression_Access;
     Paths                : not null Program.Elements.Case_Paths
         .Case_Path_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Case_Statement
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Case_Statement is
     abstract new Program.Nodes.Node
       and Program.Elements.Case_Statements.Case_Statement
     with record
        Selecting_Expression : not null Program.Elements.Expressions
          .Expression_Access;
        Paths                : not null Program.Elements.Case_Paths
          .Case_Path_Vector_Access;
     end record;

   procedure Initialize (Self : aliased in out Base_Case_Statement'Class);

   overriding procedure Visit
    (Self    : not null access Base_Case_Statement;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Selecting_Expression
    (Self : Base_Case_Statement)
      return not null Program.Elements.Expressions.Expression_Access;

   overriding function Paths
    (Self : Base_Case_Statement)
      return not null Program.Elements.Case_Paths.Case_Path_Vector_Access;

   overriding function Is_Case_Statement
    (Self : Base_Case_Statement)
      return Boolean;

   overriding function Is_Statement
    (Self : Base_Case_Statement)
      return Boolean;

   type Case_Statement is
     new Base_Case_Statement
       and Program.Elements.Case_Statements.Case_Statement_Text
     with record
        Case_Token      : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Is_Token        : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        End_Token       : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Case_Token_2    : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Semicolon_Token : not null Program.Lexical_Elements
          .Lexical_Element_Access;
     end record;

   overriding function To_Case_Statement_Text
    (Self : aliased in out Case_Statement)
      return Program.Elements.Case_Statements.Case_Statement_Text_Access;

   overriding function Case_Token
    (Self : Case_Statement)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Is_Token
    (Self : Case_Statement)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function End_Token
    (Self : Case_Statement)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Case_Token_2
    (Self : Case_Statement)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Semicolon_Token
    (Self : Case_Statement)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   type Implicit_Case_Statement is
     new Base_Case_Statement
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
     end record;

   overriding function To_Case_Statement_Text
    (Self : aliased in out Implicit_Case_Statement)
      return Program.Elements.Case_Statements.Case_Statement_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Case_Statement)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Case_Statement)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Case_Statement)
      return Boolean;

end Program.Nodes.Case_Statements;
