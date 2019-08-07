--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Lexical_Elements;
with Program.Elements.Select_Paths;
with Program.Element_Vectors;
with Program.Elements.Select_Statements;
with Program.Element_Visitors;

package Program.Nodes.Select_Statements is

   pragma Pure (Program.Nodes.Select_Statements);

   type Select_Statement is
     new Program.Nodes.Node
         and Program.Elements.Select_Statements.Select_Statement
         and Program.Elements.Select_Statements.Select_Statement_Text
     with private;

   function Create
    (Select_Token          : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Paths                 : not null Program.Elements.Select_Paths
         .Select_Path_Vector_Access;
     Then_Token            : Program.Lexical_Elements.Lexical_Element_Access;
     Abort_Token           : Program.Lexical_Elements.Lexical_Element_Access;
     Then_Abort_Statements : Program.Element_Vectors.Element_Vector_Access;
     Else_Token            : Program.Lexical_Elements.Lexical_Element_Access;
     Else_Statements       : Program.Element_Vectors.Element_Vector_Access;
     End_Token             : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Select_Token_2        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Semicolon_Token       : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Select_Statement;

   type Implicit_Select_Statement is
     new Program.Nodes.Node
         and Program.Elements.Select_Statements.Select_Statement
     with private;

   function Create
    (Paths                 : not null Program.Elements.Select_Paths
         .Select_Path_Vector_Access;
     Then_Abort_Statements : Program.Element_Vectors.Element_Vector_Access;
     Else_Statements       : Program.Element_Vectors.Element_Vector_Access;
     Is_Part_Of_Implicit   : Boolean := False;
     Is_Part_Of_Inherited  : Boolean := False;
     Is_Part_Of_Instance   : Boolean := False)
      return Implicit_Select_Statement
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Select_Statement is
     abstract new Program.Nodes.Node
       and Program.Elements.Select_Statements.Select_Statement
     with record
        Paths                 : not null Program.Elements.Select_Paths
          .Select_Path_Vector_Access;
        Then_Abort_Statements : Program.Element_Vectors.Element_Vector_Access;
        Else_Statements       : Program.Element_Vectors.Element_Vector_Access;
     end record;

   procedure Initialize (Self : aliased in out Base_Select_Statement'Class);

   overriding procedure Visit
    (Self    : not null access Base_Select_Statement;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Paths
    (Self : Base_Select_Statement)
      return not null Program.Elements.Select_Paths.Select_Path_Vector_Access;

   overriding function Then_Abort_Statements
    (Self : Base_Select_Statement)
      return Program.Element_Vectors.Element_Vector_Access;

   overriding function Else_Statements
    (Self : Base_Select_Statement)
      return Program.Element_Vectors.Element_Vector_Access;

   overriding function Is_Select_Statement
    (Self : Base_Select_Statement)
      return Boolean;

   overriding function Is_Statement
    (Self : Base_Select_Statement)
      return Boolean;

   type Select_Statement is
     new Base_Select_Statement
       and Program.Elements.Select_Statements.Select_Statement_Text
     with record
        Select_Token    : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Then_Token      : Program.Lexical_Elements.Lexical_Element_Access;
        Abort_Token     : Program.Lexical_Elements.Lexical_Element_Access;
        Else_Token      : Program.Lexical_Elements.Lexical_Element_Access;
        End_Token       : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Select_Token_2  : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Semicolon_Token : not null Program.Lexical_Elements
          .Lexical_Element_Access;
     end record;

   overriding function To_Select_Statement_Text
    (Self : aliased in out Select_Statement)
      return Program.Elements.Select_Statements.Select_Statement_Text_Access;

   overriding function Select_Token
    (Self : Select_Statement)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Then_Token
    (Self : Select_Statement)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Abort_Token
    (Self : Select_Statement)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Else_Token
    (Self : Select_Statement)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function End_Token
    (Self : Select_Statement)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Select_Token_2
    (Self : Select_Statement)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Semicolon_Token
    (Self : Select_Statement)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   type Implicit_Select_Statement is
     new Base_Select_Statement
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
     end record;

   overriding function To_Select_Statement_Text
    (Self : aliased in out Implicit_Select_Statement)
      return Program.Elements.Select_Statements.Select_Statement_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Select_Statement)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Select_Statement)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Select_Statement)
      return Boolean;

end Program.Nodes.Select_Statements;
