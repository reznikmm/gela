--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Lexical_Elements;
with Program.Elements.Expressions;
with Program.Element_Vectors;
with Program.Elements.Elsif_Paths;
with Program.Elements.If_Statements;
with Program.Element_Visitors;

package Program.Nodes.If_Statements is

   pragma Pure (Program.Nodes.If_Statements);

   type If_Statement is
     new Program.Nodes.Node and Program.Elements.If_Statements.If_Statement
         and Program.Elements.If_Statements.If_Statement_Text
     with private;

   function Create
    (If_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Condition       : not null Program.Elements.Expressions.Expression_Access;
     Then_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Then_Statements : not null Program.Element_Vectors.Element_Vector_Access;
     Elsif_Paths     : Program.Elements.Elsif_Paths.Elsif_Path_Vector_Access;
     Else_Token      : Program.Lexical_Elements.Lexical_Element_Access;
     Else_Statements : Program.Element_Vectors.Element_Vector_Access;
     End_Token       : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     If_Token_2      : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Semicolon_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return If_Statement;

   type Implicit_If_Statement is
     new Program.Nodes.Node and Program.Elements.If_Statements.If_Statement
     with private;

   function Create
    (Condition            : not null Program.Elements.Expressions
         .Expression_Access;
     Then_Statements      : not null Program.Element_Vectors
         .Element_Vector_Access;
     Elsif_Paths          : Program.Elements.Elsif_Paths
         .Elsif_Path_Vector_Access;
     Else_Statements      : Program.Element_Vectors.Element_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_If_Statement
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_If_Statement is
     abstract new Program.Nodes.Node
       and Program.Elements.If_Statements.If_Statement
     with record
        Condition       : not null Program.Elements.Expressions
          .Expression_Access;
        Then_Statements : not null Program.Element_Vectors
          .Element_Vector_Access;
        Elsif_Paths     : Program.Elements.Elsif_Paths
          .Elsif_Path_Vector_Access;
        Else_Statements : Program.Element_Vectors.Element_Vector_Access;
     end record;

   procedure Initialize (Self : aliased in out Base_If_Statement'Class);

   overriding procedure Visit
    (Self    : not null access Base_If_Statement;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Condition
    (Self : Base_If_Statement)
      return not null Program.Elements.Expressions.Expression_Access;

   overriding function Then_Statements
    (Self : Base_If_Statement)
      return not null Program.Element_Vectors.Element_Vector_Access;

   overriding function Elsif_Paths
    (Self : Base_If_Statement)
      return Program.Elements.Elsif_Paths.Elsif_Path_Vector_Access;

   overriding function Else_Statements
    (Self : Base_If_Statement)
      return Program.Element_Vectors.Element_Vector_Access;

   overriding function Is_If_Statement
    (Self : Base_If_Statement)
      return Boolean;

   overriding function Is_Statement (Self : Base_If_Statement) return Boolean;

   type If_Statement is
     new Base_If_Statement and Program.Elements.If_Statements.If_Statement_Text
     with record
        If_Token        : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Then_Token      : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Else_Token      : Program.Lexical_Elements.Lexical_Element_Access;
        End_Token       : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        If_Token_2      : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Semicolon_Token : not null Program.Lexical_Elements
          .Lexical_Element_Access;
     end record;

   overriding function To_If_Statement_Text
    (Self : aliased in out If_Statement)
      return Program.Elements.If_Statements.If_Statement_Text_Access;

   overriding function If_Token
    (Self : If_Statement)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Then_Token
    (Self : If_Statement)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Else_Token
    (Self : If_Statement)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function End_Token
    (Self : If_Statement)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function If_Token_2
    (Self : If_Statement)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Semicolon_Token
    (Self : If_Statement)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   type Implicit_If_Statement is
     new Base_If_Statement
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
     end record;

   overriding function To_If_Statement_Text
    (Self : aliased in out Implicit_If_Statement)
      return Program.Elements.If_Statements.If_Statement_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_If_Statement)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_If_Statement)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_If_Statement)
      return Boolean;

end Program.Nodes.If_Statements;
