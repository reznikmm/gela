--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Expressions;
with Program.Lexical_Elements;
with Program.Elements.Parameter_Associations;
with Program.Elements.Call_Statements;
with Program.Element_Visitors;

package Program.Nodes.Call_Statements is

   pragma Pure (Program.Nodes.Call_Statements);

   type Call_Statement is
     new Program.Nodes.Node and Program.Elements.Call_Statements.Call_Statement
         and Program.Elements.Call_Statements.Call_Statement_Text
     with private;

   function Create
    (Called_Name         : not null Program.Elements.Expressions
         .Expression_Access;
     Left_Bracket_Token  : Program.Lexical_Elements.Lexical_Element_Access;
     Parameters          : Program.Elements.Parameter_Associations
         .Parameter_Association_Vector_Access;
     Right_Bracket_Token : Program.Lexical_Elements.Lexical_Element_Access;
     Semicolon_Token     : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Call_Statement;

   type Implicit_Call_Statement is
     new Program.Nodes.Node and Program.Elements.Call_Statements.Call_Statement
     with private;

   function Create
    (Called_Name          : not null Program.Elements.Expressions
         .Expression_Access;
     Parameters           : Program.Elements.Parameter_Associations
         .Parameter_Association_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Call_Statement
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Call_Statement is
     abstract new Program.Nodes.Node
       and Program.Elements.Call_Statements.Call_Statement
     with record
        Called_Name : not null Program.Elements.Expressions.Expression_Access;
        Parameters  : Program.Elements.Parameter_Associations
          .Parameter_Association_Vector_Access;
     end record;

   procedure Initialize (Self : aliased in out Base_Call_Statement'Class);

   overriding procedure Visit
    (Self    : not null access Base_Call_Statement;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Called_Name
    (Self : Base_Call_Statement)
      return not null Program.Elements.Expressions.Expression_Access;

   overriding function Parameters
    (Self : Base_Call_Statement)
      return Program.Elements.Parameter_Associations
          .Parameter_Association_Vector_Access;

   overriding function Is_Call_Statement
    (Self : Base_Call_Statement)
      return Boolean;

   overriding function Is_Statement
    (Self : Base_Call_Statement)
      return Boolean;

   type Call_Statement is
     new Base_Call_Statement
       and Program.Elements.Call_Statements.Call_Statement_Text
     with record
        Left_Bracket_Token  : Program.Lexical_Elements.Lexical_Element_Access;
        Right_Bracket_Token : Program.Lexical_Elements.Lexical_Element_Access;
        Semicolon_Token     : not null Program.Lexical_Elements
          .Lexical_Element_Access;
     end record;

   overriding function To_Call_Statement_Text
    (Self : aliased in out Call_Statement)
      return Program.Elements.Call_Statements.Call_Statement_Text_Access;

   overriding function Left_Bracket_Token
    (Self : Call_Statement)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Right_Bracket_Token
    (Self : Call_Statement)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Semicolon_Token
    (Self : Call_Statement)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   type Implicit_Call_Statement is
     new Base_Call_Statement
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
     end record;

   overriding function To_Call_Statement_Text
    (Self : aliased in out Implicit_Call_Statement)
      return Program.Elements.Call_Statements.Call_Statement_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Call_Statement)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Call_Statement)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Call_Statement)
      return Boolean;

end Program.Nodes.Call_Statements;
