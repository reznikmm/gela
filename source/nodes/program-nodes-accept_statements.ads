--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Lexical_Elements;
with Program.Elements.Identifiers;
with Program.Elements.Expressions;
with Program.Elements.Parameter_Specifications;
with Program.Element_Vectors;
with Program.Elements.Exception_Handlers;
with Program.Elements.Accept_Statements;
with Program.Element_Visitors;

package Program.Nodes.Accept_Statements is

   pragma Pure (Program.Nodes.Accept_Statements);

   type Accept_Statement is
     new Program.Nodes.Node
         and Program.Elements.Accept_Statements.Accept_Statement
         and Program.Elements.Accept_Statements.Accept_Statement_Text
     with private;

   function Create
    (Accept_Token             : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Entry_Name               : not null Program.Elements.Identifiers
         .Identifier_Access;
     Left_Bracket_Token       : Program.Lexical_Elements
         .Lexical_Element_Access;
     Entry_Index              : Program.Elements.Expressions.Expression_Access;
     Right_Bracket_Token      : Program.Lexical_Elements
         .Lexical_Element_Access;
     Left_Bracket_Token_2     : Program.Lexical_Elements
         .Lexical_Element_Access;
     Parameters               : Program.Elements.Parameter_Specifications
         .Parameter_Specification_Vector_Access;
     Right_Bracket_Token_2    : Program.Lexical_Elements
         .Lexical_Element_Access;
     Do_Token                 : Program.Lexical_Elements
         .Lexical_Element_Access;
     Statements               : Program.Element_Vectors.Element_Vector_Access;
     Exception_Token          : Program.Lexical_Elements
         .Lexical_Element_Access;
     Exception_Handlers       : Program.Elements.Exception_Handlers
         .Exception_Handler_Vector_Access;
     End_Token                : Program.Lexical_Elements
         .Lexical_Element_Access;
     End_Statement_Identifier : Program.Elements.Identifiers.Identifier_Access;
     Semicolon_Token          : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Accept_Statement;

   type Implicit_Accept_Statement is
     new Program.Nodes.Node
         and Program.Elements.Accept_Statements.Accept_Statement
     with private;

   function Create
    (Entry_Name               : not null Program.Elements.Identifiers
         .Identifier_Access;
     Entry_Index              : Program.Elements.Expressions.Expression_Access;
     Parameters               : Program.Elements.Parameter_Specifications
         .Parameter_Specification_Vector_Access;
     Statements               : Program.Element_Vectors.Element_Vector_Access;
     Exception_Handlers       : Program.Elements.Exception_Handlers
         .Exception_Handler_Vector_Access;
     End_Statement_Identifier : Program.Elements.Identifiers.Identifier_Access;
     Is_Part_Of_Implicit      : Boolean := False;
     Is_Part_Of_Inherited     : Boolean := False;
     Is_Part_Of_Instance      : Boolean := False)
      return Implicit_Accept_Statement
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Accept_Statement is
     abstract new Program.Nodes.Node
       and Program.Elements.Accept_Statements.Accept_Statement
     with record
        Entry_Name               : not null Program.Elements.Identifiers
          .Identifier_Access;
        Entry_Index              : Program.Elements.Expressions
          .Expression_Access;
        Parameters               : Program.Elements.Parameter_Specifications
          .Parameter_Specification_Vector_Access;
        Statements               : Program.Element_Vectors
          .Element_Vector_Access;
        Exception_Handlers       : Program.Elements.Exception_Handlers
          .Exception_Handler_Vector_Access;
        End_Statement_Identifier : Program.Elements.Identifiers
          .Identifier_Access;
     end record;

   procedure Initialize (Self : aliased in out Base_Accept_Statement'Class);

   overriding procedure Visit
    (Self    : not null access Base_Accept_Statement;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Entry_Name
    (Self : Base_Accept_Statement)
      return not null Program.Elements.Identifiers.Identifier_Access;

   overriding function Entry_Index
    (Self : Base_Accept_Statement)
      return Program.Elements.Expressions.Expression_Access;

   overriding function Parameters
    (Self : Base_Accept_Statement)
      return Program.Elements.Parameter_Specifications
          .Parameter_Specification_Vector_Access;

   overriding function Statements
    (Self : Base_Accept_Statement)
      return Program.Element_Vectors.Element_Vector_Access;

   overriding function Exception_Handlers
    (Self : Base_Accept_Statement)
      return Program.Elements.Exception_Handlers
          .Exception_Handler_Vector_Access;

   overriding function End_Statement_Identifier
    (Self : Base_Accept_Statement)
      return Program.Elements.Identifiers.Identifier_Access;

   overriding function Is_Accept_Statement
    (Self : Base_Accept_Statement)
      return Boolean;

   overriding function Is_Statement
    (Self : Base_Accept_Statement)
      return Boolean;

   type Accept_Statement is
     new Base_Accept_Statement
       and Program.Elements.Accept_Statements.Accept_Statement_Text
     with record
        Accept_Token          : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Left_Bracket_Token    : Program.Lexical_Elements
          .Lexical_Element_Access;
        Right_Bracket_Token   : Program.Lexical_Elements
          .Lexical_Element_Access;
        Left_Bracket_Token_2  : Program.Lexical_Elements
          .Lexical_Element_Access;
        Right_Bracket_Token_2 : Program.Lexical_Elements
          .Lexical_Element_Access;
        Do_Token              : Program.Lexical_Elements
          .Lexical_Element_Access;
        Exception_Token       : Program.Lexical_Elements
          .Lexical_Element_Access;
        End_Token             : Program.Lexical_Elements
          .Lexical_Element_Access;
        Semicolon_Token       : not null Program.Lexical_Elements
          .Lexical_Element_Access;
     end record;

   overriding function To_Accept_Statement_Text
    (Self : aliased in out Accept_Statement)
      return Program.Elements.Accept_Statements.Accept_Statement_Text_Access;

   overriding function Accept_Token
    (Self : Accept_Statement)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Left_Bracket_Token
    (Self : Accept_Statement)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Right_Bracket_Token
    (Self : Accept_Statement)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Left_Bracket_Token_2
    (Self : Accept_Statement)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Right_Bracket_Token_2
    (Self : Accept_Statement)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Do_Token
    (Self : Accept_Statement)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Exception_Token
    (Self : Accept_Statement)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function End_Token
    (Self : Accept_Statement)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Semicolon_Token
    (Self : Accept_Statement)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   type Implicit_Accept_Statement is
     new Base_Accept_Statement
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
     end record;

   overriding function To_Accept_Statement_Text
    (Self : aliased in out Implicit_Accept_Statement)
      return Program.Elements.Accept_Statements.Accept_Statement_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Accept_Statement)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Accept_Statement)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Accept_Statement)
      return Boolean;

end Program.Nodes.Accept_Statements;
