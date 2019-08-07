--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Defining_Identifiers;
with Program.Lexical_Elements;
with Program.Elements.Expressions;
with Program.Element_Vectors;
with Program.Elements.Identifiers;
with Program.Elements.While_Loop_Statements;
with Program.Element_Visitors;

package Program.Nodes.While_Loop_Statements is

   pragma Preelaborate;

   type While_Loop_Statement is
     new Program.Nodes.Node
         and Program.Elements.While_Loop_Statements.While_Loop_Statement
         and Program.Elements.While_Loop_Statements.While_Loop_Statement_Text
     with private;

   function Create
    (Statement_Identifier     : Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Colon_Token              : Program.Lexical_Elements
         .Lexical_Element_Access;
     While_Token              : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Condition                : not null Program.Elements.Expressions
         .Expression_Access;
     Loop_Token               : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Statements               : not null Program.Element_Vectors
         .Element_Vector_Access;
     End_Token                : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Loop_Token_2             : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     End_Statement_Identifier : Program.Elements.Identifiers.Identifier_Access;
     Semicolon_Token          : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return While_Loop_Statement;

   type Implicit_While_Loop_Statement is
     new Program.Nodes.Node
         and Program.Elements.While_Loop_Statements.While_Loop_Statement
     with private;

   function Create
    (Statement_Identifier     : Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Condition                : not null Program.Elements.Expressions
         .Expression_Access;
     Statements               : not null Program.Element_Vectors
         .Element_Vector_Access;
     End_Statement_Identifier : Program.Elements.Identifiers.Identifier_Access;
     Is_Part_Of_Implicit      : Boolean := False;
     Is_Part_Of_Inherited     : Boolean := False;
     Is_Part_Of_Instance      : Boolean := False)
      return Implicit_While_Loop_Statement
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_While_Loop_Statement is
     abstract new Program.Nodes.Node
       and Program.Elements.While_Loop_Statements.While_Loop_Statement
     with record
        Statement_Identifier     : Program.Elements.Defining_Identifiers
          .Defining_Identifier_Access;
        Condition                : not null Program.Elements.Expressions
          .Expression_Access;
        Statements               : not null Program.Element_Vectors
          .Element_Vector_Access;
        End_Statement_Identifier : Program.Elements.Identifiers
          .Identifier_Access;
     end record;

   procedure Initialize
    (Self : aliased in out Base_While_Loop_Statement'Class);

   overriding procedure Visit
    (Self    : not null access Base_While_Loop_Statement;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Statement_Identifier
    (Self : Base_While_Loop_Statement)
      return Program.Elements.Defining_Identifiers.Defining_Identifier_Access;

   overriding function Condition
    (Self : Base_While_Loop_Statement)
      return not null Program.Elements.Expressions.Expression_Access;

   overriding function Statements
    (Self : Base_While_Loop_Statement)
      return not null Program.Element_Vectors.Element_Vector_Access;

   overriding function End_Statement_Identifier
    (Self : Base_While_Loop_Statement)
      return Program.Elements.Identifiers.Identifier_Access;

   overriding function Is_While_Loop_Statement
    (Self : Base_While_Loop_Statement)
      return Boolean;

   overriding function Is_Statement
    (Self : Base_While_Loop_Statement)
      return Boolean;

   type While_Loop_Statement is
     new Base_While_Loop_Statement
       and Program.Elements.While_Loop_Statements.While_Loop_Statement_Text
     with record
        Colon_Token     : Program.Lexical_Elements.Lexical_Element_Access;
        While_Token     : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Loop_Token      : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        End_Token       : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Loop_Token_2    : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Semicolon_Token : not null Program.Lexical_Elements
          .Lexical_Element_Access;
     end record;

   overriding function To_While_Loop_Statement_Text
    (Self : aliased in out While_Loop_Statement)
      return Program.Elements.While_Loop_Statements
          .While_Loop_Statement_Text_Access;

   overriding function Colon_Token
    (Self : While_Loop_Statement)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function While_Token
    (Self : While_Loop_Statement)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Loop_Token
    (Self : While_Loop_Statement)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function End_Token
    (Self : While_Loop_Statement)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Loop_Token_2
    (Self : While_Loop_Statement)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Semicolon_Token
    (Self : While_Loop_Statement)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   type Implicit_While_Loop_Statement is
     new Base_While_Loop_Statement
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
     end record;

   overriding function To_While_Loop_Statement_Text
    (Self : aliased in out Implicit_While_Loop_Statement)
      return Program.Elements.While_Loop_Statements
          .While_Loop_Statement_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_While_Loop_Statement)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_While_Loop_Statement)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_While_Loop_Statement)
      return Boolean;

end Program.Nodes.While_Loop_Statements;
