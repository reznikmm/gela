--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Defining_Identifiers;
with Program.Lexical_Elements;
with Program.Elements.Loop_Parameter_Specifications;
with Program.Elements.Generalized_Iterator_Specifications;
with Program.Elements.Element_Iterator_Specifications;
with Program.Element_Vectors;
with Program.Elements.Identifiers;
with Program.Elements.For_Loop_Statements;
with Program.Element_Visitors;

package Program.Nodes.For_Loop_Statements is

   pragma Pure (Program.Nodes.For_Loop_Statements);

   type For_Loop_Statement is
     new Program.Nodes.Node
         and Program.Elements.For_Loop_Statements.For_Loop_Statement
         and Program.Elements.For_Loop_Statements.For_Loop_Statement_Text
     with private;

   function Create
    (Statement_Identifier     : Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Colon_Token              : Program.Lexical_Elements
         .Lexical_Element_Access;
     For_Token                : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Loop_Parameter           : Program.Elements.Loop_Parameter_Specifications
         .Loop_Parameter_Specification_Access;
     Generalized_Iterator     : Program.Elements
         .Generalized_Iterator_Specifications
         .Generalized_Iterator_Specification_Access;
     Element_Iterator         : Program.Elements
         .Element_Iterator_Specifications
         .Element_Iterator_Specification_Access;
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
      return For_Loop_Statement;

   type Implicit_For_Loop_Statement is
     new Program.Nodes.Node
         and Program.Elements.For_Loop_Statements.For_Loop_Statement
     with private;

   function Create
    (Statement_Identifier     : Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Loop_Parameter           : Program.Elements.Loop_Parameter_Specifications
         .Loop_Parameter_Specification_Access;
     Generalized_Iterator     : Program.Elements
         .Generalized_Iterator_Specifications
         .Generalized_Iterator_Specification_Access;
     Element_Iterator         : Program.Elements
         .Element_Iterator_Specifications
         .Element_Iterator_Specification_Access;
     Statements               : not null Program.Element_Vectors
         .Element_Vector_Access;
     End_Statement_Identifier : Program.Elements.Identifiers.Identifier_Access;
     Is_Part_Of_Implicit      : Boolean := False;
     Is_Part_Of_Inherited     : Boolean := False;
     Is_Part_Of_Instance      : Boolean := False)
      return Implicit_For_Loop_Statement
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_For_Loop_Statement is
     abstract new Program.Nodes.Node
       and Program.Elements.For_Loop_Statements.For_Loop_Statement
     with record
        Statement_Identifier     : Program.Elements.Defining_Identifiers
          .Defining_Identifier_Access;
        Loop_Parameter           : Program.Elements
          .Loop_Parameter_Specifications.Loop_Parameter_Specification_Access;
        Generalized_Iterator     : Program.Elements
          .Generalized_Iterator_Specifications
          .Generalized_Iterator_Specification_Access;
        Element_Iterator         : Program.Elements
          .Element_Iterator_Specifications
          .Element_Iterator_Specification_Access;
        Statements               : not null Program.Element_Vectors
          .Element_Vector_Access;
        End_Statement_Identifier : Program.Elements.Identifiers
          .Identifier_Access;
     end record;

   procedure Initialize (Self : aliased in out Base_For_Loop_Statement'Class);

   overriding procedure Visit
    (Self    : not null access Base_For_Loop_Statement;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Statement_Identifier
    (Self : Base_For_Loop_Statement)
      return Program.Elements.Defining_Identifiers.Defining_Identifier_Access;

   overriding function Loop_Parameter
    (Self : Base_For_Loop_Statement)
      return Program.Elements.Loop_Parameter_Specifications
          .Loop_Parameter_Specification_Access;

   overriding function Generalized_Iterator
    (Self : Base_For_Loop_Statement)
      return Program.Elements.Generalized_Iterator_Specifications
          .Generalized_Iterator_Specification_Access;

   overriding function Element_Iterator
    (Self : Base_For_Loop_Statement)
      return Program.Elements.Element_Iterator_Specifications
          .Element_Iterator_Specification_Access;

   overriding function Statements
    (Self : Base_For_Loop_Statement)
      return not null Program.Element_Vectors.Element_Vector_Access;

   overriding function End_Statement_Identifier
    (Self : Base_For_Loop_Statement)
      return Program.Elements.Identifiers.Identifier_Access;

   overriding function Is_For_Loop_Statement
    (Self : Base_For_Loop_Statement)
      return Boolean;

   overriding function Is_Statement
    (Self : Base_For_Loop_Statement)
      return Boolean;

   type For_Loop_Statement is
     new Base_For_Loop_Statement
       and Program.Elements.For_Loop_Statements.For_Loop_Statement_Text
     with record
        Colon_Token     : Program.Lexical_Elements.Lexical_Element_Access;
        For_Token       : not null Program.Lexical_Elements
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

   overriding function To_For_Loop_Statement_Text
    (Self : aliased in out For_Loop_Statement)
      return Program.Elements.For_Loop_Statements
          .For_Loop_Statement_Text_Access;

   overriding function Colon_Token
    (Self : For_Loop_Statement)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function For_Token
    (Self : For_Loop_Statement)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Loop_Token
    (Self : For_Loop_Statement)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function End_Token
    (Self : For_Loop_Statement)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Loop_Token_2
    (Self : For_Loop_Statement)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Semicolon_Token
    (Self : For_Loop_Statement)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   type Implicit_For_Loop_Statement is
     new Base_For_Loop_Statement
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
     end record;

   overriding function To_For_Loop_Statement_Text
    (Self : aliased in out Implicit_For_Loop_Statement)
      return Program.Elements.For_Loop_Statements
          .For_Loop_Statement_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_For_Loop_Statement)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_For_Loop_Statement)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_For_Loop_Statement)
      return Boolean;

end Program.Nodes.For_Loop_Statements;
