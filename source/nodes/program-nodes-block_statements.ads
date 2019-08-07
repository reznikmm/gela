--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Defining_Identifiers;
with Program.Lexical_Elements;
with Program.Element_Vectors;
with Program.Elements.Exception_Handlers;
with Program.Elements.Identifiers;
with Program.Elements.Block_Statements;
with Program.Element_Visitors;

package Program.Nodes.Block_Statements is

   pragma Preelaborate;

   type Block_Statement is
     new Program.Nodes.Node
         and Program.Elements.Block_Statements.Block_Statement
         and Program.Elements.Block_Statements.Block_Statement_Text
     with private;

   function Create
    (Statement_Identifier     : Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Colon_Token              : Program.Lexical_Elements
         .Lexical_Element_Access;
     Declare_Token            : Program.Lexical_Elements
         .Lexical_Element_Access;
     Declarations             : Program.Element_Vectors.Element_Vector_Access;
     Begin_Token              : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Statements               : not null Program.Element_Vectors
         .Element_Vector_Access;
     Exception_Token          : Program.Lexical_Elements
         .Lexical_Element_Access;
     Exception_Handlers       : Program.Elements.Exception_Handlers
         .Exception_Handler_Vector_Access;
     End_Token                : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     End_Statement_Identifier : Program.Elements.Identifiers.Identifier_Access;
     Semicolon_Token          : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Block_Statement;

   type Implicit_Block_Statement is
     new Program.Nodes.Node
         and Program.Elements.Block_Statements.Block_Statement
     with private;

   function Create
    (Statement_Identifier     : Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Declarations             : Program.Element_Vectors.Element_Vector_Access;
     Statements               : not null Program.Element_Vectors
         .Element_Vector_Access;
     Exception_Handlers       : Program.Elements.Exception_Handlers
         .Exception_Handler_Vector_Access;
     End_Statement_Identifier : Program.Elements.Identifiers.Identifier_Access;
     Is_Part_Of_Implicit      : Boolean := False;
     Is_Part_Of_Inherited     : Boolean := False;
     Is_Part_Of_Instance      : Boolean := False)
      return Implicit_Block_Statement
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Block_Statement is
     abstract new Program.Nodes.Node
       and Program.Elements.Block_Statements.Block_Statement
     with record
        Statement_Identifier     : Program.Elements.Defining_Identifiers
          .Defining_Identifier_Access;
        Declarations             : Program.Element_Vectors
          .Element_Vector_Access;
        Statements               : not null Program.Element_Vectors
          .Element_Vector_Access;
        Exception_Handlers       : Program.Elements.Exception_Handlers
          .Exception_Handler_Vector_Access;
        End_Statement_Identifier : Program.Elements.Identifiers
          .Identifier_Access;
     end record;

   procedure Initialize (Self : aliased in out Base_Block_Statement'Class);

   overriding procedure Visit
    (Self    : not null access Base_Block_Statement;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Statement_Identifier
    (Self : Base_Block_Statement)
      return Program.Elements.Defining_Identifiers.Defining_Identifier_Access;

   overriding function Declarations
    (Self : Base_Block_Statement)
      return Program.Element_Vectors.Element_Vector_Access;

   overriding function Statements
    (Self : Base_Block_Statement)
      return not null Program.Element_Vectors.Element_Vector_Access;

   overriding function Exception_Handlers
    (Self : Base_Block_Statement)
      return Program.Elements.Exception_Handlers
          .Exception_Handler_Vector_Access;

   overriding function End_Statement_Identifier
    (Self : Base_Block_Statement)
      return Program.Elements.Identifiers.Identifier_Access;

   overriding function Is_Block_Statement
    (Self : Base_Block_Statement)
      return Boolean;

   overriding function Is_Statement
    (Self : Base_Block_Statement)
      return Boolean;

   type Block_Statement is
     new Base_Block_Statement
       and Program.Elements.Block_Statements.Block_Statement_Text
     with record
        Colon_Token     : Program.Lexical_Elements.Lexical_Element_Access;
        Declare_Token   : Program.Lexical_Elements.Lexical_Element_Access;
        Begin_Token     : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Exception_Token : Program.Lexical_Elements.Lexical_Element_Access;
        End_Token       : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Semicolon_Token : not null Program.Lexical_Elements
          .Lexical_Element_Access;
     end record;

   overriding function To_Block_Statement_Text
    (Self : aliased in out Block_Statement)
      return Program.Elements.Block_Statements.Block_Statement_Text_Access;

   overriding function Colon_Token
    (Self : Block_Statement)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Declare_Token
    (Self : Block_Statement)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Begin_Token
    (Self : Block_Statement)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Exception_Token
    (Self : Block_Statement)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function End_Token
    (Self : Block_Statement)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Semicolon_Token
    (Self : Block_Statement)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   type Implicit_Block_Statement is
     new Base_Block_Statement
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
     end record;

   overriding function To_Block_Statement_Text
    (Self : aliased in out Implicit_Block_Statement)
      return Program.Elements.Block_Statements.Block_Statement_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Block_Statement)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Block_Statement)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Block_Statement)
      return Boolean;

end Program.Nodes.Block_Statements;
