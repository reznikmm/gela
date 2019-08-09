--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Lexical_Elements;
with Program.Elements.Defining_Identifiers;
with Program.Elements.Entry_Index_Specifications;
with Program.Elements.Parameter_Specifications;
with Program.Elements.Expressions;
with Program.Element_Vectors;
with Program.Elements.Exception_Handlers;
with Program.Elements.Identifiers;
with Program.Elements.Entry_Body_Declarations;
with Program.Element_Visitors;

package Program.Nodes.Entry_Body_Declarations is

   pragma Preelaborate;

   type Entry_Body_Declaration is
     new Program.Nodes.Node
         and Program.Elements.Entry_Body_Declarations.Entry_Body_Declaration
         and Program.Elements.Entry_Body_Declarations
           .Entry_Body_Declaration_Text
     with private;

   function Create
    (Entry_Token           : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name                  : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Left_Bracket_Token    : Program.Lexical_Elements.Lexical_Element_Access;
     Entry_Index           : not null Program.Elements
         .Entry_Index_Specifications.Entry_Index_Specification_Access;
     Right_Bracket_Token   : Program.Lexical_Elements.Lexical_Element_Access;
     Left_Bracket_Token_2  : Program.Lexical_Elements.Lexical_Element_Access;
     Parameters            : Program.Elements.Parameter_Specifications
         .Parameter_Specification_Vector_Access;
     Right_Bracket_Token_2 : Program.Lexical_Elements.Lexical_Element_Access;
     When_Token            : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Entry_Barrier         : not null Program.Elements.Expressions
         .Expression_Access;
     Is_Token              : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Declarations          : Program.Element_Vectors.Element_Vector_Access;
     Begin_Token           : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Statements            : not null Program.Element_Vectors
         .Element_Vector_Access;
     Exception_Token       : Program.Lexical_Elements.Lexical_Element_Access;
     Exception_Handlers    : Program.Elements.Exception_Handlers
         .Exception_Handler_Vector_Access;
     End_Token             : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     End_Name              : Program.Elements.Identifiers.Identifier_Access;
     Semicolon_Token       : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Entry_Body_Declaration;

   type Implicit_Entry_Body_Declaration is
     new Program.Nodes.Node
         and Program.Elements.Entry_Body_Declarations.Entry_Body_Declaration
     with private;

   function Create
    (Name                 : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Entry_Index          : not null Program.Elements
         .Entry_Index_Specifications.Entry_Index_Specification_Access;
     Parameters           : Program.Elements.Parameter_Specifications
         .Parameter_Specification_Vector_Access;
     Entry_Barrier        : not null Program.Elements.Expressions
         .Expression_Access;
     Declarations         : Program.Element_Vectors.Element_Vector_Access;
     Statements           : not null Program.Element_Vectors
         .Element_Vector_Access;
     Exception_Handlers   : Program.Elements.Exception_Handlers
         .Exception_Handler_Vector_Access;
     End_Name             : Program.Elements.Identifiers.Identifier_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Entry_Body_Declaration
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Entry_Body_Declaration is
     abstract new Program.Nodes.Node
       and Program.Elements.Entry_Body_Declarations.Entry_Body_Declaration
     with record
        Name               : not null Program.Elements.Defining_Identifiers
          .Defining_Identifier_Access;
        Entry_Index        : not null Program.Elements
          .Entry_Index_Specifications.Entry_Index_Specification_Access;
        Parameters         : Program.Elements.Parameter_Specifications
          .Parameter_Specification_Vector_Access;
        Entry_Barrier      : not null Program.Elements.Expressions
          .Expression_Access;
        Declarations       : Program.Element_Vectors.Element_Vector_Access;
        Statements         : not null Program.Element_Vectors
          .Element_Vector_Access;
        Exception_Handlers : Program.Elements.Exception_Handlers
          .Exception_Handler_Vector_Access;
        End_Name           : Program.Elements.Identifiers.Identifier_Access;
     end record;

   procedure Initialize
    (Self : aliased in out Base_Entry_Body_Declaration'Class);

   overriding procedure Visit
    (Self    : not null access Base_Entry_Body_Declaration;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Name
    (Self : Base_Entry_Body_Declaration)
      return not null Program.Elements.Defining_Identifiers
          .Defining_Identifier_Access;

   overriding function Entry_Index
    (Self : Base_Entry_Body_Declaration)
      return not null Program.Elements.Entry_Index_Specifications
          .Entry_Index_Specification_Access;

   overriding function Parameters
    (Self : Base_Entry_Body_Declaration)
      return Program.Elements.Parameter_Specifications
          .Parameter_Specification_Vector_Access;

   overriding function Entry_Barrier
    (Self : Base_Entry_Body_Declaration)
      return not null Program.Elements.Expressions.Expression_Access;

   overriding function Declarations
    (Self : Base_Entry_Body_Declaration)
      return Program.Element_Vectors.Element_Vector_Access;

   overriding function Statements
    (Self : Base_Entry_Body_Declaration)
      return not null Program.Element_Vectors.Element_Vector_Access;

   overriding function Exception_Handlers
    (Self : Base_Entry_Body_Declaration)
      return Program.Elements.Exception_Handlers
          .Exception_Handler_Vector_Access;

   overriding function End_Name
    (Self : Base_Entry_Body_Declaration)
      return Program.Elements.Identifiers.Identifier_Access;

   overriding function Is_Entry_Body_Declaration_Element
    (Self : Base_Entry_Body_Declaration)
      return Boolean;

   overriding function Is_Declaration_Element
    (Self : Base_Entry_Body_Declaration)
      return Boolean;

   type Entry_Body_Declaration is
     new Base_Entry_Body_Declaration
       and Program.Elements.Entry_Body_Declarations.Entry_Body_Declaration_Text
     with record
        Entry_Token           : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Left_Bracket_Token    : Program.Lexical_Elements
          .Lexical_Element_Access;
        Right_Bracket_Token   : Program.Lexical_Elements
          .Lexical_Element_Access;
        Left_Bracket_Token_2  : Program.Lexical_Elements
          .Lexical_Element_Access;
        Right_Bracket_Token_2 : Program.Lexical_Elements
          .Lexical_Element_Access;
        When_Token            : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Is_Token              : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Begin_Token           : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Exception_Token       : Program.Lexical_Elements
          .Lexical_Element_Access;
        End_Token             : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Semicolon_Token       : not null Program.Lexical_Elements
          .Lexical_Element_Access;
     end record;

   overriding function To_Entry_Body_Declaration_Text
    (Self : aliased in out Entry_Body_Declaration)
      return Program.Elements.Entry_Body_Declarations
          .Entry_Body_Declaration_Text_Access;

   overriding function Entry_Token
    (Self : Entry_Body_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Left_Bracket_Token
    (Self : Entry_Body_Declaration)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Right_Bracket_Token
    (Self : Entry_Body_Declaration)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Left_Bracket_Token_2
    (Self : Entry_Body_Declaration)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Right_Bracket_Token_2
    (Self : Entry_Body_Declaration)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function When_Token
    (Self : Entry_Body_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Is_Token
    (Self : Entry_Body_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Begin_Token
    (Self : Entry_Body_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Exception_Token
    (Self : Entry_Body_Declaration)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function End_Token
    (Self : Entry_Body_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Semicolon_Token
    (Self : Entry_Body_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   type Implicit_Entry_Body_Declaration is
     new Base_Entry_Body_Declaration
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
     end record;

   overriding function To_Entry_Body_Declaration_Text
    (Self : aliased in out Implicit_Entry_Body_Declaration)
      return Program.Elements.Entry_Body_Declarations
          .Entry_Body_Declaration_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Entry_Body_Declaration)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Entry_Body_Declaration)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Entry_Body_Declaration)
      return Boolean;

end Program.Nodes.Entry_Body_Declarations;
