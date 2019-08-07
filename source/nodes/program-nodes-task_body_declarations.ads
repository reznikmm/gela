--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Lexical_Elements;
with Program.Elements.Defining_Identifiers;
with Program.Elements.Aspect_Specifications;
with Program.Element_Vectors;
with Program.Elements.Exception_Handlers;
with Program.Elements.Identifiers;
with Program.Elements.Task_Body_Declarations;
with Program.Element_Visitors;

package Program.Nodes.Task_Body_Declarations is

   pragma Pure (Program.Nodes.Task_Body_Declarations);

   type Task_Body_Declaration is
     new Program.Nodes.Node
         and Program.Elements.Task_Body_Declarations.Task_Body_Declaration
         and Program.Elements.Task_Body_Declarations.Task_Body_Declaration_Text
     with private;

   function Create
    (Task_Token         : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Body_Token         : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name               : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     With_Token         : Program.Lexical_Elements.Lexical_Element_Access;
     Aspects            : Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Is_Token           : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Declarations       : Program.Element_Vectors.Element_Vector_Access;
     Begin_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Statements         : not null Program.Element_Vectors
         .Element_Vector_Access;
     Exception_Token    : Program.Lexical_Elements.Lexical_Element_Access;
     Exception_Handlers : Program.Elements.Exception_Handlers
         .Exception_Handler_Vector_Access;
     End_Token          : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     End_Name           : Program.Elements.Identifiers.Identifier_Access;
     Semicolon_Token    : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Task_Body_Declaration;

   type Implicit_Task_Body_Declaration is
     new Program.Nodes.Node
         and Program.Elements.Task_Body_Declarations.Task_Body_Declaration
     with private;

   function Create
    (Name                 : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Aspects              : Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Declarations         : Program.Element_Vectors.Element_Vector_Access;
     Statements           : not null Program.Element_Vectors
         .Element_Vector_Access;
     Exception_Handlers   : Program.Elements.Exception_Handlers
         .Exception_Handler_Vector_Access;
     End_Name             : Program.Elements.Identifiers.Identifier_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Task_Body_Declaration
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Task_Body_Declaration is
     abstract new Program.Nodes.Node
       and Program.Elements.Task_Body_Declarations.Task_Body_Declaration
     with record
        Name               : not null Program.Elements.Defining_Identifiers
          .Defining_Identifier_Access;
        Aspects            : Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access;
        Declarations       : Program.Element_Vectors.Element_Vector_Access;
        Statements         : not null Program.Element_Vectors
          .Element_Vector_Access;
        Exception_Handlers : Program.Elements.Exception_Handlers
          .Exception_Handler_Vector_Access;
        End_Name           : Program.Elements.Identifiers.Identifier_Access;
     end record;

   procedure Initialize
    (Self : aliased in out Base_Task_Body_Declaration'Class);

   overriding procedure Visit
    (Self    : not null access Base_Task_Body_Declaration;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Name
    (Self : Base_Task_Body_Declaration)
      return not null Program.Elements.Defining_Identifiers
          .Defining_Identifier_Access;

   overriding function Aspects
    (Self : Base_Task_Body_Declaration)
      return Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access;

   overriding function Declarations
    (Self : Base_Task_Body_Declaration)
      return Program.Element_Vectors.Element_Vector_Access;

   overriding function Statements
    (Self : Base_Task_Body_Declaration)
      return not null Program.Element_Vectors.Element_Vector_Access;

   overriding function Exception_Handlers
    (Self : Base_Task_Body_Declaration)
      return Program.Elements.Exception_Handlers
          .Exception_Handler_Vector_Access;

   overriding function End_Name
    (Self : Base_Task_Body_Declaration)
      return Program.Elements.Identifiers.Identifier_Access;

   overriding function Is_Task_Body_Declaration
    (Self : Base_Task_Body_Declaration)
      return Boolean;

   overriding function Is_Declaration
    (Self : Base_Task_Body_Declaration)
      return Boolean;

   type Task_Body_Declaration is
     new Base_Task_Body_Declaration
       and Program.Elements.Task_Body_Declarations.Task_Body_Declaration_Text
     with record
        Task_Token      : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Body_Token      : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        With_Token      : Program.Lexical_Elements.Lexical_Element_Access;
        Is_Token        : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Begin_Token     : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Exception_Token : Program.Lexical_Elements.Lexical_Element_Access;
        End_Token       : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Semicolon_Token : not null Program.Lexical_Elements
          .Lexical_Element_Access;
     end record;

   overriding function To_Task_Body_Declaration_Text
    (Self : aliased in out Task_Body_Declaration)
      return Program.Elements.Task_Body_Declarations
          .Task_Body_Declaration_Text_Access;

   overriding function Task_Token
    (Self : Task_Body_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Body_Token
    (Self : Task_Body_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function With_Token
    (Self : Task_Body_Declaration)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Is_Token
    (Self : Task_Body_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Begin_Token
    (Self : Task_Body_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Exception_Token
    (Self : Task_Body_Declaration)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function End_Token
    (Self : Task_Body_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Semicolon_Token
    (Self : Task_Body_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   type Implicit_Task_Body_Declaration is
     new Base_Task_Body_Declaration
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
     end record;

   overriding function To_Task_Body_Declaration_Text
    (Self : aliased in out Implicit_Task_Body_Declaration)
      return Program.Elements.Task_Body_Declarations
          .Task_Body_Declaration_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Task_Body_Declaration)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Task_Body_Declaration)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Task_Body_Declaration)
      return Boolean;

end Program.Nodes.Task_Body_Declarations;
