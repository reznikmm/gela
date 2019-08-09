--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Lexical_Elements;
with Program.Elements.Defining_Identifiers;
with Program.Elements.Aspect_Specifications;
with Program.Element_Vectors;
with Program.Elements.Identifiers;
with Program.Elements.Protected_Body_Declarations;
with Program.Element_Visitors;

package Program.Nodes.Protected_Body_Declarations is

   pragma Preelaborate;

   type Protected_Body_Declaration is
     new Program.Nodes.Node
         and Program.Elements.Protected_Body_Declarations
           .Protected_Body_Declaration
         and Program.Elements.Protected_Body_Declarations
           .Protected_Body_Declaration_Text
     with private;

   function Create
    (Protected_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Body_Token           : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name                 : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     With_Token           : Program.Lexical_Elements.Lexical_Element_Access;
     Aspects              : Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Is_Token             : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Protected_Operations : not null Program.Element_Vectors
         .Element_Vector_Access;
     End_Token            : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     End_Name             : Program.Elements.Identifiers.Identifier_Access;
     Semicolon_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Protected_Body_Declaration;

   type Implicit_Protected_Body_Declaration is
     new Program.Nodes.Node
         and Program.Elements.Protected_Body_Declarations
           .Protected_Body_Declaration
     with private;

   function Create
    (Name                 : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Aspects              : Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Protected_Operations : not null Program.Element_Vectors
         .Element_Vector_Access;
     End_Name             : Program.Elements.Identifiers.Identifier_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Protected_Body_Declaration
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Protected_Body_Declaration is
     abstract new Program.Nodes.Node
       and Program.Elements.Protected_Body_Declarations
         .Protected_Body_Declaration
     with record
        Name                 : not null Program.Elements.Defining_Identifiers
          .Defining_Identifier_Access;
        Aspects              : Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access;
        Protected_Operations : not null Program.Element_Vectors
          .Element_Vector_Access;
        End_Name             : Program.Elements.Identifiers.Identifier_Access;
     end record;

   procedure Initialize
    (Self : aliased in out Base_Protected_Body_Declaration'Class);

   overriding procedure Visit
    (Self    : not null access Base_Protected_Body_Declaration;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Name
    (Self : Base_Protected_Body_Declaration)
      return not null Program.Elements.Defining_Identifiers
          .Defining_Identifier_Access;

   overriding function Aspects
    (Self : Base_Protected_Body_Declaration)
      return Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access;

   overriding function Protected_Operations
    (Self : Base_Protected_Body_Declaration)
      return not null Program.Element_Vectors.Element_Vector_Access;

   overriding function End_Name
    (Self : Base_Protected_Body_Declaration)
      return Program.Elements.Identifiers.Identifier_Access;

   overriding function Is_Protected_Body_Declaration_Element
    (Self : Base_Protected_Body_Declaration)
      return Boolean;

   overriding function Is_Declaration_Element
    (Self : Base_Protected_Body_Declaration)
      return Boolean;

   type Protected_Body_Declaration is
     new Base_Protected_Body_Declaration
       and Program.Elements.Protected_Body_Declarations
         .Protected_Body_Declaration_Text
     with record
        Protected_Token : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Body_Token      : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        With_Token      : Program.Lexical_Elements.Lexical_Element_Access;
        Is_Token        : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        End_Token       : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Semicolon_Token : not null Program.Lexical_Elements
          .Lexical_Element_Access;
     end record;

   overriding function To_Protected_Body_Declaration_Text
    (Self : aliased in out Protected_Body_Declaration)
      return Program.Elements.Protected_Body_Declarations
          .Protected_Body_Declaration_Text_Access;

   overriding function Protected_Token
    (Self : Protected_Body_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Body_Token
    (Self : Protected_Body_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function With_Token
    (Self : Protected_Body_Declaration)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Is_Token
    (Self : Protected_Body_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function End_Token
    (Self : Protected_Body_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Semicolon_Token
    (Self : Protected_Body_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   type Implicit_Protected_Body_Declaration is
     new Base_Protected_Body_Declaration
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
     end record;

   overriding function To_Protected_Body_Declaration_Text
    (Self : aliased in out Implicit_Protected_Body_Declaration)
      return Program.Elements.Protected_Body_Declarations
          .Protected_Body_Declaration_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Protected_Body_Declaration)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Protected_Body_Declaration)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Protected_Body_Declaration)
      return Boolean;

end Program.Nodes.Protected_Body_Declarations;
