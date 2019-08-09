--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Lexical_Elements;
with Program.Elements.Defining_Identifiers;
with Program.Elements.Definitions;
with Program.Elements.Formal_Type_Definitions;
with Program.Elements.Aspect_Specifications;
with Program.Elements.Formal_Type_Declarations;
with Program.Element_Visitors;

package Program.Nodes.Formal_Type_Declarations is

   pragma Preelaborate;

   type Formal_Type_Declaration is
     new Program.Nodes.Node
         and Program.Elements.Formal_Type_Declarations.Formal_Type_Declaration
         and Program.Elements.Formal_Type_Declarations
           .Formal_Type_Declaration_Text
     with private;

   function Create
    (Type_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name              : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Discriminant_Part : Program.Elements.Definitions.Definition_Access;
     Is_Token          : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Definition        : not null Program.Elements.Formal_Type_Definitions
         .Formal_Type_Definition_Access;
     With_Token        : Program.Lexical_Elements.Lexical_Element_Access;
     Aspects           : Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Semicolon_Token   : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Formal_Type_Declaration;

   type Implicit_Formal_Type_Declaration is
     new Program.Nodes.Node
         and Program.Elements.Formal_Type_Declarations.Formal_Type_Declaration
     with private;

   function Create
    (Name                 : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Discriminant_Part    : Program.Elements.Definitions.Definition_Access;
     Definition           : not null Program.Elements.Formal_Type_Definitions
         .Formal_Type_Definition_Access;
     Aspects              : Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Formal_Type_Declaration
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Formal_Type_Declaration is
     abstract new Program.Nodes.Node
       and Program.Elements.Formal_Type_Declarations.Formal_Type_Declaration
     with record
        Name              : not null Program.Elements.Defining_Identifiers
          .Defining_Identifier_Access;
        Discriminant_Part : Program.Elements.Definitions.Definition_Access;
        Definition        : not null Program.Elements.Formal_Type_Definitions
          .Formal_Type_Definition_Access;
        Aspects           : Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access;
     end record;

   procedure Initialize
    (Self : aliased in out Base_Formal_Type_Declaration'Class);

   overriding procedure Visit
    (Self    : not null access Base_Formal_Type_Declaration;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Name
    (Self : Base_Formal_Type_Declaration)
      return not null Program.Elements.Defining_Identifiers
          .Defining_Identifier_Access;

   overriding function Discriminant_Part
    (Self : Base_Formal_Type_Declaration)
      return Program.Elements.Definitions.Definition_Access;

   overriding function Definition
    (Self : Base_Formal_Type_Declaration)
      return not null Program.Elements.Formal_Type_Definitions
          .Formal_Type_Definition_Access;

   overriding function Aspects
    (Self : Base_Formal_Type_Declaration)
      return Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access;

   overriding function Is_Formal_Type_Declaration_Element
    (Self : Base_Formal_Type_Declaration)
      return Boolean;

   overriding function Is_Declaration_Element
    (Self : Base_Formal_Type_Declaration)
      return Boolean;

   type Formal_Type_Declaration is
     new Base_Formal_Type_Declaration
       and Program.Elements.Formal_Type_Declarations
         .Formal_Type_Declaration_Text
     with record
        Type_Token      : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Is_Token        : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        With_Token      : Program.Lexical_Elements.Lexical_Element_Access;
        Semicolon_Token : not null Program.Lexical_Elements
          .Lexical_Element_Access;
     end record;

   overriding function To_Formal_Type_Declaration_Text
    (Self : aliased in out Formal_Type_Declaration)
      return Program.Elements.Formal_Type_Declarations
          .Formal_Type_Declaration_Text_Access;

   overriding function Type_Token
    (Self : Formal_Type_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Is_Token
    (Self : Formal_Type_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function With_Token
    (Self : Formal_Type_Declaration)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Semicolon_Token
    (Self : Formal_Type_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   type Implicit_Formal_Type_Declaration is
     new Base_Formal_Type_Declaration
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
     end record;

   overriding function To_Formal_Type_Declaration_Text
    (Self : aliased in out Implicit_Formal_Type_Declaration)
      return Program.Elements.Formal_Type_Declarations
          .Formal_Type_Declaration_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Formal_Type_Declaration)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Formal_Type_Declaration)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Formal_Type_Declaration)
      return Boolean;

end Program.Nodes.Formal_Type_Declarations;
