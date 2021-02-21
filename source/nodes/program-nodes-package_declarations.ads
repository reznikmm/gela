--  SPDX-FileCopyrightText: 2019 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Program.Lexical_Elements;
with Program.Elements.Defining_Names;
with Program.Elements.Aspect_Specifications;
with Program.Element_Vectors;
with Program.Elements.Expressions;
with Program.Elements.Package_Declarations;
with Program.Element_Visitors;

package Program.Nodes.Package_Declarations is

   pragma Preelaborate;

   type Package_Declaration is
     new Program.Nodes.Node
         and Program.Elements.Package_Declarations.Package_Declaration
         and Program.Elements.Package_Declarations.Package_Declaration_Text
     with private;

   function Create
    (Package_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name                 : not null Program.Elements.Defining_Names
         .Defining_Name_Access;
     With_Token           : Program.Lexical_Elements.Lexical_Element_Access;
     Aspects              : Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Is_Token             : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Visible_Declarations : Program.Element_Vectors.Element_Vector_Access;
     Private_Token        : Program.Lexical_Elements.Lexical_Element_Access;
     Private_Declarations : Program.Element_Vectors.Element_Vector_Access;
     End_Token            : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     End_Name             : Program.Elements.Expressions.Expression_Access;
     Semicolon_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Package_Declaration;

   type Implicit_Package_Declaration is
     new Program.Nodes.Node
         and Program.Elements.Package_Declarations.Package_Declaration
     with private;

   function Create
    (Name                 : not null Program.Elements.Defining_Names
         .Defining_Name_Access;
     Aspects              : Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Visible_Declarations : Program.Element_Vectors.Element_Vector_Access;
     Private_Declarations : Program.Element_Vectors.Element_Vector_Access;
     End_Name             : Program.Elements.Expressions.Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Package_Declaration
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Package_Declaration is
     abstract new Program.Nodes.Node
       and Program.Elements.Package_Declarations.Package_Declaration
     with record
        Name                 : not null Program.Elements.Defining_Names
          .Defining_Name_Access;
        Aspects              : Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access;
        Visible_Declarations : Program.Element_Vectors.Element_Vector_Access;
        Private_Declarations : Program.Element_Vectors.Element_Vector_Access;
        End_Name             : Program.Elements.Expressions.Expression_Access;
     end record;

   procedure Initialize (Self : in out Base_Package_Declaration'Class);

   overriding procedure Visit
    (Self    : not null access Base_Package_Declaration;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Name
    (Self : Base_Package_Declaration)
      return not null Program.Elements.Defining_Names.Defining_Name_Access;

   overriding function Aspects
    (Self : Base_Package_Declaration)
      return Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access;

   overriding function Visible_Declarations
    (Self : Base_Package_Declaration)
      return Program.Element_Vectors.Element_Vector_Access;

   overriding function Private_Declarations
    (Self : Base_Package_Declaration)
      return Program.Element_Vectors.Element_Vector_Access;

   overriding function End_Name
    (Self : Base_Package_Declaration)
      return Program.Elements.Expressions.Expression_Access;

   overriding function Is_Package_Declaration
    (Self : Base_Package_Declaration)
      return Boolean;

   overriding function Is_Declaration
    (Self : Base_Package_Declaration)
      return Boolean;

   type Package_Declaration is
     new Base_Package_Declaration
       and Program.Elements.Package_Declarations.Package_Declaration_Text
     with record
        Package_Token   : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        With_Token      : Program.Lexical_Elements.Lexical_Element_Access;
        Is_Token        : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Private_Token   : Program.Lexical_Elements.Lexical_Element_Access;
        End_Token       : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Semicolon_Token : not null Program.Lexical_Elements
          .Lexical_Element_Access;
     end record;

   overriding function To_Package_Declaration_Text
    (Self : in out Package_Declaration)
      return Program.Elements.Package_Declarations
          .Package_Declaration_Text_Access;

   overriding function Package_Token
    (Self : Package_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function With_Token
    (Self : Package_Declaration)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Is_Token
    (Self : Package_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Private_Token
    (Self : Package_Declaration)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function End_Token
    (Self : Package_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Semicolon_Token
    (Self : Package_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   type Implicit_Package_Declaration is
     new Base_Package_Declaration
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
     end record;

   overriding function To_Package_Declaration_Text
    (Self : in out Implicit_Package_Declaration)
      return Program.Elements.Package_Declarations
          .Package_Declaration_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Package_Declaration)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Package_Declaration)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Package_Declaration)
      return Boolean;

end Program.Nodes.Package_Declarations;
