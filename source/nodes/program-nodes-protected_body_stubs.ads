--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Lexical_Elements;
with Program.Elements.Defining_Identifiers;
with Program.Elements.Aspect_Specifications;
with Program.Elements.Protected_Body_Stubs;
with Program.Element_Visitors;

package Program.Nodes.Protected_Body_Stubs is

   pragma Pure (Program.Nodes.Protected_Body_Stubs);

   type Protected_Body_Stub is
     new Program.Nodes.Node
         and Program.Elements.Protected_Body_Stubs.Protected_Body_Stub
         and Program.Elements.Protected_Body_Stubs.Protected_Body_Stub_Text
     with private;

   function Create
    (Protected_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Body_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name            : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Is_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Separate_Token  : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     With_Token      : Program.Lexical_Elements.Lexical_Element_Access;
     Aspects         : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Semicolon_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Protected_Body_Stub;

   type Implicit_Protected_Body_Stub is
     new Program.Nodes.Node
         and Program.Elements.Protected_Body_Stubs.Protected_Body_Stub
     with private;

   function Create
    (Name                 : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Aspects              : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Protected_Body_Stub
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Protected_Body_Stub is
     abstract new Program.Nodes.Node
       and Program.Elements.Protected_Body_Stubs.Protected_Body_Stub
     with record
        Name    : not null Program.Elements.Defining_Identifiers
          .Defining_Identifier_Access;
        Aspects : not null Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access;
     end record;

   procedure Initialize (Self : aliased in out Base_Protected_Body_Stub'Class);

   overriding procedure Visit
    (Self    : not null access Base_Protected_Body_Stub;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Name
    (Self : Base_Protected_Body_Stub)
      return not null Program.Elements.Defining_Identifiers
          .Defining_Identifier_Access;

   overriding function Aspects
    (Self : Base_Protected_Body_Stub)
      return not null Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access;

   overriding function Is_Protected_Body_Stub
    (Self : Base_Protected_Body_Stub)
      return Boolean;

   overriding function Is_Declaration
    (Self : Base_Protected_Body_Stub)
      return Boolean;

   type Protected_Body_Stub is
     new Base_Protected_Body_Stub
       and Program.Elements.Protected_Body_Stubs.Protected_Body_Stub_Text
     with record
        Protected_Token : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Body_Token      : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Is_Token        : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Separate_Token  : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        With_Token      : Program.Lexical_Elements.Lexical_Element_Access;
        Semicolon_Token : not null Program.Lexical_Elements
          .Lexical_Element_Access;
     end record;

   overriding function To_Protected_Body_Stub_Text
    (Self : aliased in out Protected_Body_Stub)
      return Program.Elements.Protected_Body_Stubs
          .Protected_Body_Stub_Text_Access;

   overriding function Protected_Token
    (Self : Protected_Body_Stub)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Body_Token
    (Self : Protected_Body_Stub)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Is_Token
    (Self : Protected_Body_Stub)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Separate_Token
    (Self : Protected_Body_Stub)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function With_Token
    (Self : Protected_Body_Stub)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Semicolon_Token
    (Self : Protected_Body_Stub)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   type Implicit_Protected_Body_Stub is
     new Base_Protected_Body_Stub
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
     end record;

   overriding function To_Protected_Body_Stub_Text
    (Self : aliased in out Implicit_Protected_Body_Stub)
      return Program.Elements.Protected_Body_Stubs
          .Protected_Body_Stub_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Protected_Body_Stub)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Protected_Body_Stub)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Protected_Body_Stub)
      return Boolean;

end Program.Nodes.Protected_Body_Stubs;
