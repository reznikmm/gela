--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Lexical_Elements;
with Program.Elements.Defining_Identifiers;
with Program.Elements.Parameter_Specifications;
with Program.Elements.Aspect_Specifications;
with Program.Elements.Procedure_Body_Stubs;
with Program.Element_Visitors;

package Program.Nodes.Procedure_Body_Stubs is

   pragma Preelaborate;

   type Procedure_Body_Stub is
     new Program.Nodes.Node
         and Program.Elements.Procedure_Body_Stubs.Procedure_Body_Stub
         and Program.Elements.Procedure_Body_Stubs.Procedure_Body_Stub_Text
     with private;

   function Create
    (Not_Token           : Program.Lexical_Elements.Lexical_Element_Access;
     Overriding_Token    : Program.Lexical_Elements.Lexical_Element_Access;
     Procedure_Token     : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name                : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Left_Bracket_Token  : Program.Lexical_Elements.Lexical_Element_Access;
     Parameters          : Program.Elements.Parameter_Specifications
         .Parameter_Specification_Vector_Access;
     Right_Bracket_Token : Program.Lexical_Elements.Lexical_Element_Access;
     Is_Token            : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Separate_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     With_Token          : Program.Lexical_Elements.Lexical_Element_Access;
     Aspects             : Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Semicolon_Token     : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Procedure_Body_Stub;

   type Implicit_Procedure_Body_Stub is
     new Program.Nodes.Node
         and Program.Elements.Procedure_Body_Stubs.Procedure_Body_Stub
     with private;

   function Create
    (Name                 : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Parameters           : Program.Elements.Parameter_Specifications
         .Parameter_Specification_Vector_Access;
     Aspects              : Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False;
     Has_Not              : Boolean := False;
     Has_Overriding       : Boolean := False)
      return Implicit_Procedure_Body_Stub
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Procedure_Body_Stub is
     abstract new Program.Nodes.Node
       and Program.Elements.Procedure_Body_Stubs.Procedure_Body_Stub
     with record
        Name       : not null Program.Elements.Defining_Identifiers
          .Defining_Identifier_Access;
        Parameters : Program.Elements.Parameter_Specifications
          .Parameter_Specification_Vector_Access;
        Aspects    : Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access;
     end record;

   procedure Initialize (Self : aliased in out Base_Procedure_Body_Stub'Class);

   overriding procedure Visit
    (Self    : not null access Base_Procedure_Body_Stub;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Name
    (Self : Base_Procedure_Body_Stub)
      return not null Program.Elements.Defining_Identifiers
          .Defining_Identifier_Access;

   overriding function Parameters
    (Self : Base_Procedure_Body_Stub)
      return Program.Elements.Parameter_Specifications
          .Parameter_Specification_Vector_Access;

   overriding function Aspects
    (Self : Base_Procedure_Body_Stub)
      return Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access;

   overriding function Is_Procedure_Body_Stub_Element
    (Self : Base_Procedure_Body_Stub)
      return Boolean;

   overriding function Is_Declaration_Element
    (Self : Base_Procedure_Body_Stub)
      return Boolean;

   type Procedure_Body_Stub is
     new Base_Procedure_Body_Stub
       and Program.Elements.Procedure_Body_Stubs.Procedure_Body_Stub_Text
     with record
        Not_Token           : Program.Lexical_Elements.Lexical_Element_Access;
        Overriding_Token    : Program.Lexical_Elements.Lexical_Element_Access;
        Procedure_Token     : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Left_Bracket_Token  : Program.Lexical_Elements.Lexical_Element_Access;
        Right_Bracket_Token : Program.Lexical_Elements.Lexical_Element_Access;
        Is_Token            : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        Separate_Token      : not null Program.Lexical_Elements
          .Lexical_Element_Access;
        With_Token          : Program.Lexical_Elements.Lexical_Element_Access;
        Semicolon_Token     : not null Program.Lexical_Elements
          .Lexical_Element_Access;
     end record;

   overriding function To_Procedure_Body_Stub_Text
    (Self : aliased in out Procedure_Body_Stub)
      return Program.Elements.Procedure_Body_Stubs
          .Procedure_Body_Stub_Text_Access;

   overriding function Not_Token
    (Self : Procedure_Body_Stub)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Overriding_Token
    (Self : Procedure_Body_Stub)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Procedure_Token
    (Self : Procedure_Body_Stub)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Left_Bracket_Token
    (Self : Procedure_Body_Stub)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Right_Bracket_Token
    (Self : Procedure_Body_Stub)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Is_Token
    (Self : Procedure_Body_Stub)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Separate_Token
    (Self : Procedure_Body_Stub)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function With_Token
    (Self : Procedure_Body_Stub)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Semicolon_Token
    (Self : Procedure_Body_Stub)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Has_Not (Self : Procedure_Body_Stub) return Boolean;

   overriding function Has_Overriding
    (Self : Procedure_Body_Stub)
      return Boolean;

   type Implicit_Procedure_Body_Stub is
     new Base_Procedure_Body_Stub
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
        Has_Not              : Boolean;
        Has_Overriding       : Boolean;
     end record;

   overriding function To_Procedure_Body_Stub_Text
    (Self : aliased in out Implicit_Procedure_Body_Stub)
      return Program.Elements.Procedure_Body_Stubs
          .Procedure_Body_Stub_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Procedure_Body_Stub)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Procedure_Body_Stub)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Procedure_Body_Stub)
      return Boolean;

   overriding function Has_Not
    (Self : Implicit_Procedure_Body_Stub)
      return Boolean;

   overriding function Has_Overriding
    (Self : Implicit_Procedure_Body_Stub)
      return Boolean;

end Program.Nodes.Procedure_Body_Stubs;
