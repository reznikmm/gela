--  SPDX-FileCopyrightText: 2019 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Program.Elements.Declarations;
with Program.Lexical_Elements;
with Program.Elements.Defining_Identifiers;
with Program.Elements.Aspect_Specifications;

package Program.Elements.Package_Body_Stubs is

   pragma Pure (Program.Elements.Package_Body_Stubs);

   type Package_Body_Stub is
     limited interface and Program.Elements.Declarations.Declaration;

   type Package_Body_Stub_Access is access all Package_Body_Stub'Class
     with Storage_Size => 0;

   not overriding function Name
    (Self : Package_Body_Stub)
      return not null Program.Elements.Defining_Identifiers
          .Defining_Identifier_Access is abstract;

   not overriding function Aspects
    (Self : Package_Body_Stub)
      return Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access is abstract;

   type Package_Body_Stub_Text is limited interface;

   type Package_Body_Stub_Text_Access is
     access all Package_Body_Stub_Text'Class with Storage_Size => 0;

   not overriding function To_Package_Body_Stub_Text
    (Self : aliased in out Package_Body_Stub)
      return Package_Body_Stub_Text_Access is abstract;

   not overriding function Package_Token
    (Self : Package_Body_Stub_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Body_Token
    (Self : Package_Body_Stub_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Is_Token
    (Self : Package_Body_Stub_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Separate_Token
    (Self : Package_Body_Stub_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function With_Token
    (Self : Package_Body_Stub_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Semicolon_Token
    (Self : Package_Body_Stub_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

end Program.Elements.Package_Body_Stubs;
