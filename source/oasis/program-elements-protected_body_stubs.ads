--  SPDX-FileCopyrightText: 2019 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Program.Elements.Declarations;
with Program.Lexical_Elements;
with Program.Elements.Defining_Identifiers;
with Program.Elements.Aspect_Specifications;

package Program.Elements.Protected_Body_Stubs is

   pragma Pure (Program.Elements.Protected_Body_Stubs);

   type Protected_Body_Stub is
     limited interface and Program.Elements.Declarations.Declaration;

   type Protected_Body_Stub_Access is access all Protected_Body_Stub'Class
     with Storage_Size => 0;

   not overriding function Name
    (Self : Protected_Body_Stub)
      return not null Program.Elements.Defining_Identifiers
          .Defining_Identifier_Access is abstract;

   not overriding function Aspects
    (Self : Protected_Body_Stub)
      return Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access is abstract;

   type Protected_Body_Stub_Text is limited interface;

   type Protected_Body_Stub_Text_Access is
     access all Protected_Body_Stub_Text'Class with Storage_Size => 0;

   not overriding function To_Protected_Body_Stub_Text
    (Self : in out Protected_Body_Stub)
      return Protected_Body_Stub_Text_Access is abstract;

   not overriding function Protected_Token
    (Self : Protected_Body_Stub_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Body_Token
    (Self : Protected_Body_Stub_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Is_Token
    (Self : Protected_Body_Stub_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Separate_Token
    (Self : Protected_Body_Stub_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function With_Token
    (Self : Protected_Body_Stub_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Semicolon_Token
    (Self : Protected_Body_Stub_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

end Program.Elements.Protected_Body_Stubs;
