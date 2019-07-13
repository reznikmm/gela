--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Declarations;
with Program.Lexical_Elements;
with Program.Elements.Defining_Identifiers;
with Program.Elements.Aspect_Specifications;
with Program.Element_Vectors;
with Program.Elements.Identifiers;

package Program.Elements.Protected_Body_Declarations is

   pragma Pure (Program.Elements.Protected_Body_Declarations);

   type Protected_Body_Declaration is
     limited interface and Program.Elements.Declarations.Declaration;

   type Protected_Body_Declaration_Access is
     access all Protected_Body_Declaration'Class with Storage_Size => 0;

   not overriding function Name
    (Self : Protected_Body_Declaration)
      return not null Program.Elements.Defining_Identifiers
          .Defining_Identifier_Access is abstract;

   not overriding function Aspects
    (Self : Protected_Body_Declaration)
      return not null Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access is abstract;

   not overriding function Protected_Operations
    (Self : Protected_Body_Declaration)
      return not null Program.Element_Vectors.Element_Vector_Access
     is abstract;

   not overriding function End_Name
    (Self : Protected_Body_Declaration)
      return Program.Elements.Identifiers.Identifier_Access is abstract;

   type Protected_Body_Declaration_Text is limited interface;

   type Protected_Body_Declaration_Text_Access is
     access all Protected_Body_Declaration_Text'Class with Storage_Size => 0;

   not overriding function To_Protected_Body_Declaration_Text
    (Self : aliased in out Protected_Body_Declaration)
      return Protected_Body_Declaration_Text_Access is abstract;

   not overriding function Protected_Token
    (Self : Protected_Body_Declaration_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Body_Token
    (Self : Protected_Body_Declaration_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function With_Token
    (Self : Protected_Body_Declaration_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Is_Token
    (Self : Protected_Body_Declaration_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function End_Token
    (Self : Protected_Body_Declaration_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Semicolon_Token
    (Self : Protected_Body_Declaration_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

end Program.Elements.Protected_Body_Declarations;
