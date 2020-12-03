--  SPDX-FileCopyrightText: 2019 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Program.Elements.Declarations;
with Program.Lexical_Elements;
with Program.Elements.Defining_Identifiers;
with Program.Elements.Aspect_Specifications;
with Program.Elements.Expressions;
with Program.Elements.Protected_Definitions;

package Program.Elements.Single_Protected_Declarations is

   pragma Pure (Program.Elements.Single_Protected_Declarations);

   type Single_Protected_Declaration is
     limited interface and Program.Elements.Declarations.Declaration;

   type Single_Protected_Declaration_Access is
     access all Single_Protected_Declaration'Class with Storage_Size => 0;

   not overriding function Name
    (Self : Single_Protected_Declaration)
      return not null Program.Elements.Defining_Identifiers
          .Defining_Identifier_Access is abstract;

   not overriding function Aspects
    (Self : Single_Protected_Declaration)
      return Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access is abstract;

   not overriding function Progenitors
    (Self : Single_Protected_Declaration)
      return Program.Elements.Expressions.Expression_Vector_Access is abstract;

   not overriding function Definition
    (Self : Single_Protected_Declaration)
      return not null Program.Elements.Protected_Definitions
          .Protected_Definition_Access is abstract;

   type Single_Protected_Declaration_Text is limited interface;

   type Single_Protected_Declaration_Text_Access is
     access all Single_Protected_Declaration_Text'Class with Storage_Size => 0;

   not overriding function To_Single_Protected_Declaration_Text
    (Self : aliased in out Single_Protected_Declaration)
      return Single_Protected_Declaration_Text_Access is abstract;

   not overriding function Protected_Token
    (Self : Single_Protected_Declaration_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function With_Token
    (Self : Single_Protected_Declaration_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Is_Token
    (Self : Single_Protected_Declaration_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function New_Token
    (Self : Single_Protected_Declaration_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function With_Token_2
    (Self : Single_Protected_Declaration_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Semicolon_Token
    (Self : Single_Protected_Declaration_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

end Program.Elements.Single_Protected_Declarations;
