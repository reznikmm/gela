--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Declarations;
with Program.Tokens;
with Program.Elements.Defining_Identifiers;
with Program.Elements.Known_Discriminant_Parts;
with Program.Elements.Aspect_Specifications;
with Program.Elements.Expressions;
with Program.Elements.Protected_Definitions;

package Program.Elements.Protected_Type_Declarations is

   pragma Pure (Program.Elements.Protected_Type_Declarations);

   type Protected_Type_Declaration is
     limited interface and Program.Elements.Declarations.Declaration;

   type Protected_Type_Declaration_Access is
     access all Protected_Type_Declaration'Class with Storage_Size => 0;

   not overriding function Protected_Token
    (Self : Protected_Type_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Type_Token
    (Self : Protected_Type_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Name
    (Self : Protected_Type_Declaration)
      return not null Program.Elements.Defining_Identifiers
          .Defining_Identifier_Access is abstract;

   not overriding function Discriminant_Part
    (Self : Protected_Type_Declaration)
      return Program.Elements.Known_Discriminant_Parts
          .Known_Discriminant_Part_Access is abstract;

   not overriding function With_Token
    (Self : Protected_Type_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Aspects
    (Self : Protected_Type_Declaration)
      return not null Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access is abstract;

   not overriding function Is_Token
    (Self : Protected_Type_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function New_Token
    (Self : Protected_Type_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Progenitors
    (Self : Protected_Type_Declaration)
      return not null Program.Elements.Expressions.Expression_Vector_Access
     is abstract;

   not overriding function With_Token_2
    (Self : Protected_Type_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Definition
    (Self : Protected_Type_Declaration)
      return not null Program.Elements.Protected_Definitions
          .Protected_Definition_Access is abstract;

   not overriding function Semicolon_Token
    (Self : Protected_Type_Declaration)
      return Program.Tokens.Token_Access is abstract;

end Program.Elements.Protected_Type_Declarations;
