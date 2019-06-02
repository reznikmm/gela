--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Declarations;
with Program.Tokens;
with Program.Elements.Expressions;

package Program.Elements.Discriminant_Specifications is

   pragma Pure (Program.Elements.Discriminant_Specifications);

   type Discriminant_Specification is
     limited interface and Program.Elements.Declarations.Declaration;

   type Discriminant_Specification_Access is
     access all Discriminant_Specification'Class with Storage_Size => 0;

   not overriding function Colon_Token
    (Self : Discriminant_Specification)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Not_Token
    (Self : Discriminant_Specification)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Null_Token
    (Self : Discriminant_Specification)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Object_Subtype
    (Self : Discriminant_Specification)
      return Program.Elements.Element_Access is abstract;

   not overriding function Assignment_Token
    (Self : Discriminant_Specification)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Default_Expression
    (Self : Discriminant_Specification)
      return Program.Elements.Expressions.Expression_Access is abstract;

   not overriding function Semicolon_Token
    (Self : Discriminant_Specification)
      return Program.Tokens.Token_Access is abstract;

end Program.Elements.Discriminant_Specifications;
