--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Declarations;
with Program.Tokens;
with Program.Elements.Expressions;

package Program.Elements.Parameter_Specifications is

   pragma Pure (Program.Elements.Parameter_Specifications);

   type Parameter_Specification is
     limited interface and Program.Elements.Declarations.Declaration;

   type Parameter_Specification_Access is
     access all Parameter_Specification'Class with Storage_Size => 0;

   not overriding function Colon_Token
    (Self : Parameter_Specification)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Aliased_Token
    (Self : Parameter_Specification)
      return Program.Tokens.Token_Access is abstract;

   not overriding function In_Token
    (Self : Parameter_Specification)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Out_Token
    (Self : Parameter_Specification)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Not_Token
    (Self : Parameter_Specification)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Null_Token
    (Self : Parameter_Specification)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Object_Subtype
    (Self : Parameter_Specification)
      return Program.Elements.Element_Access is abstract;

   not overriding function Assignment_Token
    (Self : Parameter_Specification)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Initialization_Expression
    (Self : Parameter_Specification)
      return Program.Elements.Expressions.Expression_Access is abstract;

end Program.Elements.Parameter_Specifications;
