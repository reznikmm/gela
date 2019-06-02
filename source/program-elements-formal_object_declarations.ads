--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Declarations;
with Program.Tokens;
with Program.Elements.Expressions;

package Program.Elements.Formal_Object_Declarations is

   pragma Pure (Program.Elements.Formal_Object_Declarations);

   type Formal_Object_Declaration is
     limited interface and Program.Elements.Declarations.Declaration;

   type Formal_Object_Declaration_Access is
     access all Formal_Object_Declaration'Class with Storage_Size => 0;

   not overriding function Colon_Token
    (Self : Formal_Object_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function In_Token
    (Self : Formal_Object_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Out_Token
    (Self : Formal_Object_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Not_Token
    (Self : Formal_Object_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Null_Token
    (Self : Formal_Object_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Object_Subtype
    (Self : Formal_Object_Declaration)
      return Program.Elements.Element_Access is abstract;

   not overriding function Assignment_Token
    (Self : Formal_Object_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Default_Expression
    (Self : Formal_Object_Declaration)
      return Program.Elements.Expressions.Expression_Access is abstract;

   not overriding function With_Token
    (Self : Formal_Object_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Semicolon_Token
    (Self : Formal_Object_Declaration)
      return Program.Tokens.Token_Access is abstract;

end Program.Elements.Formal_Object_Declarations;
