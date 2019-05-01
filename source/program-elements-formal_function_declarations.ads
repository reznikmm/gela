--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Declarations;
with Program.Tokens;
with Program.Elements.Defining_Names;
with Program.Elements.Expressions;

package Program.Elements.Formal_Function_Declarations is

   pragma Pure (Program.Elements.Formal_Function_Declarations);

   type Formal_Function_Declaration is
     limited interface and Program.Elements.Declarations.Declaration;

   type Formal_Function_Declaration_Access is
     access all Formal_Function_Declaration'Class with Storage_Size => 0;

   not overriding function With_Token
    (Self : Formal_Function_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Function_Token
    (Self : Formal_Function_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Name
    (Self : Formal_Function_Declaration)
      return Program.Elements.Defining_Names.Defining_Name_Access is abstract;

   not overriding function Left_Bracket_Token
    (Self : Formal_Function_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Right_Bracket_Token
    (Self : Formal_Function_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Return_Token
    (Self : Formal_Function_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Not_Token
    (Self : Formal_Function_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Null_Token
    (Self : Formal_Function_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Result_Subtype
    (Self : Formal_Function_Declaration)
      return Program.Elements.Element_Access is abstract;

   not overriding function Is_Token
    (Self : Formal_Function_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Abstract_Token
    (Self : Formal_Function_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Formal_Subprogram_Default
    (Self : Formal_Function_Declaration)
      return Program.Elements.Expressions.Expression_Access is abstract;

   not overriding function With_Token_2
    (Self : Formal_Function_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Semicolon_Token
    (Self : Formal_Function_Declaration)
      return Program.Tokens.Token_Access is abstract;

end Program.Elements.Formal_Function_Declarations;
