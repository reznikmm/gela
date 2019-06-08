--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Declarations;
with Program.Tokens;
with Program.Elements.Defining_Names;
with Program.Elements.Parameter_Specifications;
with Program.Elements.Parenthesized_Expressions;
with Program.Elements.Aspect_Specifications;

package Program.Elements.Function_Declarations is

   pragma Pure (Program.Elements.Function_Declarations);

   type Function_Declaration is
     limited interface and Program.Elements.Declarations.Declaration;

   type Function_Declaration_Access is access all Function_Declaration'Class
     with Storage_Size => 0;

   not overriding function Not_Token
    (Self : Function_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Overriding_Token
    (Self : Function_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Function_Token
    (Self : Function_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Name
    (Self : Function_Declaration)
      return not null Program.Elements.Defining_Names.Defining_Name_Access
     is abstract;

   not overriding function Left_Bracket_Token
    (Self : Function_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Parameters
    (Self : Function_Declaration)
      return not null Program.Elements.Parameter_Specifications
          .Parameter_Specification_Vector_Access is abstract;

   not overriding function Right_Bracket_Token
    (Self : Function_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Return_Token
    (Self : Function_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Not_Token_2
    (Self : Function_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Null_Token
    (Self : Function_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Result_Subtype
    (Self : Function_Declaration)
      return not null Program.Elements.Element_Access is abstract;

   not overriding function Is_Token
    (Self : Function_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Result_Expression
    (Self : Function_Declaration)
      return Program.Elements.Parenthesized_Expressions
          .Parenthesized_Expression_Access is abstract;

   not overriding function Abstract_Token
    (Self : Function_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function With_Token
    (Self : Function_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Aspects
    (Self : Function_Declaration)
      return not null Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access is abstract;

   not overriding function Semicolon_Token
    (Self : Function_Declaration)
      return Program.Tokens.Token_Access is abstract;

end Program.Elements.Function_Declarations;
