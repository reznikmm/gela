--  SPDX-FileCopyrightText: 2019 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Program.Elements.Declarations;
with Program.Lexical_Elements;
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

   not overriding function Name
    (Self : Function_Declaration)
      return not null Program.Elements.Defining_Names.Defining_Name_Access
     is abstract;

   not overriding function Parameters
    (Self : Function_Declaration)
      return Program.Elements.Parameter_Specifications
          .Parameter_Specification_Vector_Access is abstract;

   not overriding function Result_Subtype
    (Self : Function_Declaration)
      return not null Program.Elements.Element_Access is abstract;

   not overriding function Result_Expression
    (Self : Function_Declaration)
      return Program.Elements.Parenthesized_Expressions
          .Parenthesized_Expression_Access is abstract;

   not overriding function Aspects
    (Self : Function_Declaration)
      return Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access is abstract;

   not overriding function Has_Not (Self : Function_Declaration) return Boolean
     is abstract;

   not overriding function Has_Overriding
    (Self : Function_Declaration)
      return Boolean is abstract;

   not overriding function Has_Abstract
    (Self : Function_Declaration)
      return Boolean is abstract;

   not overriding function Has_Not_Null
    (Self : Function_Declaration)
      return Boolean is abstract;

   type Function_Declaration_Text is limited interface;

   type Function_Declaration_Text_Access is
     access all Function_Declaration_Text'Class with Storage_Size => 0;

   not overriding function To_Function_Declaration_Text
    (Self : aliased in out Function_Declaration)
      return Function_Declaration_Text_Access is abstract;

   not overriding function Not_Token
    (Self : Function_Declaration_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Overriding_Token
    (Self : Function_Declaration_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Function_Token
    (Self : Function_Declaration_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Left_Bracket_Token
    (Self : Function_Declaration_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Right_Bracket_Token
    (Self : Function_Declaration_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Return_Token
    (Self : Function_Declaration_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Not_Token_2
    (Self : Function_Declaration_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Null_Token
    (Self : Function_Declaration_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Is_Token
    (Self : Function_Declaration_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Abstract_Token
    (Self : Function_Declaration_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function With_Token
    (Self : Function_Declaration_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Semicolon_Token
    (Self : Function_Declaration_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

end Program.Elements.Function_Declarations;
