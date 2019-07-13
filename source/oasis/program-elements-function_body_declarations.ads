--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Declarations;
with Program.Lexical_Elements;
with Program.Elements.Defining_Names;
with Program.Elements.Parameter_Specifications;
with Program.Elements.Aspect_Specifications;
with Program.Element_Vectors;
with Program.Elements.Exception_Handlers;
with Program.Elements.Expressions;

package Program.Elements.Function_Body_Declarations is

   pragma Pure (Program.Elements.Function_Body_Declarations);

   type Function_Body_Declaration is
     limited interface and Program.Elements.Declarations.Declaration;

   type Function_Body_Declaration_Access is
     access all Function_Body_Declaration'Class with Storage_Size => 0;

   not overriding function Name
    (Self : Function_Body_Declaration)
      return not null Program.Elements.Defining_Names.Defining_Name_Access
     is abstract;

   not overriding function Parameters
    (Self : Function_Body_Declaration)
      return not null Program.Elements.Parameter_Specifications
          .Parameter_Specification_Vector_Access is abstract;

   not overriding function Result_Subtype
    (Self : Function_Body_Declaration)
      return not null Program.Elements.Element_Access is abstract;

   not overriding function Aspects
    (Self : Function_Body_Declaration)
      return not null Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access is abstract;

   not overriding function Declarations
    (Self : Function_Body_Declaration)
      return not null Program.Element_Vectors.Element_Vector_Access
     is abstract;

   not overriding function Statements
    (Self : Function_Body_Declaration)
      return not null Program.Element_Vectors.Element_Vector_Access
     is abstract;

   not overriding function Exception_Handlers
    (Self : Function_Body_Declaration)
      return not null Program.Elements.Exception_Handlers
          .Exception_Handler_Vector_Access is abstract;

   not overriding function End_Name
    (Self : Function_Body_Declaration)
      return Program.Elements.Expressions.Expression_Access is abstract;

   not overriding function Has_Not
    (Self : Function_Body_Declaration)
      return Boolean is abstract;

   not overriding function Has_Overriding
    (Self : Function_Body_Declaration)
      return Boolean is abstract;

   not overriding function Has_Not_Null
    (Self : Function_Body_Declaration)
      return Boolean is abstract;

   type Function_Body_Declaration_Text is limited interface;

   type Function_Body_Declaration_Text_Access is
     access all Function_Body_Declaration_Text'Class with Storage_Size => 0;

   not overriding function To_Function_Body_Declaration_Text
    (Self : aliased Function_Body_Declaration)
      return Function_Body_Declaration_Text_Access is abstract;

   not overriding function Not_Token
    (Self : Function_Body_Declaration_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Overriding_Token
    (Self : Function_Body_Declaration_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Function_Token
    (Self : Function_Body_Declaration_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Left_Bracket_Token
    (Self : Function_Body_Declaration_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Right_Bracket_Token
    (Self : Function_Body_Declaration_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Return_Token
    (Self : Function_Body_Declaration_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Not_Token_2
    (Self : Function_Body_Declaration_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Null_Token
    (Self : Function_Body_Declaration_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function With_Token
    (Self : Function_Body_Declaration_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Is_Token
    (Self : Function_Body_Declaration_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Begin_Token
    (Self : Function_Body_Declaration_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Exception_Token
    (Self : Function_Body_Declaration_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function End_Token
    (Self : Function_Body_Declaration_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Semicolon_Token
    (Self : Function_Body_Declaration_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

end Program.Elements.Function_Body_Declarations;
