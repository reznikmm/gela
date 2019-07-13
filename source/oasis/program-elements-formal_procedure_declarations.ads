--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Declarations;
with Program.Lexical_Elements;
with Program.Elements.Defining_Identifiers;
with Program.Elements.Parameter_Specifications;
with Program.Elements.Expressions;
with Program.Elements.Aspect_Specifications;

package Program.Elements.Formal_Procedure_Declarations is

   pragma Pure (Program.Elements.Formal_Procedure_Declarations);

   type Formal_Procedure_Declaration is
     limited interface and Program.Elements.Declarations.Declaration;

   type Formal_Procedure_Declaration_Access is
     access all Formal_Procedure_Declaration'Class with Storage_Size => 0;

   not overriding function Name
    (Self : Formal_Procedure_Declaration)
      return not null Program.Elements.Defining_Identifiers
          .Defining_Identifier_Access is abstract;

   not overriding function Parameters
    (Self : Formal_Procedure_Declaration)
      return not null Program.Elements.Parameter_Specifications
          .Parameter_Specification_Vector_Access is abstract;

   not overriding function Subprogram_Default
    (Self : Formal_Procedure_Declaration)
      return Program.Elements.Expressions.Expression_Access is abstract;

   not overriding function Aspects
    (Self : Formal_Procedure_Declaration)
      return not null Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access is abstract;

   not overriding function Has_Abstract
    (Self : Formal_Procedure_Declaration)
      return Boolean is abstract;

   not overriding function Has_Null
    (Self : Formal_Procedure_Declaration)
      return Boolean is abstract;

   not overriding function Has_Box
    (Self : Formal_Procedure_Declaration)
      return Boolean is abstract;

   type Formal_Procedure_Declaration_Text is limited interface;

   type Formal_Procedure_Declaration_Text_Access is
     access all Formal_Procedure_Declaration_Text'Class with Storage_Size => 0;

   not overriding function To_Formal_Procedure_Declaration_Text
    (Self : aliased Formal_Procedure_Declaration)
      return Formal_Procedure_Declaration_Text_Access is abstract;

   not overriding function With_Token
    (Self : Formal_Procedure_Declaration_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Procedure_Token
    (Self : Formal_Procedure_Declaration_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Left_Bracket_Token
    (Self : Formal_Procedure_Declaration_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Right_Bracket_Token
    (Self : Formal_Procedure_Declaration_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Is_Token
    (Self : Formal_Procedure_Declaration_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Abstract_Token
    (Self : Formal_Procedure_Declaration_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Null_Token
    (Self : Formal_Procedure_Declaration_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Box_Token
    (Self : Formal_Procedure_Declaration_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function With_Token_2
    (Self : Formal_Procedure_Declaration_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Semicolon_Token
    (Self : Formal_Procedure_Declaration_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

end Program.Elements.Formal_Procedure_Declarations;
