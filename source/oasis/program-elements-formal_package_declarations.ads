--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Declarations;
with Program.Lexical_Elements;
with Program.Elements.Defining_Identifiers;
with Program.Elements.Expressions;
with Program.Elements.Formal_Package_Associations;
with Program.Elements.Aspect_Specifications;

package Program.Elements.Formal_Package_Declarations is

   pragma Pure (Program.Elements.Formal_Package_Declarations);

   type Formal_Package_Declaration is
     limited interface and Program.Elements.Declarations.Declaration;

   type Formal_Package_Declaration_Access is
     access all Formal_Package_Declaration'Class with Storage_Size => 0;

   not overriding function Name
    (Self : Formal_Package_Declaration)
      return not null Program.Elements.Defining_Identifiers
          .Defining_Identifier_Access is abstract;

   not overriding function Generic_Package_Name
    (Self : Formal_Package_Declaration)
      return not null Program.Elements.Expressions.Expression_Access
     is abstract;

   not overriding function Parameters
    (Self : Formal_Package_Declaration)
      return Program.Elements.Formal_Package_Associations
          .Formal_Package_Association_Vector_Access is abstract;

   not overriding function Aspects
    (Self : Formal_Package_Declaration)
      return Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access is abstract;

   type Formal_Package_Declaration_Text is limited interface;

   type Formal_Package_Declaration_Text_Access is
     access all Formal_Package_Declaration_Text'Class with Storage_Size => 0;

   not overriding function To_Formal_Package_Declaration_Text
    (Self : aliased in out Formal_Package_Declaration)
      return Formal_Package_Declaration_Text_Access is abstract;

   not overriding function With_Token
    (Self : Formal_Package_Declaration_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Package_Token
    (Self : Formal_Package_Declaration_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Is_Token
    (Self : Formal_Package_Declaration_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function New_Token
    (Self : Formal_Package_Declaration_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Left_Bracket_Token
    (Self : Formal_Package_Declaration_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Right_Bracket_Token
    (Self : Formal_Package_Declaration_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function With_Token_2
    (Self : Formal_Package_Declaration_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Semicolon_Token
    (Self : Formal_Package_Declaration_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

end Program.Elements.Formal_Package_Declarations;
