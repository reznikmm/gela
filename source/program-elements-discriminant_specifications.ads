--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Element_Vectors;
with Program.Elements.Declarations;
with Program.Elements.Defining_Identifiers;
with Program.Lexical_Elements;
with Program.Elements.Expressions;

package Program.Elements.Discriminant_Specifications is

   pragma Pure (Program.Elements.Discriminant_Specifications);

   type Discriminant_Specification is
     limited interface and Program.Elements.Declarations.Declaration;

   type Discriminant_Specification_Access is
     access all Discriminant_Specification'Class with Storage_Size => 0;

   not overriding function Names
    (Self : Discriminant_Specification)
      return not null Program.Elements.Defining_Identifiers
          .Defining_Identifier_Vector_Access is abstract;

   not overriding function Object_Subtype
    (Self : Discriminant_Specification)
      return not null Program.Elements.Element_Access is abstract;

   not overriding function Default_Expression
    (Self : Discriminant_Specification)
      return Program.Elements.Expressions.Expression_Access is abstract;

   type Discriminant_Specification_Text is limited interface;

   type Discriminant_Specification_Text_Access is
     access all Discriminant_Specification_Text'Class with Storage_Size => 0;

   not overriding function To_Discriminant_Specification_Text
    (Self : aliased Discriminant_Specification)
      return Discriminant_Specification_Text_Access is abstract;

   not overriding function Colon_Token
    (Self : Discriminant_Specification_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Not_Token
    (Self : Discriminant_Specification_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Null_Token
    (Self : Discriminant_Specification_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Assignment_Token
    (Self : Discriminant_Specification_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Semicolon_Token
    (Self : Discriminant_Specification_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   type Discriminant_Specification_Vector is
     limited interface and Program.Element_Vectors.Element_Vector;

   type Discriminant_Specification_Vector_Access is
     access all Discriminant_Specification_Vector'Class with Storage_Size => 0;

   overriding function Element
    (Self  : Discriminant_Specification_Vector;
     Index : Positive)
      return not null Program.Elements.Element_Access is abstract
     with Post'Class => Element'Result.Is_Discriminant_Specification;

   function To_Discriminant_Specification
    (Self  : Discriminant_Specification_Vector'Class;
     Index : Positive)
      return not null Discriminant_Specification_Access
     is (Self.Element (Index).To_Discriminant_Specification);

end Program.Elements.Discriminant_Specifications;
