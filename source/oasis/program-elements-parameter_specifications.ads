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

package Program.Elements.Parameter_Specifications is

   pragma Pure (Program.Elements.Parameter_Specifications);

   type Parameter_Specification is
     limited interface and Program.Elements.Declarations.Declaration;

   type Parameter_Specification_Access is
     access all Parameter_Specification'Class with Storage_Size => 0;

   not overriding function Names
    (Self : Parameter_Specification)
      return not null Program.Elements.Defining_Identifiers
          .Defining_Identifier_Vector_Access is abstract;

   not overriding function Parameter_Subtype
    (Self : Parameter_Specification)
      return not null Program.Elements.Element_Access is abstract;

   not overriding function Default_Expression
    (Self : Parameter_Specification)
      return Program.Elements.Expressions.Expression_Access is abstract;

   not overriding function Has_Aliased
    (Self : Parameter_Specification)
      return Boolean is abstract;

   not overriding function Has_In
    (Self : Parameter_Specification)
      return Boolean is abstract;

   not overriding function Has_Out
    (Self : Parameter_Specification)
      return Boolean is abstract;

   not overriding function Has_Not_Null
    (Self : Parameter_Specification)
      return Boolean is abstract;

   type Parameter_Specification_Text is limited interface;

   type Parameter_Specification_Text_Access is
     access all Parameter_Specification_Text'Class with Storage_Size => 0;

   not overriding function To_Parameter_Specification_Text
    (Self : aliased Parameter_Specification)
      return Parameter_Specification_Text_Access is abstract;

   not overriding function Colon_Token
    (Self : Parameter_Specification_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Aliased_Token
    (Self : Parameter_Specification_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function In_Token
    (Self : Parameter_Specification_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Out_Token
    (Self : Parameter_Specification_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Not_Token
    (Self : Parameter_Specification_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Null_Token
    (Self : Parameter_Specification_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Assignment_Token
    (Self : Parameter_Specification_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   type Parameter_Specification_Vector is
     limited interface and Program.Element_Vectors.Element_Vector;

   type Parameter_Specification_Vector_Access is
     access all Parameter_Specification_Vector'Class with Storage_Size => 0;

   overriding function Element
    (Self  : Parameter_Specification_Vector;
     Index : Positive)
      return not null Program.Elements.Element_Access is abstract
     with Post'Class => Element'Result.Is_Parameter_Specification;

   function To_Parameter_Specification
    (Self  : Parameter_Specification_Vector'Class;
     Index : Positive)
      return not null Parameter_Specification_Access
     is (Self.Element (Index).To_Parameter_Specification);

end Program.Elements.Parameter_Specifications;
