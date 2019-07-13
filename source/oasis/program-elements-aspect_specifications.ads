--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Element_Vectors;
with Program.Elements.Definitions;
with Program.Elements.Expressions;
with Program.Lexical_Elements;

package Program.Elements.Aspect_Specifications is

   pragma Pure (Program.Elements.Aspect_Specifications);

   type Aspect_Specification is
     limited interface and Program.Elements.Definitions.Definition;

   type Aspect_Specification_Access is access all Aspect_Specification'Class
     with Storage_Size => 0;

   not overriding function Aspect_Mark
    (Self : Aspect_Specification)
      return not null Program.Elements.Expressions.Expression_Access
     is abstract;

   not overriding function Aspect_Definition
    (Self : Aspect_Specification)
      return not null Program.Elements.Expressions.Expression_Access
     is abstract;

   type Aspect_Specification_Text is limited interface;

   type Aspect_Specification_Text_Access is
     access all Aspect_Specification_Text'Class with Storage_Size => 0;

   not overriding function To_Aspect_Specification_Text
    (Self : aliased in out Aspect_Specification)
      return Aspect_Specification_Text_Access is abstract;

   not overriding function Arrow_Token
    (Self : Aspect_Specification_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   type Aspect_Specification_Vector is
     limited interface and Program.Element_Vectors.Element_Vector;

   type Aspect_Specification_Vector_Access is
     access all Aspect_Specification_Vector'Class with Storage_Size => 0;

   overriding function Element
    (Self  : Aspect_Specification_Vector;
     Index : Positive)
      return not null Program.Elements.Element_Access is abstract
     with Post'Class => Element'Result.Is_Aspect_Specification;

   function To_Aspect_Specification
    (Self  : Aspect_Specification_Vector'Class;
     Index : Positive)
      return not null Aspect_Specification_Access
     is (Self.Element (Index).To_Aspect_Specification);

end Program.Elements.Aspect_Specifications;
