--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Declarations;
with Program.Tokens;
with Program.Elements.Expressions;
with Program.Element_Vectors;

package Program.Elements.Parameter_Specifications is

   pragma Pure (Program.Elements.Parameter_Specifications);

   type Parameter_Specification is
     limited interface and Program.Elements.Declarations.Declaration;

   type Parameter_Specification_Access is
     access all Parameter_Specification'Class with Storage_Size => 0;

   not overriding function Colon_Token
    (Self : Parameter_Specification)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Aliased_Token
    (Self : Parameter_Specification)
      return Program.Tokens.Token_Access is abstract;

   not overriding function In_Token
    (Self : Parameter_Specification)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Out_Token
    (Self : Parameter_Specification)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Not_Token
    (Self : Parameter_Specification)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Null_Token
    (Self : Parameter_Specification)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Parameter_Subtype
    (Self : Parameter_Specification)
      return Program.Elements.Element_Access is abstract;

   not overriding function Assignment_Token
    (Self : Parameter_Specification)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Default_Expression
    (Self : Parameter_Specification)
      return Program.Elements.Expressions.Expression_Access is abstract;

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
