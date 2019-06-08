--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Element_Vectors;
with Program.Elements.Definitions;
with Program.Tokens;

package Program.Elements.Variants is

   pragma Pure (Program.Elements.Variants);

   type Variant is
     limited interface and Program.Elements.Definitions.Definition;

   type Variant_Access is access all Variant'Class with Storage_Size => 0;

   not overriding function When_Token
    (Self : Variant)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Choices
    (Self : Variant)
      return not null Program.Element_Vectors.Element_Vector_Access
     is abstract;

   not overriding function Arrow_Token
    (Self : Variant)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Components
    (Self : Variant)
      return not null Program.Element_Vectors.Element_Vector_Access
     is abstract;

   type Variant_Vector is
     limited interface and Program.Element_Vectors.Element_Vector;

   type Variant_Vector_Access is access all Variant_Vector'Class
     with Storage_Size => 0;

   overriding function Element
    (Self  : Variant_Vector;
     Index : Positive)
      return not null Program.Elements.Element_Access is abstract
     with Post'Class => Element'Result.Is_Variant;

   function To_Variant
    (Self  : Variant_Vector'Class;
     Index : Positive)
      return not null Variant_Access is (Self.Element (Index).To_Variant);

end Program.Elements.Variants;
