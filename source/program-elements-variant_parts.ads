--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Definitions;
with Program.Lexical_Elements;
with Program.Elements.Identifiers;
with Program.Elements.Variants;

package Program.Elements.Variant_Parts is

   pragma Pure (Program.Elements.Variant_Parts);

   type Variant_Part is
     limited interface and Program.Elements.Definitions.Definition;

   type Variant_Part_Access is access all Variant_Part'Class
     with Storage_Size => 0;

   not overriding function Discriminant
    (Self : Variant_Part)
      return not null Program.Elements.Identifiers.Identifier_Access
     is abstract;

   not overriding function Variants
    (Self : Variant_Part)
      return not null Program.Elements.Variants.Variant_Vector_Access
     is abstract;

   type Variant_Part_Text is limited interface;

   type Variant_Part_Text_Access is access all Variant_Part_Text'Class
     with Storage_Size => 0;

   not overriding function To_Variant_Part_Text
    (Self : aliased Variant_Part)
      return Variant_Part_Text_Access is abstract;

   not overriding function Case_Token
    (Self : Variant_Part_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Is_Token
    (Self : Variant_Part_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function End_Token
    (Self : Variant_Part_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Case_Token_2
    (Self : Variant_Part_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Semicolon_Token
    (Self : Variant_Part_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

end Program.Elements.Variant_Parts;
