--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Declarations;
with Program.Lexical_Elements;
with Program.Elements.Defining_Identifiers;
with Program.Elements.Subtype_Indications;
with Program.Elements.Aspect_Specifications;

package Program.Elements.Subtype_Declarations is

   pragma Pure (Program.Elements.Subtype_Declarations);

   type Subtype_Declaration is
     limited interface and Program.Elements.Declarations.Declaration;

   type Subtype_Declaration_Access is access all Subtype_Declaration'Class
     with Storage_Size => 0;

   not overriding function Name
    (Self : Subtype_Declaration)
      return not null Program.Elements.Defining_Identifiers
          .Defining_Identifier_Access is abstract;

   not overriding function Subtype_Indication
    (Self : Subtype_Declaration)
      return not null Program.Elements.Subtype_Indications
          .Subtype_Indication_Access is abstract;

   not overriding function Aspects
    (Self : Subtype_Declaration)
      return Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access is abstract;

   type Subtype_Declaration_Text is limited interface;

   type Subtype_Declaration_Text_Access is
     access all Subtype_Declaration_Text'Class with Storage_Size => 0;

   not overriding function To_Subtype_Declaration_Text
    (Self : aliased in out Subtype_Declaration)
      return Subtype_Declaration_Text_Access is abstract;

   not overriding function Subtype_Token
    (Self : Subtype_Declaration_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Is_Token
    (Self : Subtype_Declaration_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function With_Token
    (Self : Subtype_Declaration_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Semicolon_Token
    (Self : Subtype_Declaration_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

end Program.Elements.Subtype_Declarations;
