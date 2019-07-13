--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Declarations;
with Program.Elements.Defining_Identifiers;
with Program.Lexical_Elements;
with Program.Elements.Expressions;

package Program.Elements.Return_Object_Specifications is

   pragma Pure (Program.Elements.Return_Object_Specifications);

   type Return_Object_Specification is
     limited interface and Program.Elements.Declarations.Declaration;

   type Return_Object_Specification_Access is
     access all Return_Object_Specification'Class with Storage_Size => 0;

   not overriding function Name
    (Self : Return_Object_Specification)
      return not null Program.Elements.Defining_Identifiers
          .Defining_Identifier_Access is abstract;

   not overriding function Object_Subtype
    (Self : Return_Object_Specification)
      return not null Program.Elements.Element_Access is abstract;

   not overriding function Expression
    (Self : Return_Object_Specification)
      return Program.Elements.Expressions.Expression_Access is abstract;

   not overriding function Has_Aliased
    (Self : Return_Object_Specification)
      return Boolean is abstract;

   not overriding function Has_Constant
    (Self : Return_Object_Specification)
      return Boolean is abstract;

   type Return_Object_Specification_Text is limited interface;

   type Return_Object_Specification_Text_Access is
     access all Return_Object_Specification_Text'Class with Storage_Size => 0;

   not overriding function To_Return_Object_Specification_Text
    (Self : aliased Return_Object_Specification)
      return Return_Object_Specification_Text_Access is abstract;

   not overriding function Colon_Token
    (Self : Return_Object_Specification_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Aliased_Token
    (Self : Return_Object_Specification_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Constant_Token
    (Self : Return_Object_Specification_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Assignment_Token
    (Self : Return_Object_Specification_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

end Program.Elements.Return_Object_Specifications;
