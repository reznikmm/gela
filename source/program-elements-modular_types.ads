--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Type_Definitions;
with Program.Lexical_Elements;
with Program.Elements.Expressions;

package Program.Elements.Modular_Types is

   pragma Pure (Program.Elements.Modular_Types);

   type Modular_Type is
     limited interface and Program.Elements.Type_Definitions.Type_Definition;

   type Modular_Type_Access is access all Modular_Type'Class
     with Storage_Size => 0;

   not overriding function Modulus
    (Self : Modular_Type)
      return not null Program.Elements.Expressions.Expression_Access
     is abstract;

   type Modular_Type_Text is limited interface;

   type Modular_Type_Text_Access is access all Modular_Type_Text'Class
     with Storage_Size => 0;

   not overriding function To_Modular_Type_Text
    (Self : aliased Modular_Type)
      return Modular_Type_Text_Access is abstract;

   not overriding function Mod_Token
    (Self : Modular_Type_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

end Program.Elements.Modular_Types;
