--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Type_Definitions;
with Program.Tokens;
with Program.Elements.Expressions;

package Program.Elements.Modular_Types is

   pragma Pure (Program.Elements.Modular_Types);

   type Modular_Type is
     limited interface and Program.Elements.Type_Definitions.Type_Definition;

   type Modular_Type_Access is access all Modular_Type'Class
     with Storage_Size => 0;

   not overriding function Mod_Token
    (Self : Modular_Type)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Modulus
    (Self : Modular_Type)
      return not null Program.Elements.Expressions.Expression_Access
     is abstract;

end Program.Elements.Modular_Types;
