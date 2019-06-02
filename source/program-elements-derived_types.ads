--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Type_Definitions;
with Program.Tokens;
with Program.Elements.Expressions;

package Program.Elements.Derived_Types is

   pragma Pure (Program.Elements.Derived_Types);

   type Derived_Type is
     limited interface and Program.Elements.Type_Definitions.Type_Definition;

   type Derived_Type_Access is access all Derived_Type'Class
     with Storage_Size => 0;

   not overriding function Abstract_Token
    (Self : Derived_Type)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Limited_Token
    (Self : Derived_Type)
      return Program.Tokens.Token_Access is abstract;

   not overriding function New_Token
    (Self : Derived_Type)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Parent
    (Self : Derived_Type)
      return Program.Elements.Expressions.Expression_Access is abstract;

end Program.Elements.Derived_Types;
