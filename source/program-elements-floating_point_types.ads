--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Type_Definitions;
with Program.Tokens;
with Program.Elements.Expressions;
with Program.Elements.Real_Range_Specifications;

package Program.Elements.Floating_Point_Types is

   pragma Pure (Program.Elements.Floating_Point_Types);

   type Floating_Point_Type is
     limited interface and Program.Elements.Type_Definitions.Type_Definition;

   type Floating_Point_Type_Access is access all Floating_Point_Type'Class
     with Storage_Size => 0;

   not overriding function Digits_Token
    (Self : Floating_Point_Type)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Digits_Expression
    (Self : Floating_Point_Type)
      return Program.Elements.Expressions.Expression_Access is abstract;

   not overriding function Real_Range
    (Self : Floating_Point_Type)
      return Program.Elements.Real_Range_Specifications
          .Real_Range_Specification_Access is abstract;

end Program.Elements.Floating_Point_Types;
