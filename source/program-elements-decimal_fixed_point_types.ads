--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Type_Definitions;
with Program.Tokens;
with Program.Elements.Expressions;
with Program.Elements.Simple_Expression_Ranges;

package Program.Elements.Decimal_Fixed_Point_Types is

   pragma Pure (Program.Elements.Decimal_Fixed_Point_Types);

   type Decimal_Fixed_Point_Type is
     limited interface and Program.Elements.Type_Definitions.Type_Definition;

   type Decimal_Fixed_Point_Type_Access is
     access all Decimal_Fixed_Point_Type'Class with Storage_Size => 0;

   not overriding function Delta_Token
    (Self : Decimal_Fixed_Point_Type)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Delta_Expression
    (Self : Decimal_Fixed_Point_Type)
      return Program.Elements.Expressions.Expression_Access is abstract;

   not overriding function Digits_Token
    (Self : Decimal_Fixed_Point_Type)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Digits_Expression
    (Self : Decimal_Fixed_Point_Type)
      return Program.Elements.Expressions.Expression_Access is abstract;

   not overriding function Real_Range_Constraint
    (Self : Decimal_Fixed_Point_Type)
      return Program.Elements.Simple_Expression_Ranges
          .Simple_Expression_Range_Access is abstract;

end Program.Elements.Decimal_Fixed_Point_Types;
