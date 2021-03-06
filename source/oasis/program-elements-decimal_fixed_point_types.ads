--  SPDX-FileCopyrightText: 2019 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Program.Elements.Type_Definitions;
with Program.Lexical_Elements;
with Program.Elements.Expressions;
with Program.Elements.Real_Range_Specifications;

package Program.Elements.Decimal_Fixed_Point_Types is

   pragma Pure (Program.Elements.Decimal_Fixed_Point_Types);

   type Decimal_Fixed_Point_Type is
     limited interface and Program.Elements.Type_Definitions.Type_Definition;

   type Decimal_Fixed_Point_Type_Access is
     access all Decimal_Fixed_Point_Type'Class with Storage_Size => 0;

   not overriding function Delta_Expression
    (Self : Decimal_Fixed_Point_Type)
      return not null Program.Elements.Expressions.Expression_Access
     is abstract;

   not overriding function Digits_Expression
    (Self : Decimal_Fixed_Point_Type)
      return not null Program.Elements.Expressions.Expression_Access
     is abstract;

   not overriding function Real_Range
    (Self : Decimal_Fixed_Point_Type)
      return Program.Elements.Real_Range_Specifications
          .Real_Range_Specification_Access is abstract;

   type Decimal_Fixed_Point_Type_Text is limited interface;

   type Decimal_Fixed_Point_Type_Text_Access is
     access all Decimal_Fixed_Point_Type_Text'Class with Storage_Size => 0;

   not overriding function To_Decimal_Fixed_Point_Type_Text
    (Self : in out Decimal_Fixed_Point_Type)
      return Decimal_Fixed_Point_Type_Text_Access is abstract;

   not overriding function Delta_Token
    (Self : Decimal_Fixed_Point_Type_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Digits_Token
    (Self : Decimal_Fixed_Point_Type_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

end Program.Elements.Decimal_Fixed_Point_Types;
