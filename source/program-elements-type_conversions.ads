--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Expressions;
with Program.Tokens;

package Program.Elements.Type_Conversions is

   pragma Pure (Program.Elements.Type_Conversions);

   type Type_Conversion is
     limited interface and Program.Elements.Expressions.Expression;

   type Type_Conversion_Access is access all Type_Conversion'Class
     with Storage_Size => 0;

   not overriding function Subtype_Mark
    (Self : Type_Conversion)
      return not null Program.Elements.Expressions.Expression_Access
     is abstract;

   not overriding function Left_Bracket_Token
    (Self : Type_Conversion)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Operand
    (Self : Type_Conversion)
      return not null Program.Elements.Expressions.Expression_Access
     is abstract;

   not overriding function Right_Bracket_Token
    (Self : Type_Conversion)
      return Program.Tokens.Token_Access is abstract;

end Program.Elements.Type_Conversions;
