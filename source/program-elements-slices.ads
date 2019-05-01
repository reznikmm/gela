--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Expressions;
with Program.Tokens;
with Program.Elements.Discrete_Ranges;

package Program.Elements.Slices is

   pragma Pure (Program.Elements.Slices);

   type Slice is limited interface and Program.Elements.Expressions.Expression;

   type Slice_Access is access all Slice'Class with Storage_Size => 0;

   not overriding function Prefix
    (Self : Slice)
      return Program.Elements.Expressions.Expression_Access is abstract;

   not overriding function Left_Bracket_Token
    (Self : Slice)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Slice_Range
    (Self : Slice)
      return Program.Elements.Discrete_Ranges.Discrete_Range_Access
     is abstract;

   not overriding function Right_Bracket_Token
    (Self : Slice)
      return Program.Tokens.Token_Access is abstract;

end Program.Elements.Slices;
