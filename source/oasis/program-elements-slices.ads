--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Expressions;
with Program.Lexical_Elements;
with Program.Elements.Discrete_Ranges;

package Program.Elements.Slices is

   pragma Pure (Program.Elements.Slices);

   type Slice is limited interface and Program.Elements.Expressions.Expression;

   type Slice_Access is access all Slice'Class with Storage_Size => 0;

   not overriding function Prefix
    (Self : Slice)
      return not null Program.Elements.Expressions.Expression_Access
     is abstract;

   not overriding function Slice_Range
    (Self : Slice)
      return not null Program.Elements.Discrete_Ranges.Discrete_Range_Access
     is abstract;

   type Slice_Text is limited interface;

   type Slice_Text_Access is access all Slice_Text'Class
     with Storage_Size => 0;

   not overriding function To_Slice_Text
    (Self : aliased in out Slice)
      return Slice_Text_Access is abstract;

   not overriding function Left_Bracket_Token
    (Self : Slice_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Right_Bracket_Token
    (Self : Slice_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

end Program.Elements.Slices;
