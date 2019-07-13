--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Expressions;
with Program.Lexical_Elements;
with Program.Elements.Array_Component_Associations;

package Program.Elements.Array_Aggregates is

   pragma Pure (Program.Elements.Array_Aggregates);

   type Array_Aggregate is
     limited interface and Program.Elements.Expressions.Expression;

   type Array_Aggregate_Access is access all Array_Aggregate'Class
     with Storage_Size => 0;

   not overriding function Components
    (Self : Array_Aggregate)
      return not null Program.Elements.Array_Component_Associations
          .Array_Component_Association_Vector_Access is abstract;

   type Array_Aggregate_Text is limited interface;

   type Array_Aggregate_Text_Access is access all Array_Aggregate_Text'Class
     with Storage_Size => 0;

   not overriding function To_Array_Aggregate_Text
    (Self : aliased in out Array_Aggregate)
      return Array_Aggregate_Text_Access is abstract;

   not overriding function Left_Bracket_Token
    (Self : Array_Aggregate_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Right_Bracket_Token
    (Self : Array_Aggregate_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

end Program.Elements.Array_Aggregates;
