--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Expressions;
with Program.Tokens;
with Program.Elements.Array_Component_Associations;

package Program.Elements.Array_Aggregates is

   pragma Pure (Program.Elements.Array_Aggregates);

   type Array_Aggregate is
     limited interface and Program.Elements.Expressions.Expression;

   type Array_Aggregate_Access is access all Array_Aggregate'Class
     with Storage_Size => 0;

   not overriding function Left_Bracket_Token
    (Self : Array_Aggregate)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Components
    (Self : Array_Aggregate)
      return not null Program.Elements.Array_Component_Associations
          .Array_Component_Association_Vector_Access is abstract;

   not overriding function Right_Bracket_Token
    (Self : Array_Aggregate)
      return Program.Tokens.Token_Access is abstract;

end Program.Elements.Array_Aggregates;
