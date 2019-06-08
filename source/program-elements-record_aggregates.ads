--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Expressions;
with Program.Tokens;
with Program.Elements.Record_Component_Associations;

package Program.Elements.Record_Aggregates is

   pragma Pure (Program.Elements.Record_Aggregates);

   type Record_Aggregate is
     limited interface and Program.Elements.Expressions.Expression;

   type Record_Aggregate_Access is access all Record_Aggregate'Class
     with Storage_Size => 0;

   not overriding function Left_Bracket_Token
    (Self : Record_Aggregate)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Components
    (Self : Record_Aggregate)
      return not null Program.Elements.Record_Component_Associations
          .Record_Component_Association_Vector_Access is abstract;

   not overriding function Right_Bracket_Token
    (Self : Record_Aggregate)
      return Program.Tokens.Token_Access is abstract;

end Program.Elements.Record_Aggregates;
