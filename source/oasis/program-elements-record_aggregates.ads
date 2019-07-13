--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Expressions;
with Program.Lexical_Elements;
with Program.Elements.Record_Component_Associations;

package Program.Elements.Record_Aggregates is

   pragma Pure (Program.Elements.Record_Aggregates);

   type Record_Aggregate is
     limited interface and Program.Elements.Expressions.Expression;

   type Record_Aggregate_Access is access all Record_Aggregate'Class
     with Storage_Size => 0;

   not overriding function Components
    (Self : Record_Aggregate)
      return not null Program.Elements.Record_Component_Associations
          .Record_Component_Association_Vector_Access is abstract;

   type Record_Aggregate_Text is limited interface;

   type Record_Aggregate_Text_Access is access all Record_Aggregate_Text'Class
     with Storage_Size => 0;

   not overriding function To_Record_Aggregate_Text
    (Self : aliased in out Record_Aggregate)
      return Record_Aggregate_Text_Access is abstract;

   not overriding function Left_Bracket_Token
    (Self : Record_Aggregate_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Right_Bracket_Token
    (Self : Record_Aggregate_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

end Program.Elements.Record_Aggregates;
