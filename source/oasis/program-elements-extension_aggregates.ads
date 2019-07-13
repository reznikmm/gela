--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Expressions;
with Program.Lexical_Elements;
with Program.Elements.Record_Component_Associations;

package Program.Elements.Extension_Aggregates is

   pragma Pure (Program.Elements.Extension_Aggregates);

   type Extension_Aggregate is
     limited interface and Program.Elements.Expressions.Expression;

   type Extension_Aggregate_Access is access all Extension_Aggregate'Class
     with Storage_Size => 0;

   not overriding function Ancestor
    (Self : Extension_Aggregate)
      return not null Program.Elements.Expressions.Expression_Access
     is abstract;

   not overriding function Components
    (Self : Extension_Aggregate)
      return not null Program.Elements.Record_Component_Associations
          .Record_Component_Association_Vector_Access is abstract;

   type Extension_Aggregate_Text is limited interface;

   type Extension_Aggregate_Text_Access is
     access all Extension_Aggregate_Text'Class with Storage_Size => 0;

   not overriding function To_Extension_Aggregate_Text
    (Self : aliased in out Extension_Aggregate)
      return Extension_Aggregate_Text_Access is abstract;

   not overriding function Left_Bracket_Token
    (Self : Extension_Aggregate_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function With_Token
    (Self : Extension_Aggregate_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Right_Bracket_Token
    (Self : Extension_Aggregate_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

end Program.Elements.Extension_Aggregates;
