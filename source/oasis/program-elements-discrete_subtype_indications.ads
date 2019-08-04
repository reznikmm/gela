--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Discrete_Ranges;
with Program.Elements.Expressions;
with Program.Elements.Constraints;

package Program.Elements.Discrete_Subtype_Indications is

   pragma Pure (Program.Elements.Discrete_Subtype_Indications);

   type Discrete_Subtype_Indication is
     limited interface and Program.Elements.Discrete_Ranges.Discrete_Range;

   type Discrete_Subtype_Indication_Access is
     access all Discrete_Subtype_Indication'Class with Storage_Size => 0;

   not overriding function Subtype_Mark
    (Self : Discrete_Subtype_Indication)
      return not null Program.Elements.Expressions.Expression_Access
     is abstract;

   not overriding function Constraint
    (Self : Discrete_Subtype_Indication)
      return Program.Elements.Constraints.Constraint_Access is abstract;

   type Discrete_Subtype_Indication_Text is limited interface;

   type Discrete_Subtype_Indication_Text_Access is
     access all Discrete_Subtype_Indication_Text'Class with Storage_Size => 0;

   not overriding function To_Discrete_Subtype_Indication_Text
    (Self : aliased in out Discrete_Subtype_Indication)
      return Discrete_Subtype_Indication_Text_Access is abstract;

end Program.Elements.Discrete_Subtype_Indications;
