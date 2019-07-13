--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Definitions;
with Program.Elements.Discrete_Subtype_Definitions;
with Program.Elements.Discrete_Ranges;
with Program.Lexical_Elements;
with Program.Elements.Expressions;
with Program.Elements.Constraints;

package Program.Elements.Subtype_Indications is

   pragma Pure (Program.Elements.Subtype_Indications);

   type Subtype_Indication is
     limited interface and Program.Elements.Definitions.Definition
       and Program.Elements.Discrete_Subtype_Definitions
         .Discrete_Subtype_Definition
       and Program.Elements.Discrete_Ranges.Discrete_Range;

   type Subtype_Indication_Access is access all Subtype_Indication'Class
     with Storage_Size => 0;

   not overriding function Subtype_Mark
    (Self : Subtype_Indication)
      return not null Program.Elements.Expressions.Expression_Access
     is abstract;

   not overriding function Constraint
    (Self : Subtype_Indication)
      return Program.Elements.Constraints.Constraint_Access is abstract;

   not overriding function Has_Not_Null
    (Self : Subtype_Indication)
      return Boolean is abstract;

   type Subtype_Indication_Text is limited interface;

   type Subtype_Indication_Text_Access is
     access all Subtype_Indication_Text'Class with Storage_Size => 0;

   not overriding function To_Subtype_Indication_Text
    (Self : aliased Subtype_Indication)
      return Subtype_Indication_Text_Access is abstract;

   not overriding function Not_Token
    (Self : Subtype_Indication_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Null_Token
    (Self : Subtype_Indication_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

end Program.Elements.Subtype_Indications;
