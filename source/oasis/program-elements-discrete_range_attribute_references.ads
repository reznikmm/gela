--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Discrete_Ranges;
with Program.Elements.Attribute_References;

package Program.Elements.Discrete_Range_Attribute_References is

   pragma Pure (Program.Elements.Discrete_Range_Attribute_References);

   type Discrete_Range_Attribute_Reference is
     limited interface and Program.Elements.Discrete_Ranges.Discrete_Range;

   type Discrete_Range_Attribute_Reference_Access is
     access all Discrete_Range_Attribute_Reference'Class
     with Storage_Size => 0;

   not overriding function Range_Attribute
    (Self : Discrete_Range_Attribute_Reference)
      return not null Program.Elements.Attribute_References
          .Attribute_Reference_Access is abstract;

   type Discrete_Range_Attribute_Reference_Text is limited interface;

   type Discrete_Range_Attribute_Reference_Text_Access is
     access all Discrete_Range_Attribute_Reference_Text'Class
     with Storage_Size => 0;

   not overriding function To_Discrete_Range_Attribute_Reference_Text
    (Self : aliased in out Discrete_Range_Attribute_Reference)
      return Discrete_Range_Attribute_Reference_Text_Access is abstract;

end Program.Elements.Discrete_Range_Attribute_References;
