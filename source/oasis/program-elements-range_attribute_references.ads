--  SPDX-FileCopyrightText: 2019 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Program.Elements.Constraints;
with Program.Elements.Attribute_References;

package Program.Elements.Range_Attribute_References is

   pragma Pure (Program.Elements.Range_Attribute_References);

   type Range_Attribute_Reference is
     limited interface and Program.Elements.Constraints.Constraint;

   type Range_Attribute_Reference_Access is
     access all Range_Attribute_Reference'Class with Storage_Size => 0;

   not overriding function Range_Attribute
    (Self : Range_Attribute_Reference)
      return not null Program.Elements.Attribute_References
          .Attribute_Reference_Access is abstract;

   type Range_Attribute_Reference_Text is limited interface;

   type Range_Attribute_Reference_Text_Access is
     access all Range_Attribute_Reference_Text'Class with Storage_Size => 0;

   not overriding function To_Range_Attribute_Reference_Text
    (Self : in out Range_Attribute_Reference)
      return Range_Attribute_Reference_Text_Access is abstract;

end Program.Elements.Range_Attribute_References;
