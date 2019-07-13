--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Type_Definitions;

package Program.Elements.Root_Types is

   pragma Pure (Program.Elements.Root_Types);

   type Root_Type is
     limited interface and Program.Elements.Type_Definitions.Type_Definition;

   type Root_Type_Access is access all Root_Type'Class with Storage_Size => 0;

   type Root_Type_Text is limited interface;

   type Root_Type_Text_Access is access all Root_Type_Text'Class
     with Storage_Size => 0;

   not overriding function To_Root_Type_Text
    (Self : aliased in out Root_Type)
      return Root_Type_Text_Access is abstract;

end Program.Elements.Root_Types;
