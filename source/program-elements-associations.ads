--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package Program.Elements.Associations is

   pragma Pure (Program.Elements.Associations);

   type Association is limited interface and Program.Elements.Element;

   type Association_Access is access all Association'Class
     with Storage_Size => 0;

   type Association_Text is limited interface;

   type Association_Text_Access is access all Association_Text'Class
     with Storage_Size => 0;

   not overriding function To_Association_Text
    (Self : aliased Association)
      return Association_Text_Access is abstract;

end Program.Elements.Associations;
