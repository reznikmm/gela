--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package Program.Elements.Paths is

   pragma Pure (Program.Elements.Paths);

   type Path is limited interface and Program.Elements.Element;

   type Path_Access is access all Path'Class with Storage_Size => 0;

   type Path_Text is limited interface;

   type Path_Text_Access is access all Path_Text'Class with Storage_Size => 0;

   not overriding function To_Path_Text
    (Self : aliased Path)
      return Path_Text_Access is abstract;

end Program.Elements.Paths;
