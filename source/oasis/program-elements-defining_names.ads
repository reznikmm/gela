--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package Program.Elements.Defining_Names is

   pragma Pure (Program.Elements.Defining_Names);

   type Defining_Name is limited interface and Program.Elements.Element;

   type Defining_Name_Access is access all Defining_Name'Class
     with Storage_Size => 0;

   not overriding function Image (Self : Defining_Name) return Text
     is abstract;

end Program.Elements.Defining_Names;
