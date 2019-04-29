--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Type_Definitions;

package Program.Elements.Derived_Record_Extensions is

   pragma Pure (Program.Elements.Derived_Record_Extensions);

   type Derived_Record_Extension is
     limited interface and Program.Elements.Type_Definitions.Type_Definition;

   type Derived_Record_Extension_Access is
     access all Derived_Record_Extension'Class with Storage_Size => 0;

end Program.Elements.Derived_Record_Extensions;
