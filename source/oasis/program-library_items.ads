--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Compilation_Units;
with Program.Compilation_Unit_Vectors;
limited with Program.Library_Unit_Declarations;

package Program.Library_Items is
   pragma Pure;

   type Library_Item is limited interface
     and Program.Compilation_Units.Compilation_Unit;
   --  A library_item is a compilation unit that is the declaration, body, or
   --  renaming of a library unit. Each library unit (except Standard) has a
   --  parent unit, which is a library package or generic library package. A
   --  library unit is a child of its parent unit. The root library units are
   --  the children of the predefined library package Standard.

   type Library_Item_Access is access all Library_Item'Class
     with Storage_Size => 0;

   not overriding function Parent (Self : Library_Item)
     return Program.Library_Unit_Declarations.Library_Unit_Declaration_Access
       is abstract;
   --  Returns the parent unit of the given library unit.
   --
   --  Returns a null if the Library_Unit argument represents package Standard.
   --  Root Library_Unit arguments return the package Standard.

end Program.Library_Items;
