--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Library_Items;
with Program.Library_Unit_Bodies;
with Program.Library_Unit_Declarations;
with Program.Subunits;

package body Program.Compilation_Units is

   ----------------
   -- To_Subunit --
   ----------------

   function To_Subunit (Self : access Compilation_Unit'Class)
      return Program.Subunits.Subunit_Access is
   begin
      return Program.Subunits.Subunit_Access (Self);
   end To_Subunit;

   ---------------------
   -- To_Library_Item --
   ---------------------

   function To_Library_Item (Self : access Compilation_Unit'Class)
      return Program.Library_Items.Library_Item_Access is
   begin
      return Program.Library_Items.Library_Item_Access (Self);
   end To_Library_Item;

   --------------------------
   -- To_Library_Unit_Body --
   --------------------------

   function To_Library_Unit_Body (Self : access Compilation_Unit'Class)
     return Program.Library_Unit_Bodies.Library_Unit_Body_Access is
   begin
      return Program.Library_Unit_Bodies.Library_Unit_Body_Access (Self);
   end To_Library_Unit_Body;

   ---------------------------------
   -- To_Library_Unit_Declaration --
   ---------------------------------

   function To_Library_Unit_Declaration (Self : access Compilation_Unit'Class)
      return Program.Library_Unit_Declarations.Library_Unit_Declaration_Access
   is
   begin
      return Program.Library_Unit_Declarations.Library_Unit_Declaration_Access
        (Self);
   end To_Library_Unit_Declaration;

end Program.Compilation_Units;
