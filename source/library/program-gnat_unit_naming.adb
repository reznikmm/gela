--  SPDX-FileCopyrightText: 2019 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Ada.Strings.Wide_Wide_Fixed;
with Ada.Wide_Wide_Characters.Handling;

package body Program.GNAT_Unit_Naming is

   --------------------
   -- Body_Text_Name --
   --------------------

   function Body_Text_Name
     (Self : GNAT_Unit_Naming;
      Name : Program.Text)
      return Program.Text
   is
      Lower_Text : constant Text :=
        Ada.Wide_Wide_Characters.Handling.To_Lower (Name);

      Base_Name : constant Text :=
        Ada.Strings.Wide_Wide_Fixed.Translate (Lower_Text, Self.Map);
   begin
      return Base_Name & ".adb";
   end Body_Text_Name;

   ---------------------------
   -- Declaration_Text_Name --
   ---------------------------

   overriding function Declaration_Text_Name
     (Self : GNAT_Unit_Naming;
      Name : Program.Text)
      return Program.Text
   is
      Lower_Text : constant Text :=
        Ada.Wide_Wide_Characters.Handling.To_Lower (Name);

      Base_Name : constant Text :=
        Ada.Strings.Wide_Wide_Fixed.Translate (Lower_Text, Self.Map);
   begin
      return Base_Name & ".ads";
   end Declaration_Text_Name;

   ------------------------
   -- Standard_Text_Name --
   ------------------------

   overriding function Standard_Text_Name
     (Self : GNAT_Unit_Naming) return Text
   is
      pragma Unreferenced (Self);
   begin
      return "_standard_.ads";
   end Standard_Text_Name;

   -----------------------
   -- Subunit_Text_Name --
   -----------------------

   overriding function Subunit_Text_Name
     (Self : GNAT_Unit_Naming; Name : Program.Text) return Text
       renames Body_Text_Name;

end Program.GNAT_Unit_Naming;
