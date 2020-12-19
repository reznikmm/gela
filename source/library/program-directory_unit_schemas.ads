--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Program.Unit_Naming;
private with Ada.Strings.Wide_Wide_Unbounded;

package Program.Directory_Unit_Schemas is

   pragma Preelaborate;

   type Directory_Unit_Schema
     (Base_Name : Program.Unit_Naming.Unit_Naming_Schema_Access)
   is new Program.Unit_Naming.Unit_Naming_Schema
     with private;
   --  This object returns full file name. It uses Base_Name to find a base
   --  file name. It looks into a current directory if Add_Directory
   --  was never called. Otherwise it looks given Path only.

   procedure Add_Directory
     (Self : in out Directory_Unit_Schema;
      Path : Program.Text);
   --  Add the Path to directory search list.

private

   type Directory_Unit_Schema_Access is access all Directory_Unit_Schema;

   type Directory_Unit_Schema
     (Base_Name : Program.Unit_Naming.Unit_Naming_Schema_Access)
   is new Program.Unit_Naming.Unit_Naming_Schema with record
      Path : Ada.Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String;
      Next : Directory_Unit_Schema_Access;
   end record;

   overriding function Standard_Text_Name (Self : Directory_Unit_Schema)
      return Program.Text;
   --  Get compilation Text_Name for Standard library package.

   overriding function Declaration_Text_Name
     (Self : Directory_Unit_Schema;
      Name : Program.Text)
      return Program.Text;
   --  Get compilation Text_Name for given library declaration unit.

   overriding function Body_Text_Name
     (Self : Directory_Unit_Schema;
      Name : Program.Text)
      return Program.Text;
   --  Get compilation Text_Name for given body.

   overriding function Subunit_Text_Name
     (Self : Directory_Unit_Schema;
      Name : Program.Text)
      return Program.Text;

end Program.Directory_Unit_Schemas;
