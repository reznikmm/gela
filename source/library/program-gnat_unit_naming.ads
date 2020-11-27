--  SPDX-FileCopyrightText: 2019 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Program.Unit_Naming;

private with Ada.Strings.Wide_Wide_Maps;

package Program.GNAT_Unit_Naming is
   pragma Preelaborate;

   type GNAT_Unit_Naming
   is new Program.Unit_Naming.Unit_Naming_Schema
     with private;

private

   type GNAT_Unit_Naming is new Program.Unit_Naming.Unit_Naming_Schema with
   record
      Map : Ada.Strings.Wide_Wide_Maps.Wide_Wide_Character_Mapping :=
           Ada.Strings.Wide_Wide_Maps.To_Mapping (".", "-");
   end record;

   overriding function Standard_Text_Name (Self : GNAT_Unit_Naming)
      return Text;
   --  Get compilation Text_Name for Standard library package.

   overriding function Declaration_Text_Name
     (Self   : GNAT_Unit_Naming;
      Name : Program.Text)
      return Program.Text;
   --  Get compilation Text_Name for given library declaration unit.

   overriding function Body_Text_Name
     (Self   : GNAT_Unit_Naming;
      Name : Program.Text)
      return Program.Text;
   --  Get compilation Text_Name for given body.

   overriding function Subunit_Text_Name
     (Self   : GNAT_Unit_Naming;
      Name : Program.Text)
      return Program.Text;

end Program.GNAT_Unit_Naming;
