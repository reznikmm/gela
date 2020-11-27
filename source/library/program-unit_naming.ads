--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------
--
--  This package provides Unit_Naming_Schema interface and its methods.

package Program.Unit_Naming is
   pragma Preelaborate;

   type Unit_Naming_Schema is limited interface;
   --  Interface to get compilation Text_Name for given compilation unit.

   type Unit_Naming_Schema_Access is access all Unit_Naming_Schema'Class;
   for Unit_Naming_Schema_Access'Storage_Size use 0;

   not overriding function Standard_Text_Name (Self : Unit_Naming_Schema)
      return Text is abstract;
   --  Get compilation Text_Name for Standard library package.

   not overriding function Declaration_Text_Name
     (Self : Unit_Naming_Schema;
      Name : Program.Text)
      return Text is abstract;
   --  Get compilation Text_Name for given library declaration unit.

   not overriding function Body_Text_Name
     (Self : Unit_Naming_Schema;
      Name : Program.Text)
      return Text is abstract;
   --  Get compilation Text_Name for given body.

   not overriding function Subunit_Text_Name
     (Self : Unit_Naming_Schema;
      Name : Program.Text)
      return Text is abstract;
   --  Get compilation Text_Name for given subunit.

end Program.Unit_Naming;
