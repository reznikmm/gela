--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Program.Elements.Defining_Names;
with Program.Elements;

package Program.Cross_Reference_Updaters is

   pragma Preelaborate;

   type Cross_Reference_Updater is limited interface;

   type Cross_Reference_Updater_Access is
     access all Cross_Reference_Updater'Class
       with Storage_Size => 0;

   not overriding procedure Set_Corresponding_Defining_Name
     (Self : in out Cross_Reference_Updater;
      Name : not null Program.Elements.Element_Access;
      Def  : Program.Elements.Defining_Names.Defining_Name_Access) is abstract;

end Program.Cross_Reference_Updaters;
