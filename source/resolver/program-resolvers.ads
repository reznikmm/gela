--  SPDX-FileCopyrightText: 2019-2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Program.Compilation_Units;
with Program.Elements.Defining_Names;
with Program.Elements.Expressions;
with Program.Visibility;

private package Program.Resolvers is
   pragma Preelaborate;

   type Cross_Reference_Updater is limited interface;

   type Cross_Reference_Updater_Access is
     access all Cross_Reference_Updater'Class
       with Storage_Size => 0;

   not overriding procedure Set_Corresponding_Defining_Name
     (Self : in out Cross_Reference_Updater;
      Name : Program.Elements.Element_Access;
      Def  : Program.Elements.Defining_Names.Defining_Name_Access) is abstract;

   procedure Resolve_Names
     (Env  : aliased in out Program.Visibility.Context;
      Unit : not null Program.Compilation_Units.Compilation_Unit_Access);

end Program.Resolvers;
