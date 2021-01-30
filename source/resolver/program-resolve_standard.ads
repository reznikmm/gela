--  SPDX-FileCopyrightText: 2019-2021 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with System.Storage_Pools.Subpools;

with Program.Compilation_Units;
with Program.Cross_Reference_Updaters;
with Program.Library_Environments;
with Program.Visibility;

private
procedure Program.Resolve_Standard
  (Unit    : not null Program.Compilation_Units.Compilation_Unit_Access;
   Context : aliased in out Program.Visibility.Context;
   Library : in out Program.Library_Environments.Library_Environment;
   Subpool : not null System.Storage_Pools.Subpools.Subpool_Handle;
   Setter  : not null
     Program.Cross_Reference_Updaters.Cross_Reference_Updater_Access);
--  Resolve names in predefined Standard package.
--  This is simplified version of resolver that creates and polulates
--  name table for Standard package. It uses some implementation defined
--  tricks, because Standard package can't be expressed in Ada.
pragma Preelaborate (Program.Resolve_Standard);
