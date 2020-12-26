--  SPDX-FileCopyrightText: 2019-2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Program.Compilation_Units;
with Program.Visibility;
with Program.Library_Environments;

private
procedure Program.Resolve_Standard
  (Unit    : not null Program.Compilation_Units.Compilation_Unit_Access;
   Context : aliased in out Program.Visibility.Context;
   Library : in out Program.Library_Environments.Library_Environment);
--  Resolve names in predefined Standard package.
--  This is simplified version of resolver that creates and polulates
--  name table for Standard package. It uses some implementation defined
--  tricks, because Standard package can't be expressed in Ada.
pragma Preelaborate (Program.Resolve_Standard);
