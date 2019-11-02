--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Compilation_Units;
with Program.Visibility;

private
procedure Program.Resolve_Standard
  (Unit : not null Program.Compilation_Units.Compilation_Unit_Access;
   Env  : aliased in out Program.Visibility.Context);
--  Resolve names in predefined Standard package.
--  This is simplified version of resolver that creates and polulates
--  name table for Standard package. It uses some implementation defined
--  tricks, because Standard package can't be expressed in Ada.
pragma Preelaborate (Program.Resolve_Standard);
