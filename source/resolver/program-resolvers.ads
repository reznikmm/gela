--  SPDX-FileCopyrightText: 2019 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Program.Compilation_Units;
with Program.Visibility;
with Program.Elements.Expressions;

private package Program.Resolvers is
   pragma Preelaborate;

   procedure Resolve_Names
     (Env  : aliased in out Program.Visibility.Context;
      Unit : not null Program.Compilation_Units.Compilation_Unit_Access);

end Program.Resolvers;
