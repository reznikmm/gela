--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Compilation_Units;
with Program.Visibility;

private package Program.Resolvers is
   pragma Preelaborate;

   procedure Resolve_Names
     (Env  : aliased in out Program.Visibility.Context;
      Unit : not null Program.Compilation_Units.Compilation_Unit_Access);

end Program.Resolvers;
