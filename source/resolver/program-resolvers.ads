--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Compilation_Units;
with Program.Visibility;
with Program.Elements.Defining_Names;
with Program.Symbols;

private package Program.Resolvers is
   pragma Preelaborate;

   procedure Resolve_Names
     (Env  : aliased in out Program.Visibility.Context;
      Unit : not null Program.Compilation_Units.Compilation_Unit_Access);

   function To_Symbol
     (Name : access Program.Elements.Defining_Names.Defining_Name'Class)
         return Program.Symbols.Symbol;
   --  Return a symbol for given defining name. Return symbol of the
   --  selector for expanded defining name.

end Program.Resolvers;
