--  SPDX-FileCopyrightText: 2019-2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Program.Compilation_Units;
with Program.Elements.Expressions;
with Program.Library_Environments;
with Program.Symbol_Lists;
with Program.Visibility;
with Program.Cross_Reference_Updaters;
with Program.Simple_Resolvers;
private package Program.Resolvers is
   pragma Preelaborate;

   procedure Resolve_Names
     (Unit    : not null Program.Compilation_Units.Compilation_Unit_Access;
      Unit_Name_Resolver : not null
        Program.Simple_Resolvers.Simple_Resolver_Access;
      Lists   : in out Program.Symbol_Lists.Symbol_List_Table'Class;
      Context : not null Program.Visibility.Context_Access;
      Library : in out Program.Library_Environments.Library_Environment;
      Setter  : not null
        Program.Cross_Reference_Updaters.Cross_Reference_Updater_Access);

end Program.Resolvers;
