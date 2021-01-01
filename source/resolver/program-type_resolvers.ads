--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Program.Compilation_Units;
with Program.Elements.Expressions;
with Program.Visibility;
with Program.Cross_Reference_Updaters;

private package Program.Type_Resolvers is
   pragma Preelaborate;

   procedure Resolve_Type
     (Element : Program.Elements.Expressions.Expression_Access;
      Context : not null Program.Visibility.Context_Access;
      Setter  : not null
        Program.Cross_Reference_Updaters.Cross_Reference_Updater_Access;
      Value   : out Program.Visibility.View);
   --  The Element is a subtype_mark. Resolve it and return corresponding
   --  type view.

   procedure Resolve_Type_Definition
     (Element : Program.Elements.Element_Access;
      Context : not null Program.Visibility.Context_Access;
      Setter  : not null
        Program.Cross_Reference_Updaters.Cross_Reference_Updater_Access;
      Value   : out Program.Visibility.View);
   --  The Element is a subtype_indication, access_definition or
   --  array_type_definition. Resolve it and return corresponding type view.

end Program.Type_Resolvers;
