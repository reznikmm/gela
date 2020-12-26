--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Program.Cross_Reference_Updaters;
with Program.Elements.Identifiers;
with Program.Simple_Resolvers;
with Program.Symbol_Lists;
with Program.Error_Listeners;

private
package Program.Plain_Contexts.Unit_Name_Resolvers is
   pragma Preelaborate;

   type Unit_Name_Resolver
     (Lists        : not null Program.Symbol_Lists.Symbol_List_Table_Access;
      Errors       : not null Program.Error_Listeners.Error_Listener_Access;
      Declarations : not null Unit_Vector_Access;
      Bodies       : not null Unit_Vector_Access)
       is new Program.Simple_Resolvers.Simple_Resolver
         with null record;

   type Unit_Name_Resolver_Access is
     access all Unit_Name_Resolver'Class with Storage_Size => 0;

   overriding procedure Resolve_Identifier
     (Self   : Unit_Name_Resolver;
      Name   : not null Program.Elements.Identifiers.Identifier_Access;
      Setter : not null
        Program.Cross_Reference_Updaters.Cross_Reference_Updater_Access);

end Program.Plain_Contexts.Unit_Name_Resolvers;
