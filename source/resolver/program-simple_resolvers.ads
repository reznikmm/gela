--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Program.Elements.Expressions;
with Program.Elements.Identifiers;
with Program.Cross_Reference_Updaters;

private
package Program.Simple_Resolvers is
   pragma Preelaborate;

   type Simple_Resolver is limited interface;

   type Simple_Resolver_Access is
     access all Simple_Resolver'Class with Storage_Size => 0;

   not overriding procedure Resolve_Identifier
     (Self   : Simple_Resolver;
      Name   : not null Program.Elements.Identifiers.Identifier_Access;
      Setter : not null
        Program.Cross_Reference_Updaters.Cross_Reference_Updater_Access)
          is abstract;

   procedure Resolve
     (Self   : aliased in out Simple_Resolver'Class;
      Name   : Program.Elements.Expressions.Expression_Access;
      Setter : not null
        Program.Cross_Reference_Updaters.Cross_Reference_Updater_Access);
   --  Resolve Name and call Setter.Set_Corresponding_Defining_Name

end Program.Simple_Resolvers;
