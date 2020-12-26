--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Program.Safe_Element_Visitors;

package body Program.Simple_Resolvers is

   type Visitor
     (SR     : not null Simple_Resolver_Access;
      Setter : not null
        Program.Cross_Reference_Updaters.Cross_Reference_Updater_Access)
          is new Program.Safe_Element_Visitors.Safe_Element_Visitor
            with null record;

   overriding procedure Identifier
    (Self    : in out Visitor;
     Element : not null Program.Elements.Identifiers.Identifier_Access);

   ----------------
   -- Identifier --
   ----------------

   overriding procedure Identifier
    (Self    : in out Visitor;
     Element : not null Program.Elements.Identifiers.Identifier_Access) is
   begin
      Self.SR.Resolve_Identifier (Element, Self.Setter);
   end Identifier;

   -------------
   -- Resolve --
   -------------

   procedure Resolve
     (Self   : aliased in out Simple_Resolver'Class;
      Name   : Program.Elements.Expressions.Expression_Access;
      Setter : not null
        Program.Cross_Reference_Updaters.Cross_Reference_Updater_Access)
   is
      V : Visitor (Self'Unchecked_Access, Setter);
   begin
      V.Visit (Name);
   end Resolve;

end Program.Simple_Resolvers;
