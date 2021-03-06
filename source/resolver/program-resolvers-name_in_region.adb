--  SPDX-FileCopyrightText: 2021 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Program.Cross_Reference_Updaters;
with Program.Elements.Identifiers;
with Program.Node_Symbols;
with Program.Simple_Resolvers;
with Program.Symbols;

package body Program.Resolvers.Name_In_Region is

   type Resolver is limited new Program.Simple_Resolvers.Simple_Resolver
   with record
      Region : Program.Visibility.View;
   end record;

   overriding procedure Resolve_Identifier
     (Self   : Resolver;
      Name   : not null Program.Elements.Identifiers.Identifier_Access;
      Setter : not null Program.Cross_Reference_Updaters
        .Cross_Reference_Updater_Access);

   ------------------------
   -- Resolve_Identifier --
   ------------------------

   overriding procedure Resolve_Identifier
     (Self   : Resolver;
      Name   : not null Program.Elements.Identifiers.Identifier_Access;
      Setter : not null Program.Cross_Reference_Updaters
        .Cross_Reference_Updater_Access)
   is
      Symbol : constant Program.Symbols.Symbol :=
        Program.Node_Symbols.Get_Symbol (Name);
   begin
      for J in Program.Visibility.Immediate_Visible (Self.Region, Symbol) loop
         declare
            use type Program.Visibility.View_Cursor;

            View : constant Program.Visibility.View := +J;
         begin
            Setter.Set_Corresponding_Defining_Name
              (Name => Name.To_Element,
               Def  => Program.Visibility.Name (View));

            exit;
         end;
      end loop;
   end Resolve_Identifier;

   ------------------
   -- Resolve_Name --
   ------------------

   procedure Resolve_Name
     (Region : Program.Visibility.View;
      Name   : Program.Elements.Expressions.Expression_Access;
      Setter : not null Program.Cross_Reference_Updaters
        .Cross_Reference_Updater_Access)
   is
      R : aliased Resolver := (Region => Region);
   begin
      R.Resolve (Name, Setter);
   end Resolve_Name;

end Program.Resolvers.Name_In_Region;
