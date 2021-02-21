--  SPDX-FileCopyrightText: 2021 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Program.Cross_Reference_Updaters;
with Program.Elements.Call_Statements;
with Program.Elements.Discrete_Ranges;
with Program.Interpretations;
with Program.Visibility;

package Program.Complete_Contexts is
   pragma Preelaborate;

   procedure Call_Statement
     (Sets    : not null Program.Interpretations.Context_Access;
      Setter  : not null Program.Cross_Reference_Updaters
                           .Cross_Reference_Updater_Access;
      Element : Program.Elements.Call_Statements.Call_Statement_Access);

   procedure Resolve_To_Expected_Type
     (Sets    : not null Program.Interpretations.Context_Access;
      Setter  : not null Program.Cross_Reference_Updaters
                           .Cross_Reference_Updater_Access;
      Expect  : Program.Visibility.View;
      Element : not null Program.Elements.Element_Access);

end Program.Complete_Contexts;
