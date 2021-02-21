--  SPDX-FileCopyrightText: 2021 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Program.Cross_Reference_Updaters;
with Program.Elements.Call_Statements;
with Program.Interpretations;

package Program.Complete_Contexts.Call_Statements is
   pragma Preelaborate;

   procedure Call_Statement
     (Sets    : not null Program.Interpretations.Context_Access;
      Setter  : not null Program.Cross_Reference_Updaters
                           .Cross_Reference_Updater_Access;
      Element : Program.Elements.Call_Statements.Call_Statement_Access);

end Program.Complete_Contexts.Call_Statements;
