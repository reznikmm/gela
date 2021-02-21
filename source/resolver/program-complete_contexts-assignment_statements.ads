--  SPDX-FileCopyrightText: 2021 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Program.Cross_Reference_Updaters;
with Program.Elements.Assignment_Statements;
with Program.Interpretations;

package Program.Complete_Contexts.Assignment_Statements is
   pragma Preelaborate;

   procedure Assignment_Statement
     (Sets    : not null Program.Interpretations.Context_Access;
      Setter  : not null Program.Cross_Reference_Updaters
                           .Cross_Reference_Updater_Access;
      Element : not null Program.Elements.Assignment_Statements
      .Assignment_Statement_Access);

end Program.Complete_Contexts.Assignment_Statements;
