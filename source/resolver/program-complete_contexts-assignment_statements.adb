--  SPDX-FileCopyrightText: 2021 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

package body Program.Complete_Contexts.Assignment_Statements is

   --------------------------
   -- Assignment_Statement --
   --------------------------

   procedure Assignment_Statement
     (Sets   : not null Program.Interpretations.Context_Access;
      Setter : not null Program.Cross_Reference_Updaters
        .Cross_Reference_Updater_Access;
      Element : not null Program.Elements.Assignment_Statements
        .Assignment_Statement_Access)
   is
      View : Program.Visibility.View;
   begin
      Resolve_To_Any_Type
        (Element => Element.Variable_Name,
         Sets    => Sets,
         Setter  => Setter,
         Result  => View);

      Resolve_To_Expected_Type
        (Element => Element.Expression.To_Element,
         Sets    => Sets,
         Setter  => Setter,
         Expect  => View);
   end Assignment_Statement;

end Program.Complete_Contexts.Assignment_Statements;
