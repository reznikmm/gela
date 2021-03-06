--  SPDX-FileCopyrightText: 2021 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Program.Elements.Case_Paths;

package body Program.Complete_Contexts.Case_Statements is

   --------------------
   -- Case_Statement --
   --------------------

   procedure Case_Statement
     (Sets   : not null Program.Interpretations.Context_Access;
      Setter : not null Program.Cross_Reference_Updaters
        .Cross_Reference_Updater_Access;
      Element : Program.Elements.Case_Statements.Case_Statement_Access)
   is
      View : Program.Visibility.View;
   begin
      Resolve_To_Discrete_Type
        (Element => Element.Selecting_Expression,
         Sets    => Sets,
         Setter  => Setter,
         Result  => View);

      for J in Element.Paths.Each_Element loop
         declare
            Path : constant Program.Elements.Case_Paths.Case_Path_Access :=
              J.Element.To_Case_Path;
         begin
            for K in Path.Choices.Each_Element loop
               Resolve_To_Expected_Type
                 (Element => K.Element,
                  Sets    => Sets,
                  Setter  => Setter,
                  Expect  => View);
            end loop;

            for K in Path.Statements.Each_Element loop
               null;
            end loop;
         end;
      end loop;
   end Case_Statement;

end Program.Complete_Contexts.Case_Statements;
