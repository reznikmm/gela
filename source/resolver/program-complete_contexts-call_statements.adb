--  SPDX-FileCopyrightText: 2021 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Program.Interpretations.Names;

package body Program.Complete_Contexts.Call_Statements is

   --------------------
   -- Call_Statement --
   --------------------

   procedure Call_Statement
     (Sets    : not null Program.Interpretations.Context_Access;
      Setter  : not null Program.Cross_Reference_Updaters
                           .Cross_Reference_Updater_Access;
      Element : Program.Elements.Call_Statements.Call_Statement_Access)
   is
      use type Program.Interpretations.Names.Cursor;
      use all type Program.Visibility.View_Kind;
      procedure Callback (Down : Program.Interpretations.Solution_Array);
      Name : Program.Interpretations.Interpretation_Set;
      Args : Program.Interpretations.Interpretation_Set_Array
        (1 .. Element.Parameters.Length);
      Found : Program.Interpretations.Solution_Array (0 .. Args'Length);
      Count : Natural := 0;

      procedure Callback (Down : Program.Interpretations.Solution_Array) is
      begin
         Count := Count + 1;
         Found := Down;
      end Callback;

   begin
      Name := Up (Element.Called_Name, Sets);

      for J in Element.Parameters.Each_Element loop
         Args (J.Index) := Up (J.Element, Sets);
      end loop;

      for N in Program.Interpretations.Names.Each (Name) loop
         declare
            View   : constant Program.Visibility.View := +N;
            Params : constant Program.Visibility.View_Array :=
              Program.Visibility.Parameters (View);
            Down   : Program.Interpretations.Solution_Array
              (0 .. Params'Length);
         begin
            if View.Kind = Procedure_View then
               Down (0) :=
                 (Program.Interpretations.Defining_Name_Solution,
                  View);

               Resolve_Parameters
                 (Arguments  => Args,
                  Parameters => Params,
                  Callback   => Callback'Access,
                  Down       => Down);
            end if;
         end;
      end loop;

      if Count > 0 then
         Down (Element.Called_Name, Found (0), Setter, Sets);

         for J in Element.Parameters.Each_Element loop
            Down (J.Element, Found (J.Index), Setter, Sets);
         end loop;
      end if;
   end Call_Statement;

end Program.Complete_Contexts.Call_Statements;
