--  SPDX-FileCopyrightText: 2021 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Program.Cross_Reference_Updaters;
with Program.Elements.Discrete_Ranges;
with Program.Elements.Expressions;
with Program.Interpretations;
with Program.Visibility;

package Program.Complete_Contexts is
   pragma Preelaborate;

   procedure Resolve_To_Expected_Type
     (Element : not null Program.Elements.Element_Access;
      Sets    : not null Program.Interpretations.Context_Access;
      Setter  : not null Program.Cross_Reference_Updaters
                           .Cross_Reference_Updater_Access;
      Expect  : Program.Visibility.View)
        with Pre => Expect.Kind in Program.Visibility.Type_View_Kind;

   procedure Resolve_To_Any_Type
     (Element : not null Program.Elements.Expressions.Expression_Access;
      Sets    : not null Program.Interpretations.Context_Access;
      Setter  : not null Program.Cross_Reference_Updaters
                           .Cross_Reference_Updater_Access;
      Result  : out Program.Visibility.View);

private

   function Up
     (Element : not null access Program.Elements.Element'Class;
      Sets    : not null Program.Interpretations.Context_Access)
        return Program.Interpretations.Interpretation_Set;
   --  Visit subtree rooted at the Element and construct set of possible
   --  interpretations for it.

   procedure Down
     (Element  : not null access Program.Elements.Element'Class;
      Solution : Program.Interpretations.Solution;
      Setter   : not null Program.Cross_Reference_Updaters
        .Cross_Reference_Updater_Access;
      Sets     : not null Program.Interpretations.Context_Access);
   --  Assign solution to the Element and all its children.

   procedure Resolve_Parameters
     (Arguments   : Program.Interpretations.Interpretation_Set_Array;
      Parameters  : Program.Visibility.View_Array;
      Callback    : access procedure
        (Down : Program.Interpretations.Solution_Array);
      Down        : in out Program.Interpretations.Solution_Array;
      Index       : Positive := 1);
   --  For each parameter starting from Parameters (Index), find an
   --  expression interpretation in Arguments provided that its type is an
   --  expected type of the parameter. Then fill corresponding solutions in
   --  Down and call Callback (Down).

end Program.Complete_Contexts;
