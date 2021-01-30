--  SPDX-FileCopyrightText: 2019-2021 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Program.Cross_Reference_Updaters;
with Program.Element_Vector_Factories;
with Program.Element_Vectors;
with Program.Implicit_Element_Factories;
with Program.Visibility;

package Program.Predefined_Operators is
   pragma Preelaborate;

   procedure Create_Operators_For_Array
     (Self      : in out Program.Visibility.Context'Class;
      Type_View : Program.Visibility.View;
      Setter    : not null
        Program.Cross_Reference_Updaters.Cross_Reference_Updater_Access;
      Factory   : Program.Implicit_Element_Factories.Element_Factory;
      Vectors   : Program.Element_Vector_Factories.Element_Vector_Factory;
      Result    : out Program.Element_Vectors.Element_Vector_Access);

end Program.Predefined_Operators;
