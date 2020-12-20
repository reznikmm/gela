--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Program.Error_Listeners;

package Errors is

   type Error_Listener is new Program.Error_Listeners.Error_Listener
     with null record;

   overriding procedure No_Body_Text
     (Self : access Error_Listener;
      Name : Program.Text);

   overriding procedure Circular_Dependency
     (Self : access Error_Listener;
      Name : Program.Text);

end Errors;
