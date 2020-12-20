--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Ada.Wide_Wide_Text_IO;
with Ada.Command_Line;

package body Errors is

   -------------------------
   -- Circular_Dependency --
   -------------------------

   overriding procedure Circular_Dependency
     (Self : access Error_Listener;
      Name : Program.Text)
   is
      pragma Unreferenced (Self);
   begin
      Ada.Wide_Wide_Text_IO.Put_Line
        (Ada.Wide_Wide_Text_IO.Standard_Error,
         "Circular dependency for unit: " & Name);

      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end Circular_Dependency;

   ------------------
   -- No_Body_Text --
   ------------------

   overriding procedure No_Body_Text
     (Self : access Error_Listener;
      Name : Program.Text)
   is
      pragma Unreferenced (Self);
   begin
      Ada.Wide_Wide_Text_IO.Put_Line
        (Ada.Wide_Wide_Text_IO.Standard_Error,
         "No text for unit/body: " & Name);

      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end No_Body_Text;

end Errors;
