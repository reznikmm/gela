--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

package Program.Error_Listeners is
   pragma Pure;

   type Error_Listener is limited interface;
   --  A program error listener

   type Error_Listener_Access is
     access all Error_Listener'Class with Storage_Size => 0;

   not overriding procedure No_Body_Text
     (Self : access Error_Listener;
      Name : Program.Text) is null;
   --  Missing semantic dependency

   not overriding procedure Circular_Dependency
     (Self : access Error_Listener;
      Name : Program.Text) is null;
   --  A circular semantic dependency found during a unit analysis

end Program.Error_Listeners;
